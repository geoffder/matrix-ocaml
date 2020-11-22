open Core
open Result.Monad_infix

let header   = "-----BEGIN MEGOLM SESSION DATA-----\n"
let footer   = "\n-----END MEGOLM SESSION DATA-----"
let dk_len   = Int32.of_int_exn 64
let hmac_len = 32

let sha512_pbkdf2 ~password ~salt ~count =
  Pbkdf.pbkdf2 ~prf:`SHA512 ~password ~salt ~count ~dk_len
  |> Fn.flip Cstruct.split @@ 32

let encrypt ?(count=100000) pass data =
  let password          = Cstruct.of_string pass in
  let salt              = Nocrypto.Rng.generate 16 in
  let aes_key, hmac_key = sha512_pbkdf2 ~password ~salt ~count in
  let ctr = Nocrypto.Rng.generate 16 in
  (* Set bit 63 to 0. *)
  Cstruct.set_uint8 ctr 8 ((Cstruct.get_uint8 ctr 8) land (lnot (1 lsl 7)));
  let count_cs  = Neolm_utils.int_to_bigend_cstruct ~len:4 count in
  let encrypted =
    let open Nocrypto.Cipher_block.AES.CTR in
    encrypt ~key:(of_secret aes_key) ~ctr (Cstruct.of_string data)
  in
  let payload = Cstruct.concat
      [ Cstruct.of_string "\001"; salt; ctr; count_cs; encrypted ]
  in
  let hmac = Nocrypto.Hash.SHA256.hmac ~key:hmac_key payload in
  Cstruct.concat [ payload; hmac ]
  |> Cstruct.to_string
  |> Base64.encode ~pad:false
  |> Result.map_error ~f:(fun _ -> `InvalidBase64)

let decrypt pass payload =
  Base64.decode ~pad:false payload |> Result.map_error ~f:(fun _ -> `InvalidBase64) >>|
  Cstruct.of_string >>= fun decoded ->
  Result.ok_if_true ((Cstruct.get_uint8 decoded 0) = 1)
    ~error:(`Protocol "Unsupported export format version") >>= fun () ->
  let password = Cstruct.of_string pass in
  let salt     = Cstruct.sub decoded 1 16 in
  let ctr      = Cstruct.sub decoded 17 16 in
  let count    = Cstruct.sub decoded 33 4
                 |> Cstruct.to_bytes
                 |> Neolm_utils.bigend_bytes_to_int
  in
  let hmac_offset       = Cstruct.len decoded - hmac_len in
  let encrypted         = Cstruct.sub decoded 37 (hmac_offset - 37) in
  let expected_hmac     = Cstruct.sub decoded hmac_offset hmac_len in
  let aes_key, hmac_key = sha512_pbkdf2 ~password ~salt ~count in
  let hmac = Cstruct.sub decoded 0 (Cstruct.len decoded - hmac_len)
             |> Nocrypto.Hash.SHA256.hmac ~key:hmac_key
  in
  Result.ok_if_true (Cstruct.equal expected_hmac hmac)
    ~error:(`Protocol "HMAC check failed for encrypted payload.") >>| fun () ->
  let open Nocrypto.Cipher_block.AES in
  CTR.decrypt encrypted ~key:(CTR.of_secret aes_key) ~ctr
  |> Cstruct.to_string

let encrypt_and_save ?count outfile pass data =
  encrypt ?count pass data >>| fun encrypted ->
  let oc = Out_channel.create outfile in
  fprintf oc "%s%s%s" header encrypted footer;
  Out_channel.close oc

let decrypt_and_read infile pass =
  let ic = In_channel.create infile in
  let contents = In_channel.input_all ic in
  In_channel.close ic;
  String.chop_prefix ~prefix:header contents
  |> Option.bind ~f:(String.chop_suffix ~suffix:footer)
  |> Result.of_option ~error:(`Protocol "Wrong file format.")
  >>= decrypt pass
