open! Core

let dk_len = Int32.of_int_exn 64

let sha512_pbkdf2 ~password ~salt ~count =
  Pbkdf.pbkdf2 ~prf:`SHA512 ~password ~salt ~count ~dk_len

let encrypt ?(count=100000) pass data =
  let password          = Cstruct.of_string pass in
  let salt              = Nocrypto.Rng.generate 16 in
  let aes_key, hmac_key = sha512_pbkdf2 ~password ~salt ~count
                          |> Fn.flip Cstruct.split @@ 32
  in
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
  let hmac = Nocrypto.Hash.SHA512.hmac ~key:hmac_key payload in
  Cstruct.concat [ payload; hmac ]

let decrypt = ()

let encrypt_and_save = ()

let decrypt_and_read = ()
