open! Core

let util = Olm.Utility.create ()
let sha256 = Olm.Utility.sha256 util
let ed25519_verify = Olm.Utility.ed25519_verify util

let bin8_of_int_exn d =
  if d < 0 || d > 255
  then failwith "Must be unsigned 8bit int."
  else (
    let rec aux acc = function
      | 0 -> acc
      | d -> aux (string_of_int (d land 1) :: acc) (d lsr 1)
    in
    let bin = String.concat (aux [] d) in
    let pad = String.init ~f:(fun _ -> '0') (8 - String.length bin) in
    pad ^ bin )

let int_of_bin_exn s =
  let char_to_bit = function
    | '0' -> 0
    | '1' -> 1
    | _   -> failwith "Bitstring must only consist of 0s and 1s."
  in
  String.lstrip ~drop:(Char.equal '0') s
  |> String.rev
  |> String.foldi ~init:0 ~f:(fun i sum c -> sum + (char_to_bit c * Int.pow 2 i))

let int_to_bigend_cstruct ~len i =
  let cs = Cstruct.create len in
  let rec loop n rem =
    if n >= 0
    then (
      let b = Int.pow 256 n in
      Cstruct.set_uint8 cs (len - n - 1) (rem / b);
      loop (n - 1) (rem mod b) )
    else ()
  in
  loop (len - 1) i;
  cs

let bigend_bytes_to_int bytes =
  Bytes.to_list bytes
  |> List.rev
  |> List.foldi ~init:0 ~f:(fun i acc c -> acc + (Char.to_int c * Int.pow 256 i))

let map_fold_result ~init ~f m =
  with_return (fun { return } ->
      Result.return
      @@ Map.fold m ~init ~f:(fun ~key ~data acc ->
             match f acc ~key ~data with
             | Ok r         -> r
             | Error _ as e -> return e))
