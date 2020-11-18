open! Core

let util = Olm.Utility.create ()

let sha256 = Olm.Utility.sha256 util

let ed25519_verify = Olm.Utility.ed25519_verify util

let bin8_of_int_exn d =
  if d < 0 || d > 255
  then failwith "Must be unsigned 8bit int."
  else
    let rec aux acc = function
      | 0 -> acc
      | d -> aux (string_of_int (d land 1) :: acc) (d lsr 1)
    in
    let bin = String.concat (aux [] d) in
    let pad = String.init ~f:(fun _ -> '0') (8 - String.length bin) in
    pad ^ bin

let int_of_bin_exn s =
  let char_to_bit = function
    | '0' -> 0
    | '1' -> 1
    | _   -> failwith "Bitstring must only consist of 0s and 1s."
  in
  String.lstrip ~drop:(Char.equal '0') s
  |> String.rev
  |> String.foldi ~init:0
    ~f:(fun i sum c -> sum + (char_to_bit c * (Int.pow 2 i)))

let map_fold_result ~init ~f m =
  with_return begin fun { return } ->
    Result.return @@
    Map.fold m ~init ~f:begin fun ~key ~data acc ->
      match f acc ~key ~data with
      | Ok r         -> r
      | Error _ as e -> return e
    end
  end
