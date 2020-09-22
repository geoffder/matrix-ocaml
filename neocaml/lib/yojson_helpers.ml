open Base

module U = Yojson.Safe.Util

let yo_assoc l  : Yojson.Safe.t = `Assoc l
let yo_string s : Yojson.Safe.t = `String s
let yo_float f  : Yojson.Safe.t = `Float f
let yo_int i    : Yojson.Safe.t = `Int i
let yo_bool b   : Yojson.Safe.t = `Bool b
let yo_list l   : Yojson.Safe.t = `List l

let json_of_option con opt : Yojson.Safe.t =
  Option.value_map ~f:con ~default:`Null opt

(* TODO: Change this to use Result return safe_to functions instead of exn
 * throwing ones (make my own set of basics. This will allow me to use Result
 * monad versions from ppx_deriving_yojson rather than having to generate exn
 * ones as well) *)
let alist_of_yojson safe_to j =
  try U.to_assoc j |> List.map ~f:(fun (k, v) -> k, safe_to v) |> Result.return
  with _ -> Result.fail "Invalid alist."
