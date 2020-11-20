open Base
open Neo_infix

module U = Yojson.Safe.Util

let yo_assoc l   : Yojson.Safe.t = `Assoc l
let yo_string s  : Yojson.Safe.t = `String s
let yo_float f   : Yojson.Safe.t = `Float f
let yo_int i     : Yojson.Safe.t = `Int i
let yo_bool b    : Yojson.Safe.t = `Bool b
let yo_list yo l : Yojson.Safe.t = `List (List.map ~f:yo l)

let yojson_of_string s =
  try Yojson.Safe.from_string s |> Result.return
  with Yojson.Json_error e -> Result.fail (`InvalidJson e)

let json_of_option con opt : Yojson.Safe.t =
  Option.value_map ~f:con ~default:`Null opt

let assoc_of_yojson j =
  try U.to_assoc j |> Result.return
  with _ -> Result.fail "Input yojson is not an `Assoc."

let string_of_yojson j =
  try U.to_string j |> Result.return
  with _ -> Result.fail "Input yojson is not a `String."

let float_of_yojson j =
  try U.to_float j |> Result.return
  with _ -> Result.fail "Input yojson is not a `Float."

let int_of_yojson j =
  try U.to_int j |> Result.return
  with _ -> Result.fail "Input yojson is not an `Int."

let bool_of_yojson j =
  try U.to_bool j |> Result.return
  with _ -> Result.fail "Input yojson is not a `Bool."

let list_of_yojson j =
  try U.to_list j |> Result.return
  with _ -> Result.fail "Input yojson is not a `List."

let typed_list_of_yojson of_yojson j =
  list_of_yojson j |> Result.bind ~f:(List.map ~f:of_yojson >> Result.all)

let alist_of_yojson of_yojson j =
  let open Result in
  let pair_of_yojson (k, yo_v) = of_yojson yo_v >>| fun v -> k, v in
  assoc_of_yojson j >>= (List.map ~f:pair_of_yojson >> Result.all)

module StringMap = struct
  type 'a t = (string, 'a, String.comparator_witness) Map.t

  let empty = Map.empty (module String)

  let of_yojson a_of_yojson j =
    let open Result in
    alist_of_yojson a_of_yojson j >>= fun l ->
    try Map.of_alist_exn (module String) l |> Result.return
    with _ -> Result.fail "Invalid 'a string_map."

  let to_yojson a_to_yojson a =
    let yo_pair (k, v) = k, a_to_yojson v in
    Map.to_alist a |> List.map ~f:yo_pair |> yo_assoc
end

module type DerivingYojson = sig
  type t
  val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
  val to_yojson : t -> Yojson.Safe.t
end

module type DerivingOfYojson = sig
  type t
  val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
end
