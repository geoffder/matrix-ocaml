open Base

let matrix_api_path   = "/_matrix/client/r0"
let matrix_media_path = "/_matrix/media/r0"

let build_path ?queries ?api_path:(api=matrix_api_path) path =
  let q = queries |> Option.value_map ~f:Uri.encoded_of_query ~default:"" in
  api ^ "/" ^ Uri.pct_encode path ^ "?" ^ q
  |> Uri.of_string

(* Build a post request to server for login🥵.
 * See: https://github.com/poljar/matrix-nio/blob/master/nio/api.py
 * line 305, def login. Need to decide whether I really want to break this
 * in to Api and Client modules where so much is shared, or jsut have a client
 * module. For example, login in the nio api.py and login in async_client.py
 * https://github.com/poljar/matrix-nio/blob/master/nio/client/async_client.py
 * are almost the same. The client simply sends it and basically passes along
 * all of it's arguments. Should I follow the same pattern, or not? *)
let login ?device_name ?device_id user cred =
  let pth = build_path "login" in
  ()
