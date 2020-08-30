open Base
(* open Yojson *)

open Types
open Yojson_helpers

let matrix_api_path   = "/_matrix/client/r0"
let matrix_media_path = "/_matrix/media/r0"

(* NOTE: Now returning String rather than Uri for easy prepend of server... *)
let build_path ?queries ?api_path:(api=matrix_api_path) path =
  let q = queries |> Option.value_map ~f:Uri.encoded_of_query ~default:"" in
  api ^ "/" ^ Uri.pct_encode path ^ "?" ^ q

let login ?device_name ?device_id user cred =
  let credential =
    match cred with
    | Password str  -> [ ("type", `String "m.login.password")
                       ; ("user", `String user)
                       ; ("password", `String str)
                       ]
    | AuthToken str -> [ ("type", `String "m.login.token")
                       ; ("token", `String str)
                       ] in
  let content =
    credential @
    [ ("device_id", json_of_option yo_string device_id)
    ; ("device_name", json_of_option yo_string device_name)
    ] |> yo_assoc in
  (* (build_path "login", content) *)
  (`POST, build_path "login", content)

let logout ?all_devices:(all=false) access_token =
  begin if all then "logout/all" else "logout" end
  |> build_path ~queries:[("access_token", [ access_token ])]
  |> fun pth -> (pth, yo_assoc [])
