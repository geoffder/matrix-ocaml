open Base
(* open Yojson *)

open Types
open Yojson_helpers

let matrix_api_path   = "/_matrix/client/r0"
let matrix_media_path = "/_matrix/media/r0"

let string_of_message_direction = function | Back -> "b" | Forward -> "f"

let query key v = [ (key, [ v ]) ]

let query_of_option key = function | None -> [] | Some v -> query key v

let build_path ?queries ?api_path:(api=matrix_api_path) path =
  let q = queries |> Option.value_map ~f:Uri.encoded_of_query ~default:"" in
  api ^ "/" ^ Uri.pct_encode path ^ "?" ^ q

(* Api call funcs ->
 *  Cohttp.Code.meth * string * Yojson.Basic.t option *)

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
  (`POST, build_path "login", Some content)

let logout ?all_devices:(all=false) access_token =
  begin if all then "logout/all" else "logout" end
  |> build_path ~queries:(query "access_token" access_token)
  |> fun pth -> (`POST, pth, Some (yo_assoc []))

let sync = ()

let room_send = ()

let room_get_event = ()

let room_put_state = ()

let room_get_state_event = ()

let room_get_state = ()

let room_redact = ()

let room_kick = ()

let room_ban = ()

let room_unban = ()

let room_invite = ()

let room_create = ()

let join = ()

let room_leave = ()

let room_forget = ()

let room_messages ?stop ?dir:(dir=Back) ?lim:(lim=10) ?filter access id start =
  let queries =
    query_of_option "to" stop @
    query_of_option "filter" filter @
    [ ("access_token", [ access ])
    ; ("from", [ start ])
    ; ("limit", [ Int.to_string lim ])
    ; ("dir", [ string_of_message_direction dir ])
    ] in
  let pth = "rooms/" ^ id ^ "/messages" in
  (`GET, build_path ~queries pth, None)

let keys_upload = ()

let keys_query = ()

let keys_claim = ()

let to_device = ()

let devices = ()

let update_device = ()

let delete_devices = ()

let joined_members = ()

let joined_rooms access_token =
  (`GET, build_path ~queries:(query "access_token" access_token), "joined_rooms")

let room_resolve_alias = ()

let room_typing = ()

let update_receipt_marker = ()

let room_read_markers = ()

let content_repository_config = ()

let upload = ()

let download = ()

let thumbnail = ()

let profile_get = ()

let profile_get_displayname = ()

let profile_set_displayname = ()

let profile_get_avatar = ()

let profile_set_avatar = ()

let get_presence = ()

let set_presence = ()

let whoami = ()

let room_context = ()

let upload_filter = ()
