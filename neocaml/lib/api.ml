open Core

open Types
open Yojson_helpers

(* Returned from every apicall building function. *)
type triple = Cohttp.Code.meth * string * Yojson.Basic.t option

let matrix_api_path   = "/_matrix/client/r0"
let matrix_media_path = "/_matrix/media/r0"

let query key v = [ (key, [ v ]) ]

let query_of_option key = Option.value_map ~f:(query key) ~default:[]

let query_of_option_map ~f key =
  Option.value_map ~f:(f |> Fn.compose (query key)) ~default:[]

let ( // ) a b = a ^ "/" ^ b

let build_path ?queries ?(api_path=matrix_api_path) path =
  let q = queries |> Option.value_map ~f:Uri.encoded_of_query ~default:"" in
  api_path // path ^ "?" ^ q

let mxc_to_http ?homeserver mxc =
  let open Option in
  let uri = Uri.of_string mxc in
  Uri.scheme uri                         >>= fun scheme ->
  some_if (String.equal scheme "mxc") () >>= fun () ->
  Uri.host uri                           >>= fun server_name ->
  let home =
    match homeserver with
    | None ->
      let port =
        Uri.port uri
        |> value_map ~default:"" ~f:(fun p -> ":" ^ (Int.to_string p)) in
      "https://" ^ server_name ^ port
    | Some url -> url
  in
  Some (home ^ "/_matrix/media/r0/download/" ^ server_name ^ (Uri.path uri))

let encrypted_mxc_to_plumb ?homeserver mxc key hash iv =
  let open Option in
  let uri = Uri.of_string mxc in
  Uri.scheme uri                         >>= fun scheme ->
  some_if (String.equal scheme "mxc") () >>= fun () ->
  Uri.host uri                           >>= fun server_name ->
  let home =
    match homeserver with
    | None ->
      let port =
        Uri.port uri
        |> value_map ~default:"" ~f:(fun p -> ":" ^ (Int.to_string p)) in
      "emxc://" ^ server_name ^ port
    | Some url -> Uri.with_scheme  (Uri.of_string url) (Some "emxc") |> Uri.to_string
  in
  [ ("key",  [ key ])
  ; ("hash", [ hash ])
  ; ("iv",   [ iv ])
  ] |> Uri.encoded_of_query
  |> sprintf "%s/_matrix/media/r0/download/%s%s?%s" home server_name (Uri.path uri)
  |> some

(* Api call funcs ->
 *  Cohttp.Code.meth * string * Yojson.Safe.t option *)

let login ?device_name ?device_id user cred =
  let credential =
    let open Credential in
    match cred with
    | Password str  -> [ ("type",     yo_string "m.login.password")
                       ; ("user",     yo_string user)
                       ; ("password", yo_string str)
                       ]
    | AuthToken str -> [ ("type",  yo_string "m.login.token")
                       ; ("token", yo_string str)
                       ] in
  let content =
    credential
    @ [ ("device_id", json_of_option yo_string device_id)
      ; ("device_name", json_of_option yo_string device_name)
      ] |> yo_assoc in
  (`POST, build_path "login", Some content)

let logout ?(all_devices=false) access =
  begin if all_devices then "logout/all" else "logout" end
  |> build_path ~queries:(query "access_token" access)
  |> fun pth -> (`POST, pth, Some (yo_assoc []))

let sync ?since ?timeout ?filter ?(full_state=false) ?set_presence access =
  let queries =
    query_of_option "since" since
    @ query_of_option_map ~f:Int.to_string "timeout" timeout
    @ query_of_option_map ~f:Presence.to_string "set_presence" set_presence
    @ query_of_option_map ~f:Yojson.Safe.to_string "filter" filter
    @ [ ("access_token", [ access ])
      ; ("full_state",   [ Bool.to_string full_state ])
      ] in
  (`GET, build_path ~queries "sync", None)

let room_send access room_id event_type body tx_id =
  let queries = query "access_token" access in
  let pth =  "rooms" // room_id // "send" // event_type // tx_id in
  (`PUT, build_path ~queries pth, Some body)

let room_get_event access room_id event_id =
  let queries = query "access_token" access in
  let pth = "rooms" // room_id // "event" // event_id in
  (`GET, build_path ~queries pth, None)

let room_put_state ?state_key access room_id event_type body =
  let queries = query "access_token" access in
  let key = Option.value ~default:"" state_key in
  let pth = "rooms" // room_id // "state" // event_type // key in
  (`PUT, build_path ~queries pth, Some body)

let room_get_state_event ?state_key access room_id event_type =
  let queries = query "access_token" access in
  let key = Option.value ~default:"" state_key in
  let pth = "rooms" // room_id // "state" // event_type // key in
  (`GET, build_path ~queries pth, None)

let room_get_state access room_id =
  let queries = query "access_token" access in
  let pth = "rooms" // room_id // "state" in
  (`GET, build_path ~queries pth, None)

let room_redact ?reason access room_id event_id tx_id =
  let queries = query "access_token" access in
  let content = [ ("reason", json_of_option yo_string reason) ] |> yo_assoc in
  let pth = "rooms" // room_id // "redact" // event_id // tx_id in
  (`PUT, build_path ~queries pth, Some content)

let room_kick ?reason access room_id user_id =
  let queries = query "access_token" access in
  let content = [ ("user_id", yo_string user_id)
                ; ("reason", json_of_option yo_string reason)
                ] |> yo_assoc in
  let pth = "rooms" // room_id // "kick" in
  (`POST, build_path ~queries pth, Some content)

let room_ban ?reason access room_id user_id =
  let queries = query "access_token" access in
  let content = [ ("user_id", yo_string user_id)
                ; ("reason", json_of_option yo_string reason)
                ] |> yo_assoc in
  let pth = "rooms" // room_id // "ban" in
  (`POST, build_path ~queries pth, Some content)

let room_unban access room_id user_id =
  let queries = query "access_token" access in
  let content = yo_assoc [ ("user_id", yo_string user_id) ] in
  let pth = "rooms" // room_id // "unban" in
  (`POST, build_path ~queries pth, Some content)

let room_invite access room_id user_id =
  let queries = query "access_token" access in
  let content = yo_assoc [ ("user_id", yo_string user_id) ] in
  let pth = "rooms" // room_id // "invite" in
  (`POST, build_path ~queries pth, Some content)

(* NOTE: Invite is just a list of user_id, but initial state and power args
 * are lists of json. Eventually these should probably be record types that
 * are converted into json (either here or in calling method.) *)
let room_create ?invite ?initial_state ?power_override access config =
  let queries = query "access_token" access in
  let power = Option.value ~default:`Null power_override in
  let content =
    [ ("invite", json_of_option (yo_list yo_string) invite)
    ; ("initial_state", json_of_option (yo_list Fn.id) initial_state)
    ; ("power_level_content_override", power)
    ] |> yo_assoc
    |> Yojson.Safe.Util.combine @@ Room.Config.to_yojson config in
  (`POST, build_path ~queries "createRoom", Some content)

let join access room_id =
  let queries = query "access_token" access in
  let pth = "join" // room_id in
  (`POST, build_path ~queries pth, Some (yo_assoc []))

let room_leave access room_id =
  let queries = query "access_token" access in
  let pth = "rooms" // room_id // "leave" in
  (`POST, build_path ~queries pth, Some (yo_assoc []))

let room_forget access room_id =
  let queries = query "access_token" access in
  let pth = "rooms" // room_id // "forget" in
  (`POST, build_path ~queries pth, Some (yo_assoc []))

let room_messages ?stop ?dir ?(limit=10) ?filter access room_id start =
  let queries =
    query_of_option "to" stop
    @ query_of_option_map ~f:Yojson.Safe.to_string "filter" filter
    @ query_of_option_map ~f:MessageDirection.to_string "dir" dir
    @ [ ("access_token", [ access ])
      ; ("from",         [ start ])
      ; ("limit",        [ Int.to_string limit ])
      ] in
  let pth = "rooms" // room_id // "messages" in
  (`GET, build_path ~queries pth, None)

(* NOTE: the body that is expected is a complex nested json. Will need to build
 * a type to hold all the values required and has a to_json. *)
let keys_upload = ()

let keys_query ?since_token access user_set =
  let queries = query "access_token" access in
  let users = Set.to_list user_set
              |> List.map ~f:(fun u -> u, `List [])
              |> yo_assoc in
  let content = [ ("device_keys", users)
                ; ("token", json_of_option yo_string since_token)
                ] |> yo_assoc in
  (`POST, build_path ~queries "keys/query", Some content)

let keys_claim access user_devices =
  let queries = query "access_token" access in
  (* Map devices list into an alist indicating desired encryption key type. *)
  let f (u, ds) =
    (u, List.map ~f:(fun d -> d, yo_string "signed_curve25519") ds |> yo_assoc) in
  let content = [ ("one_time_keys", user_devices |> List.map ~f |> yo_assoc) ]
                |> yo_assoc in
  (`POST, build_path ~queries "keys/claim", Some content)

(* TODO: content is of form { user_id: { device_id: msg_body } }, where device_id
 * can be "\*" to indicate all known devices for the user. (from nio docstring).
 * Need to decide on json conversion in here or in the client function. *)
let to_device access event_str content tx_id =
  let queries = query "access_token" access in
  let pth = Printf.sprintf "sendToDevice/%s/%s" event_str tx_id in
  (`PUT, build_path ~queries pth, Some content)

let devices access =
  let queries = query "access_token" access in
  (`GET, build_path ~queries "devices", None)

let update_device access device_id content =
  let queries = query "access_token" access in
  (`PUT, build_path ~queries ("devices" // device_id), Some content)

(* TODO: Don't know what auth_dict (auth_json) is yet, but the nio docstring
 * says that this should first be called without it... *)
let delete_devices ?auth access devices =
  let queries = query "access_token" access in
  let content = [ ("devices", yo_list yo_string devices)
                ; ("auth", json_of_option Fn.id auth)
                ] |> yo_assoc in
  (`POST, build_path ~queries "delete_devices", Some content)

let joined_members access room_id =
  let queries = query "access_token" access in
  let pth = "rooms" // room_id // "joined_members" in
  (`GET, build_path ~queries pth, None)

let joined_rooms access =
  let queries = query "access_token" access in
  (`GET, build_path ~queries "joined_rooms", None)

let room_resolve_alias room_alias =
  (`GET, build_path ("directory/room" // room_alias), None)

let room_typing ?(typing=true) ?timeout access room_id user_id =
  let queries = query "access_token" access in
  let content = [ ("typing",  Bool.to_string typing |> yo_string)
                ; ("timeout", Option.value ~default:30000 timeout |> yo_int)
                ] |> yo_assoc in
  let pth = "rooms" // room_id // "typing" // user_id in
  (`PUT, build_path ~queries pth, Some content)

let update_receipt_marker ?receipt_type access room_id event_id =
  let queries = query "access_token" access in
  (* Currently "m.read" is the only supported type. *)
  let rec_type = Option.value ~default:"m.read" receipt_type in
  let pth = "rooms" // room_id // "receipt" // rec_type // event_id in
  (`POST, build_path ~queries pth, None)

let room_read_markers ?read_event_id access room_id fully_read_event_id =
  let queries = query "access_token" access in
  let content = [ ("m.fully_read", yo_string fully_read_event_id)
                ; ("m.read", json_of_option yo_string read_event_id)
                ] |> yo_assoc in
  let pth = "rooms" // room_id // "read_markers" in
  (`POST, build_path ~queries pth, Some content)

let content_repository_config access =
  let queries = query "access_token" access in
  (`GET, build_path ~queries ~api_path:matrix_media_path "config", None)

let upload ?filename access =
  let queries =
    query_of_option "filename" filename
    @ query "access_token" access in
  (`POST, build_path ~queries ~api_path:matrix_media_path "upload", None)

let download ?filename ?(allow_remote=true) server_name media_id =
  let queries = query "allow_remote" (Bool.to_string allow_remote) in
  let pth = Option.value_map ~f:(( ^ ) "/") ~default:"" filename
            |> Printf.sprintf "download/%s/%s%s" server_name media_id in
  (`GET, build_path ~queries ~api_path:matrix_media_path pth, None)

let thumbnail ?(allow_remote=true) server_name media_id width height resize =
  let queries =
    query "width" (Int.to_string width)
    @ query "height" (Int.to_string height)
    @ query "method" (Resize.to_string resize)
    @ query "allow_remote" (Bool.to_string allow_remote) in
  let pth = "thumbnail" // server_name // media_id in
  (`GET, build_path ~queries ~api_path:matrix_media_path pth, None)

let profile_get user_id = (`GET, build_path ("profile" // user_id), None)

let profile_get_displayname user_id =
  let pth = "profile" // user_id // "displayname" in
  (`GET, build_path pth, None)

let profile_set_displayname access user_id display_name =
  let queries = query "access_token" access in
  let content = yo_assoc [ ("displayname", yo_string display_name) ] in
  let pth = "profile" // user_id // "displayname" in
  (`PUT, build_path ~queries pth, Some content)

let profile_get_avatar user_id =
  let pth = "profile" // user_id // "avatar_url" in
  (`GET, build_path pth, None)

let profile_set_avatar access user_id avatar_url =
  let queries = query "access_token" access in
  (* NOTE: Note sure if appropriate to Uri encode here... *)
  let content =
    yo_assoc [ ("avatar_url", Uri.pct_encode avatar_url |> yo_string) ] in
  let pth = "profile" // user_id // "avatar_url" in
  (`PUT, build_path ~queries pth, Some content)

let get_presence access user_id =
  let queries = query "access_token" access in
  let pth = "presence" // user_id // "status" in
  (`GET, build_path ~queries pth, None)

let set_presence ?status_msg access user_id presence =
  let queries = query "access_token" access in
  let content =
    [ ("presence"  , Presence.to_string presence |> yo_string)
    ; ("status_msg", json_of_option yo_string status_msg)
    ] |> yo_assoc in
  let pth = "presence" // user_id // "status" in
  (`PUT, build_path ~queries pth, Some content)

let whoami access =
  let queries = query "access_token" access in
  (`GET, build_path ~queries "account/whoami", None)

let room_context ?limit access room_id event_id =
  let queries =
    query_of_option_map ~f:Int.to_string "limit" limit
    @ query "access_token" access in
  let pth = "rooms" // room_id // "context" // event_id in
  (`GET, build_path ~queries pth, None)

let upload_filter = ()
