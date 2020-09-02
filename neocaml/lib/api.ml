open Base

open Types
open Yojson_helpers

let matrix_api_path   = "/_matrix/client/r0"
let matrix_media_path = "/_matrix/media/r0"

let query key v = [ (key, [ v ]) ]

let query_of_option key = Option.value_map ~f:(query key) ~default:[]

let query_of_option_map ~f key =
  Option.value_map ~f:(f |> Fn.compose (query key)) ~default:[]

let build_path ?queries ?api_path:(api=matrix_api_path) path =
  let q = queries |> Option.value_map ~f:Uri.encoded_of_query ~default:"" in
  (* NOTE: Removed pct_encode, was breaking paths that included '/'
   *  (and maybe '!') *)
  (* api ^ "/" ^ Uri.pct_encode path ^ "?" ^ q *)
  api ^ "/" ^ path ^ "?" ^ q

(* Api call funcs ->
 *  Cohttp.Code.meth * string * Yojson.Basic.t option *)

let login ?device_name ?device_id user cred =
  let credential =
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

let logout ?all_devices:(all=false) access =
  begin if all then "logout/all" else "logout" end
  |> build_path ~queries:(query "access_token" access)
  |> fun pth -> (`POST, pth, Some (yo_assoc []))

let sync ?since ?timeout ?filter ?full_state:(full=false) ?set_presence access =
  let queries =
    query_of_option "since" since
    @ query_of_option_map ~f:Int.to_string "timeout" timeout
    @ query_of_option_map ~f:Presence.to_string "set_presence" set_presence
    @ query_of_option_map ~f:Yojson.Basic.to_string "filter" filter
    @ [ ("access_token", [ access ])
      ; ("full_state",   [ Bool.to_string full ])
      ] in
  (`GET, build_path ~queries "sync", None)

let room_send access room_id event_type body tx_id =
  let queries = query "access_token" access in
  (* TODO: Need to check out how events are broken up in nio. In matrix they
   * are of the form "m.room.message". I need to model them with types and
   * create the relevant conversion funtions. Also, for each of these events,
   * should I make records that derive from yojson (for automatic conversion
   * methods) ? Then I could just pass a room event into this function, which
   * will give a constructor to generate the matrix string with and also be easy
   * to convert to a json body. *)
  (* let event_str = Events.to_string event_type in *)
  let event_str = event_type in
  let pth = Printf.sprintf "rooms/%s/send/%s/%s" room_id event_str tx_id in
  (`PUT, build_path ~queries pth, body)

let room_get_event access room_id event_id =
  let queries = query "access_token" access in
  let pth = Printf.sprintf "rooms/%s/event/%s" room_id event_id in
  (`GET, build_path ~queries pth, None)

(* TODO: Decide between passing in an already json'd body, or converting it in
 * here (and getting the event_type string at the same time.) This applies to
 * so other Api calls in here as well. *)
let room_put_state ?state_key access room_id event_type body =
  let queries = query "access_token" access in
  let event_str = event_type in  (* FIXME: reminder that this shouldn't be str *)
  let key = Option.value ~default:"" state_key in
  let pth = Printf.sprintf "rooms/%s/state/%s/%s" room_id event_str key in
  (`PUT, build_path ~queries pth, Some body)

let room_get_state_event ?state_key access room_id event_type =
  let queries = query "access_token" access in
  let event_str = event_type in  (* FIXME: reminder that this shouldn't be str *)
  let key = Option.value ~default:"" state_key in
  let pth = Printf.sprintf "rooms/%s/state/%s/%s" room_id event_str key in
  (`GET, build_path ~queries pth, None)

let room_get_state access room_id =
  let queries = query "access_token" access in
  let pth = "rooms/" ^ room_id ^ "/state" in
  (`GET, build_path ~queries pth, None)

let room_redact ?reason access room_id event_id tx_id =
  let queries = query "access_token" access in
  let content = [ ("reason", json_of_option yo_string reason) ] |> yo_assoc in
  let pth = Printf.sprintf "rooms/%s/redact/%s/%s" room_id event_id tx_id in
  (`PUT, build_path ~queries pth, Some content)

let room_kick ?reason access room_id user_id =
  let queries = query "access_token" access in
  let content = [ ("user_id", yo_string user_id)
                ; ("reason", json_of_option yo_string reason)
                ] |> yo_assoc in
  let pth = "rooms/" ^ room_id ^ "/kick" in
  (`POST, build_path ~queries pth, Some content)

let room_ban ?reason access room_id user_id =
  let queries = query "access_token" access in
  let content = [ ("user_id", yo_string user_id)
                ; ("reason", json_of_option yo_string reason)
                ] |> yo_assoc in
  let pth = "rooms/" ^ room_id ^ "/ban" in
  (`POST, build_path ~queries pth, Some content)

let room_unban access room_id user_id =
  let queries = query "access_token" access in
  let content = yo_assoc [ ("user_id", yo_string user_id) ] in
  let pth = "rooms/" ^ room_id ^ "/unban" in
  (`POST, build_path ~queries pth, Some content)

let room_invite access room_id user_id =
  let queries = query "access_token" access in
  let content = yo_assoc [ ("user_id", yo_string user_id) ] in
  let pth = "rooms/" ^ room_id ^ "/invite" in
  (`POST, build_path ~queries pth, Some content)

let room_create = ()

let join access room_id =
  let queries = query "access_token" access in
  let pth = "join/" ^ room_id in
  (`POST, build_path ~queries pth, Some (yo_assoc []))

let room_leave access room_id =
  let queries = query "access_token" access in
  let pth = "rooms/" ^ room_id ^ "/leave" in
  (`POST, build_path ~queries pth, Some (yo_assoc []))

let room_forget access room_id =
  let queries = query "access_token" access in
  let pth = "rooms/" ^ room_id ^ "/forget" in
  (`POST, build_path ~queries pth, Some (yo_assoc []))

let room_messages ?stop ?dir ?lim:(lim=10) ?filter access room_id start =
  let queries =
    query_of_option "to" stop
    @ query_of_option_map ~f:Yojson.Basic.to_string "filter" filter
    @ query_of_option_map ~f:MessageDirection.to_string "dir" dir
    @ [ ("access_token", [ access ])
      ; ("from",         [ start ])
      ; ("limit",        [ Int.to_string lim ])
      ] in
  let pth = "rooms/" ^ room_id ^ "/messages" in
  (`GET, build_path ~queries pth, None)

(* NOTE: the body that is expected is a complex nested json. Will need to build
 * a type to hold all the values required and has a to_json. *)
let keys_upload = ()

let keys_query ?since_token access user_set =
  let queries = query "access_token" access in
  let users = Set.to_list user_set
              |> List.map ~f:(fun u -> u, yo_list [])
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

let to_device = ()

let devices access =
  let queries = query "access_token" access in
  (`GET, build_path ~queries "devices", None)

let update_device = ()

let delete_devices = ()

let joined_members access room_id =
  let queries = query "access_token" access in
  let pth = "rooms/" ^ room_id ^ "/joined_members" in
  (`GET, build_path ~queries pth, None)

let joined_rooms access =
  let queries = query "access_token" access in
  (`GET, build_path ~queries "joined_rooms", None)

let room_resolve_alias room_alias =
  (`GET, build_path ("directory/room/" ^ room_alias), None)

let room_typing ?typing:(typing=true) ?timeout access room_id user_id =
  let queries = query "access_token" access in
  let content = [ ("typing",  Bool.to_string typing |> yo_string)
                ; ("timeout", Option.value ~default:30000 timeout |> yo_int)
                ] |> yo_assoc in
  let pth = Printf.sprintf "rooms/%s/typing/%s" room_id user_id in
  (`PUT, build_path ~queries pth, Some content)

let update_receipt_marker = ()

let room_read_markers = ()

let content_repository_config = ()

let upload ?filename access =
  let queries =
    query_of_option "filename" filename
    @ query "access_token" access in
  (`POST, build_path ~queries ~api_path:matrix_media_path "upload", None)

let download = ()

let thumbnail = ()

let profile_get user_id = (`GET, build_path ("profile/" ^ user_id), None)

let profile_get_displayname user_id =
  let pth = "profile/" ^ user_id ^ "/displayname" in
  (`GET, build_path pth, None)

let profile_set_displayname = ()

let profile_get_avatar user_id =
  let pth = "profile/" ^ user_id ^ "/avatar_url" in
  (`GET, build_path pth, None)

let profile_set_avatar = ()

let get_presence access user_id =
  let queries = query "access_token" access in
  let pth = "presence/" ^ user_id ^ "/status" in
  (`GET, build_path ~queries pth, None)

let set_presence ?status_msg access user_id presence =
  let queries = query "access_token" access in
  let content =
    [ ("presence",   Presence.to_string presence |> yo_string)
    ; ("status_msg", json_of_option yo_string status_msg)
    ] |> yo_assoc in
  let pth = "presence/" ^ user_id ^ "/status" in
  (`PUT, build_path ~queries pth, Some content)

let whoami access =
  let queries = query "access_token" access in
  (`GET, build_path ~queries "account/whoami", None)

let room_context ?limit access room_id event_id =
  let queries =
    query_of_option_map ~f:Int.to_string "limit" limit
    @ query "access_token" access in
  let pth = Printf.sprintf "rooms/%s/context/%s" room_id event_id in
  (`GET, build_path ~queries pth, None)

let upload_filter = ()
