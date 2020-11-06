open Core

open Lwt
open Cohttp
open Cohttp_lwt_unix

(* open Types *)
(* open Yojson_helpers *)
open Neo_infix

(* NOTE: Should probably not have random state in the client, but this will
 * do for now until I think about it. *)
type t = { homeserver      : string
         ; user            : string
         ; user_id         : string option
         ; device_id       : string option
         ; store_path      : string option
         ; access_token    : string option
         ; rooms           : (string, Room.t, String.comparator_witness) Map.t
         ; encrypted_rooms : (string, String.comparator_witness) Set.t
         ; random_state    : Random.State.t
         }

let create ?device_id ?store_path ?access_token homeserver user =
  { homeserver
  ; user
  ; user_id         = None
  ; device_id
  ; store_path
  ; access_token
  ; rooms           = Map.empty (module String)
  ; encrypted_rooms = Set.empty (module String)
  ; random_state    = Random.State.make_self_init ~allow_in_tests:true ()
  }

let complete_uri t pth = t.homeserver ^ pth |> Uri.of_string

let response_code = Response.status >> Code.code_of_status

let body_of_json j = j |> Yojson.Safe.to_string |> Cohttp_lwt.Body.of_string

let json_of_body b = b |> Cohttp_lwt.Body.to_string >|= Yojson.Safe.from_string

let read_chunk ?(sz=1024) (monitor : Monitor.t) fd () =
  let buffer = Bytes.create sz in
  Lwt_unix.read fd buffer 0 sz >|= function
  | a when a = sz -> monitor.step a; Some (Bytes.to_string buffer)
  | 0 -> monitor.finish (); None
  | a -> monitor.step a; Some (Bytes.sub ~pos:0 ~len:a buffer |> Bytes.to_string)

let create_data_provider ?(monitor=Monitor.def) pth () =
  let file_size = (Unix.stat pth).st_size
                  |> Int64.to_int
                  |> Option.value ~default:Int.max_value in
  monitor.init file_size;
  Lwt_unix.(openfile pth [ O_RDONLY ] 0) >|= fun fd ->
  Lwt_stream.from (read_chunk monitor fd)
  |> Cohttp_lwt.Body.of_stream
  |> Option.some

(* Continue execution of given function if logged in. *)
let logged_in t =
  t.access_token
  |> Option.value_map ~f:Lwt.return_ok
    ~default:(Lwt.return_error `NotLoggedIn)

let with_timeout ?timeout ~call uri =
  let times_up =
    timeout
    |> Option.value_map
      ~default:Float.max_finite_value
      ~f:(fun i -> i / 1000 |> Int.to_float)
    |> Lwt_unix.sleep
    >>= fun () -> Lwt.return_error `TimedOut in
  let called = call uri >>= Lwt.return_ok in
  Lwt.pick [ times_up; called ]

let repeat ?timeout ?(max_429s=100) ?(max_outs=100) ~call ~get_data meth uri =
  let rec aux n_429s n_outs =
    get_data () >>= fun body ->
    let loaded = call ?body meth in
    with_timeout ?timeout ~call:loaded uri
    >>= function
    | Ok (resp, _body as response) ->
      if phys_equal resp.Response.status `Too_many_requests
      then
        if n_429s < max_429s
        then Lwt_unix.sleep 5. >>= fun () -> aux (n_429s + 1) n_outs
        else Lwt.return_error `Max429s
      else Lwt.return_ok response
    | Error `TimedOut ->
      if n_outs < max_outs
      then Lwt_unix.sleep 5. >>= fun () -> aux n_429s (n_outs + 1)
      else Lwt.return_error `MaxTimeouts
  in
  aux 0 0

(* ctx would come from client if actually needed? See where it is used in nio *)
let send
    ?ctx
    ?content_type
    ?content_len
    ?timeout
    ?data_provider
    t
    (meth, pth, content)
  =
  let headers =
    ("Content-Type", Option.value ~default:"application/json" content_type)
    :: Option.value_map
      ~f:(fun i -> [ ("Content-Length", Int.to_string i) ])
      ~default:[]
      content_len
    |> Header.of_list in
  let uri = complete_uri t pth in
  let body = Option.map ~f:body_of_json content in
  let get_data = Option.value ~default:(fun () -> Lwt.return body) data_provider in
  let call = Client.call ?ctx ?chunked:None ~headers in
  repeat ?timeout ~call ~get_data meth uri
  >>=? fun (_resp, body) ->
  Cohttp_lwt.Body.to_string body
  >>= ( Yojson.Safe.from_string >> Lwt.return_ok )

let login ?device_name cred t =
  Api.login ?device_name ?device_id:t.device_id t.user cred
  |> send t
  >>|? fun j ->
  let open Yojson.Safe.Util in
  { t with
    user_id      = j |> member "user_id" |> to_string_option
  ; device_id    = j |> member "device_id" |> to_string_option
  ; access_token = j |> member "access_token" |> to_string_option
  }

let logout ?(all_devices=false) t =
  logged_in t >>=? fun token ->
  Api.logout ~all_devices token
  |> send t
  >>|? fun _ -> { t with access_token = None }

let sync ?since ?timeout ?filter ?(full_state=false) ?set_presence t =
  logged_in t >>=? fun token ->
  Api.sync ?since ?timeout ?filter ~full_state ?set_presence token
  |> send t
  >>|=? Responses.(of_yojson Sync.of_yojson)

let keys_query users t =
  logged_in t >>=? fun token ->
  if List.length users > 0 then
    Api.keys_query token (Set.of_list (module String) users)
    |> send t
    >>|=? Responses.(of_yojson KeysQuery.of_yojson)
  else Lwt_result.fail `NoKeyQueryRequired

let devices t =
  logged_in t >>=? fun token ->
  Api.devices token
  |> send t
  >>|=? Responses.(of_yojson Devices.of_yojson)

let update_device device_id display_name t =
  logged_in t >>=? fun token ->
  `Assoc [ ("display_name", `String display_name) ]
  |> Api.update_device token device_id
  |> send t
  >>|=? Responses.(of_yojson UpdateDevice.of_yojson)

let delete_devices ?cred devices t =
  logged_in t >>=? fun token ->
  let auth = Types.Credential.(Option.(cred >>| function
    | Password s  -> `Assoc [ ("type",     `String "m.login.password")
                            ; ("user",     `String t.user)
                            ; ("password", `String s)
                            ]
    | AuthToken s -> `Assoc [ ("type",  `String "m.login.token")
                            ; ("token", `String s)
                            ]
    ))
  in
  Api.delete_devices ?auth token devices
  |> send t
  >>|=? Responses.(of_yojson DeleteDevices.of_yojson)

let joined_members room_id t =
  logged_in t >>=? fun token ->
  Api.joined_members token room_id
  |> send t
  >>|=? Responses.(of_yojson JoinedMembers.of_yojson)

let joined_rooms t =
  logged_in t >>=? fun token ->
  Api.joined_rooms token
  |> send t
  >>|=? Responses.(of_yojson JoinedRooms.of_yojson)

(* TODO: Handling encryption. *)
let room_send ?tx_id id event t =
  logged_in t >>=? fun token ->
  let body = Events.Room.Content.to_yojson event in
  let m_type = Events.Room.Content.to_m_type event in
  tx_id
  |> Option.value ~default:(Uuid.create_random t.random_state |> Uuid.to_string)
  |> Api.room_send token id m_type body
  |> send t
  >>|=? Responses.(of_yojson EventID.of_yojson)

let room_get_event room_id event_id t =
  logged_in t >>=? fun token ->
  Api.room_get_event token room_id event_id
  |> send t
  >>|=? Responses.of_yojson Events.Room.of_yojson

let room_put_state ?state_key room_id event t =
  logged_in t >>=? fun token ->
  let body   = Events.Room.Content.to_yojson event in
  let m_type = Events.Room.Content.to_m_type event in
  Api.room_put_state ?state_key token room_id m_type body
  |> send t
  >>|=? Responses.(of_yojson EventID.of_yojson)

let room_get_state room_id t =
  logged_in t >>=? fun token ->
  Api.room_get_state token room_id
  |> send t
  >>|=? Responses.(of_yojson RoomGetState.of_yojson)

let room_get_state_event room_id event_type state_key t =
  logged_in t >>=? fun token ->
  Api.room_get_state_event token room_id event_type state_key
  |> send t
  >>|=? Responses.of_yojson (Events.Room.Content.of_yojson event_type)

let room_redact ?reason ?tx_id room_id event_id t =
  logged_in t >>=? fun token ->
  tx_id
  |> Option.value ~default:(Uuid.create_random t.random_state |> Uuid.to_string)
  |> Api.room_redact ?reason token room_id event_id
  |> send t
  >>|=? Responses.(of_yojson EventID.of_yojson)

let room_resolve_alias room_alias t =
  Api.room_resolve_alias room_alias
  |> send t
  >>|=? Responses.(of_yojson RoomResolveAlias.of_yojson)

let room_create = ()

let join = ()

let room_invite = ()

let room_leave = ()

let room_forget = ()

let room_kick = ()

let room_ban = ()

let room_unban = ()

let room_context = ()

let room_messages ?stop ?dir ?(limit=10) ?filter id start t =
  logged_in t >>=? fun token ->
  Api.room_messages ?stop ?dir ~limit ?filter token id start
  |> send t
  >>|=? Responses.(of_yojson RoomMessages.of_yojson)

let room_typing = ()

let update_receipt_marker = ()

let room_read_markers = ()

let content_repository_config = ()

(* TODO: Add encrypt generation option... *)
let upload
    ?(content_type="application/octet-stream")
    ?filename
    ?content_len
    data_provider
    t
  =
  logged_in t >>=? fun token ->
  Api.upload ?filename token
  |> send ~content_type ~data_provider ?content_len t
  >>|=? Responses.(of_yojson Upload.of_yojson)

let send_image ?monitor pth room_id t =
  let provider = create_data_provider ?monitor pth in
  let filename = Filename.split pth |> snd in
  let content_type = Filename.split_extension filename
                     |> snd
                     |> Option.value ~default:"png"
                     |> ( ^ ) "image/" in
  upload ~content_type ~filename provider t >>=? fun { content_uri } ->
  let open Events.Room in
  let msg = Message.Image.create ~url:content_uri filename |> Message.image in
  room_send room_id (Content.Message msg) t

let room_upload ?monitor pth room_id t =
  let open File_helpers in
  let provider = create_data_provider ?monitor pth in
  let filename = Filename.split pth |> snd in
  let ext = Filename.split_extension filename |> snd in
  let content_type = Ext.to_content_type ext in
  upload ~content_type ~filename provider t >>=? fun { content_uri } ->
  let msg = (Ext.to_msg_create ext) ~url:content_uri filename in
  room_send room_id (Events.Room.Content.Message msg) t

let download = ()

let thumbnail = ()

let get_profile = ()

let get_presence = ()

let set_presence = ()

let get_displayname = ()

let set_displayname = ()

let get_avatar = ()

let set_avatar = ()

let upload_filter = ()
