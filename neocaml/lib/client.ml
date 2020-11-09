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

let cohttp_response_to_yojson (_, body) =
  Cohttp_lwt.Body.to_string body >>= fun str ->
  Yojson_helpers.yojson_of_string str
  |> Lwt_result.lift

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

let logged_in t =
  t.access_token
  |> Option.value_map ~f:Lwt.return_ok
    ~default:(Lwt.return_error `NotLoggedIn)

let user_id t =
  t.user_id
  |> Option.value_map ~f:Lwt.return_ok
    ~default:(Lwt.return_error `NoUserID)

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
  let uri      = complete_uri t pth in
  let body     = Option.map ~f:body_of_json content in
  let get_data = Option.value ~default:(fun () -> Lwt.return body) data_provider in
  let call     = Client.call ?ctx ?chunked:None ~headers in
  repeat ?timeout ~call ~get_data meth uri

let discovery_info = ()

let login_info = ()

let register = ()

let login ?device_name cred t =
  Api.login ?device_name ?device_id:t.device_id t.user cred
  |> send t >>=?
  cohttp_response_to_yojson >>|? fun j ->
  let open Yojson.Safe.Util in
  { t with
    user_id      = j |> member "user_id" |> to_string_option
  ; device_id    = j |> member "device_id" |> to_string_option
  ; access_token = j |> member "access_token" |> to_string_option
  }

let logout ?(all_devices=false) t =
  logged_in t >>=? fun token ->
  Api.logout ~all_devices token
  |> send t >>=?
  cohttp_response_to_yojson >>|? fun _ ->
  { t with access_token = None }

let sync ?since ?timeout ?filter ?(full_state=false) ?set_presence t =
  logged_in t >>=? fun token ->
  Api.sync ?since ?timeout ?filter ~full_state ?set_presence token
  |> send t >>=?
  cohttp_response_to_yojson >>|=?
  Responses.(of_yojson Sync.of_yojson)

(* TODO: Handling encryption. *)
let room_send ?tx_id id event t =
  logged_in t >>=? fun token ->
  let body = Events.Room.Content.to_yojson event in
  let m_type = Events.Room.Content.to_m_type event in
  tx_id
  |> Option.value ~default:(Uuid.create_random t.random_state |> Uuid.to_string)
  |> Api.room_send token id m_type body
  |> send t >>=?
  cohttp_response_to_yojson >>|=?
  Responses.(of_yojson EventID.of_yojson)

let room_get_event room_id event_id t =
  logged_in t >>=? fun token ->
  Api.room_get_event token room_id event_id
  |> send t >>=?
  cohttp_response_to_yojson >>|=?
  Responses.of_yojson Events.Room.of_yojson

let room_put_state ?state_key room_id event t =
  logged_in t >>=? fun token ->
  let body   = Events.Room.Content.to_yojson event in
  let m_type = Events.Room.Content.to_m_type event in
  Api.room_put_state ?state_key token room_id m_type body
  |> send t >>=?
  cohttp_response_to_yojson >>|=?
  Responses.(of_yojson EventID.of_yojson)

let room_get_state room_id t =
  logged_in t >>=? fun token ->
  Api.room_get_state token room_id
  |> send t >>=?
  cohttp_response_to_yojson >>|=?
  Responses.(of_yojson RoomGetState.of_yojson)

let room_get_state_event room_id event_type state_key t =
  logged_in t >>=? fun token ->
  Api.room_get_state_event token room_id event_type state_key
  |> send t >>=?
  cohttp_response_to_yojson >>|=?
  Responses.of_yojson (Events.Room.Content.of_yojson event_type)

let room_redact ?reason ?tx_id room_id event_id t =
  logged_in t >>=? fun token ->
  tx_id
  |> Option.value ~default:(Uuid.create_random t.random_state |> Uuid.to_string)
  |> Api.room_redact ?reason token room_id event_id
  |> send t >>=?
  cohttp_response_to_yojson >>|=?
  Responses.(of_yojson EventID.of_yojson)

let room_resolve_alias room_alias t =
  Api.room_resolve_alias room_alias
  |> send t >>=?
  cohttp_response_to_yojson >>|=?
  Responses.(of_yojson RoomResolveAlias.of_yojson)

let room_create ?invite ?initial_state ?power_override config t =
  logged_in t >>=? fun token ->
  Api.room_create ?invite ?initial_state ?power_override token config
  |> send t >>=?
  cohttp_response_to_yojson >>|=?
  Responses.(of_yojson RoomCreate.of_yojson)

let join room_id t =
  logged_in t >>=? fun token ->
  Api.join token room_id
  |> send t >>=?
  cohttp_response_to_yojson >>|=?
  Responses.(of_yojson Join.of_yojson)

let room_invite room_id user_id t =
  logged_in t >>=? fun token ->
  Api.room_invite token room_id user_id
  |> send t >>=?
  cohttp_response_to_yojson >>|=?
  Responses.(of_yojson RoomInvite.of_yojson)

let room_leave room_id t =
  logged_in t >>=? fun token ->
  Api.room_leave token room_id
  |> send t >>=?
  cohttp_response_to_yojson >>|=?
  Responses.(of_yojson RoomLeave.of_yojson)

let room_forget room_id t =
  logged_in t >>=? fun token ->
  Api.room_forget token room_id
  |> send t >>=?
  cohttp_response_to_yojson >>|=?
  Responses.(of_yojson RoomForget.of_yojson)

let room_kick ?reason room_id user_id t =
  logged_in t >>=? fun token ->
  Api.room_kick ?reason token room_id user_id
  |> send t >>=?
  cohttp_response_to_yojson >>|=?
  Responses.(of_yojson RoomKick.of_yojson)

let room_ban ?reason room_id user_id t =
  logged_in t >>=? fun token ->
  Api.room_ban ?reason token room_id user_id
  |> send t >>=?
  cohttp_response_to_yojson >>|=?
  Responses.(of_yojson RoomBan.of_yojson)

let room_unban room_id user_id t =
  logged_in t >>=? fun token ->
  Api.room_unban token room_id user_id
  |> send t >>=?
  cohttp_response_to_yojson >>|=?
  Responses.(of_yojson RoomUnban.of_yojson)

let room_context ?limit room_id event_id t =
  logged_in t >>=? fun token ->
  Api.room_context ?limit token room_id event_id
  |> send t >>=?
  cohttp_response_to_yojson >>|=?
  Responses.(of_yojson RoomContext.of_yojson)

let room_messages ?stop ?dir ?(limit=10) ?filter id start t =
  logged_in t >>=? fun token ->
  Api.room_messages ?stop ?dir ~limit ?filter token id start
  |> send t >>=?
  cohttp_response_to_yojson >>|=?
  Responses.(of_yojson RoomMessages.of_yojson)

let keys_upload = ()

let keys_query users t =
  logged_in t >>=? fun token ->
  if List.length users > 0 then
    Api.keys_query token (Set.of_list (module String) users)
    |> send t >>=?
    cohttp_response_to_yojson >>|=?
    Responses.(of_yojson KeysQuery.of_yojson)
  else Lwt_result.fail `NoKeyQueryRequired

let keys_claim = ()

let to_device = ()

let devices t =
  logged_in t >>=? fun token ->
  Api.devices token
  |> send t >>=?
  cohttp_response_to_yojson >>|=?
  Responses.(of_yojson Devices.of_yojson)

let update_device device_id display_name t =
  logged_in t >>=? fun token ->
  `Assoc [ ("display_name", `String display_name) ]
  |> Api.update_device token device_id
  |> send t >>=?
  cohttp_response_to_yojson >>|=?
  Responses.(of_yojson UpdateDevice.of_yojson)

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
  |> send t >>=?
  cohttp_response_to_yojson >>|=?
  Responses.(of_yojson DeleteDevices.of_yojson)

let joined_members room_id t =
  logged_in t >>=? fun token ->
  Api.joined_members token room_id
  |> send t >>=?
  cohttp_response_to_yojson >>|=?
  Responses.(of_yojson JoinedMembers.of_yojson)

let joined_rooms t =
  logged_in t >>=? fun token ->
  Api.joined_rooms token
  |> send t >>=?
  cohttp_response_to_yojson >>|=?
  Responses.(of_yojson JoinedRooms.of_yojson)

let room_typing ?typing ?timeout room_id t =
  logged_in t >>=? fun token ->
  user_id t   >>=? fun id ->
  Api.room_typing ?typing ?timeout token room_id id
  |> send t >>=?
  cohttp_response_to_yojson >>|=?
  Responses.(of_yojson RoomTyping.of_yojson)

let update_receipt_marker ?receipt_type room_id event_id t =
  logged_in t >>=? fun token ->
  Api.update_receipt_marker ?receipt_type token room_id event_id
  |> send t >>=?
  cohttp_response_to_yojson >>|=?
  Responses.(of_yojson UpdateReceiptMarker.of_yojson)

let room_read_markers ?read_event_id room_id fully_read_event_id t =
  logged_in t >>=? fun token ->
  Api.room_read_markers ?read_event_id token room_id fully_read_event_id
  |> send t >>=?
  cohttp_response_to_yojson >>|=?
  Responses.(of_yojson RoomReadMarkers.of_yojson)

let content_repository_config t =
  logged_in t >>=? fun token ->
  Api.content_repository_config token
  |> send t >>=?
  cohttp_response_to_yojson >>|=?
  Responses.(of_yojson ContentRepositoryConfig.of_yojson)

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
  |> send ~content_type ~data_provider ?content_len t >>=?
  cohttp_response_to_yojson >>|=?
  Responses.(of_yojson Upload.of_yojson)

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

let download ?filename ?allow_remote server_name media_id t =
  Api.download ?filename ?allow_remote server_name media_id
  |> send t >>=?
  Types.DownloadedFile.of_cohttp_response

let thumbnail ?resize ?allow_remote server_name media_id ~w ~h t =
  let resize = Option.value ~default:Types.Resize.Scale resize in
  Api.thumbnail ?allow_remote server_name media_id ~w ~h resize
  |> send t >>=?
  Types.DownloadedFile.of_cohttp_response

let get_profile ?user_id t =
  Option.first_some user_id t.user_id |> function
  | None    -> Lwt_result.fail `NoUserID
  | Some id ->
    Api.get_profile id
    |> send t >>=?
    cohttp_response_to_yojson >>|=?
    Responses.(of_yojson GetProfile.of_yojson)

let get_presence user_id t =
  logged_in t >>=? fun token ->
  Api.get_presence token user_id
  |> send t >>=?
  cohttp_response_to_yojson >>|=?
  Responses.(of_yojson GetPresence.of_yojson)

let set_presence ?status_msg presence t =
  logged_in t >>=? fun token ->
  user_id t   >>=? fun id ->
  Api.set_presence ?status_msg token id presence
  |> send t >>=?
  cohttp_response_to_yojson >>|=?
  Responses.(of_yojson SetPresence.of_yojson)

let get_display_name ?user_id t =
  Option.first_some user_id t.user_id |> function
  | None    -> Lwt_result.fail `NoUserID
  | Some id ->
    Api.get_display_name id
    |> send t >>=?
    cohttp_response_to_yojson >>|=?
    Responses.(of_yojson GetDisplayName.of_yojson)

let set_display_name display_name t =
  logged_in t >>=? fun token ->
  user_id t   >>=? fun id ->
  Api.set_display_name token id display_name
  |> send t >>=?
  cohttp_response_to_yojson >>|=?
  Responses.(of_yojson SetDisplayName.of_yojson)

let get_avatar ?user_id t =
  Option.first_some user_id t.user_id |> function
  | None    -> Lwt_result.fail `NoUserID
  | Some id ->
    Api.get_avatar id
    |> send t >>=?
    cohttp_response_to_yojson >>|=?
    Responses.(of_yojson GetAvatar.of_yojson)

let set_avatar avatar_url t =
  logged_in t >>=? fun token ->
  user_id t   >>=? fun id ->
  Api.set_avatar token id avatar_url
  |> send t >>=?
  cohttp_response_to_yojson >>|=?
  Responses.(of_yojson SetAvatar.of_yojson)

let upload_filter ?user_id filter t =
  logged_in t >>=? fun token ->
  Option.first_some user_id t.user_id |> function
  | None    -> Lwt_result.fail `NoUserID
  | Some id ->
    Api.upload_filter token id (Filter.to_yojson filter)
    |> send t >>=?
    cohttp_response_to_yojson >>|=?
    Responses.(of_yojson UploadFilter.of_yojson)

let download_filter ?user_id filter_id t =
  logged_in t >>=? fun token ->
  Option.first_some user_id t.user_id |> function
  | None    -> Lwt_result.fail `NoUserID
  | Some id ->
    Api.download_filter token id filter_id
    |> send t >>=?
    cohttp_response_to_yojson >>|=?
    Responses.of_yojson Filter.of_yojson
