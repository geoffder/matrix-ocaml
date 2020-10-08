open Core

open Lwt
open Cohttp
open Cohttp_lwt_unix

(* open Types *)
(* open Yojson_helpers *)
open Neo_infix

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

let make ?device_id ?store_path ?access_token homeserver user =
  { homeserver
  ; user
  ; user_id         = None
  ; device_id
  ; store_path
  ; access_token
  ; rooms           = Map.empty (module String)
  ; encrypted_rooms = Set.empty (module String)
  ; random_state    = Random.State.make_self_init ()
  }

let complete_uri client pth = client.homeserver ^ pth |> Uri.of_string

let response_code = Response.status >> Code.code_of_status

let body_of_json j = j |> Yojson.Safe.to_string |> Cohttp_lwt.Body.of_string

let json_of_body b = b |> Cohttp_lwt.Body.to_string >|= Yojson.Safe.from_string

(* Continue execution of given function if logged in. *)
let logged_in client =
  client.access_token
  |> Option.value_map ~f:Lwt.return_ok
    ~default:(Lwt.return_error "Not logged in.")

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
    let loaded = call ?body:(get_data ()) meth in
    with_timeout ?timeout ~call:loaded uri
    >>= function
    | Ok (resp, _body as response) ->
      if phys_equal resp.Response.status `Too_many_requests
      then
        if n_429s < max_429s
        then Lwt_unix.sleep 5. >>= fun () -> aux (n_429s + 1) n_outs
        else Lwt.return_error "Hit maximum 'too many requests' (429) responses."
      else Lwt.return_ok response
    | Error `TimedOut ->
      if n_outs < max_outs
      then Lwt_unix.sleep 5. >>= fun () -> aux n_429s (n_outs + 1)
      else Lwt.return_error "Hit maximum timeouts." in
  aux 0 0

(* ctx would come from client if actually needed? See where it is used in nio *)
let send
    ?ctx
    ?content_type
    ?content_len
    ?timeout
    ?data_provider
    client
    (meth, pth, content)
  =
  let headers =
    ("Content-Type", Option.value ~default:"application/json" content_type)
    :: Option.value_map
      ~f:(fun i -> [ ("Content-Length", Int.to_string i) ])
      ~default:[]
      content_len
    |> Header.of_list in
  let uri = complete_uri client pth in
  let body = Option.map ~f:body_of_json content in
  let get_data = Option.value ~default:(fun () -> body) data_provider in
  let call = Client.call ?ctx ?chunked:None ~headers in
  repeat ?timeout ~call ~get_data meth uri
  >=> fun (_resp, body) ->
    Cohttp_lwt.Body.to_string body
    >>= ( Yojson.Safe.from_string >> Lwt.return_ok )

let login ?device_name client cred =
  Api.login ?device_name ?device_id:client.device_id client.user cred
  |> send client
  >>| fun j ->
  let open Yojson.Safe.Util in
  { client with
    user_id      = j |> member "user_id" |> to_string_option
  ; device_id    = j |> member "device_id" |> to_string_option
  ; access_token = j |> member "access_token" |> to_string_option
  }

let logout ?(all_devices=false) client =
  match client.access_token with
  | None -> Lwt.return_error "Already logged out."
  | Some token ->
    Api.logout ~all_devices token
    |> send client
    >>| fun _ -> { client with access_token = None }

let joined_rooms client =
  logged_in client >=> fun token ->
    Api.joined_rooms token
    |> send client
    >>| Responses.JoinedRooms.of_yojson

(* NOTE:
 * Response is a list of room events, the of json is a bit more compicated.
 * Requires checking "content" -> "msgtype" for each element of the returned
 * list at key "chunk". Keys "end" and "start" contain the since tokens I think.
 * m.room.message and m.room.name are actually both possible for "type", which
 * is at the same level as "content", meaning that not just messages (which have
 * a "msgtype" in their "content" dict) are included. The module for this one
 * must cover a lot of bases. *)
let room_messages ?stop ?dir ?(limit=10) ?filter client id start =
  logged_in client >=> fun token ->
    Api.room_messages ?stop ?dir ~limit ?filter token id start
    |> send client
    (* >>| Yojson.Safe.to_string *)
    >>| Responses.RoomMessages.of_yojson

let room_send client id event =
  logged_in client >=> fun token ->
    let body = Events.Room.Content.to_yojson event in
    let m_type = Events.Room.Content.to_m_type event in
    let tx_id = Uuid.create_random client.random_state |> Uuid.to_string in
    Api.room_send token id m_type body tx_id
    |> send client
    >>| Responses.RoomSend.of_yojson

let sync ?since ?timeout ?filter ?full_state:(full=false) ?set_presence client =
  logged_in client >=> fun token ->
    Api.sync ?since ?timeout ?filter ~full_state:full ?set_presence token
    |> send client
    (* >>| Yojson.Safe.to_string *)
    >>| Responses.Sync.of_yojson
