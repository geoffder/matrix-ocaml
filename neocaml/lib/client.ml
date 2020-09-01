open Base

open Lwt
open Cohttp
open Cohttp_lwt_unix

(* open Types *)
(* open Yojson_helpers *)

type t = { homeserver      : string
         ; user            : string
         ; user_id         : string option
         ; device_id       : string option
         ; store_path      : string option
         ; access_token    : string option
         ; rooms           : (string, Room.t, String.comparator_witness) Map.t
         ; encrypted_rooms : (string, String.comparator_witness) Set.t
         }

type credential = Password of string | AuthToken of string

let make ?device_id ?store_path ?access_token homeserver user =
  { homeserver
  ; user
  ; user_id         = None
  ; device_id
  ; store_path
  ; access_token
  ; rooms           = Map.empty (module String)
  ; encrypted_rooms = Set.empty (module String)
  }

let complete_uri client pth = client.homeserver ^ pth |> Uri.of_string

let response_code = Response.status |> Fn.compose Code.code_of_status

let body_of_json j = j |> Yojson.Basic.to_string |> Cohttp_lwt.Body.of_string

let json_of_body b = b |> Cohttp_lwt.Body.to_string >|= Yojson.Basic.from_string

(* Continue execution of given function if logged in. *)
let logged_in client f =
  match client.access_token with
  | None ->
    Stdio.print_endline "Not logged in.";
    Lwt.return client
  | Some token -> f token

(* ctx would come from client if actually needed? See where it is used in nio *)
(* Create call closure so that it can be recursively called until response is
 * received (e.g. not 429 code), as with _send in nio async_client.
 *
 * Need to figure out my error response handing architecture (general in here
 * and specific in the calling functions). Or have a handler that only gets called
 * after send has returned, but it has the general cases closed in, and accepts
 * specific ones as a parameter *)
let send ?ctx ?content_type ?content_len ?_timeout client (meth, pth, content) =
  let headers =
    ("Content-Type", Option.value ~default:"application/json" content_type)
    :: Option.value_map
      ~f:(fun i -> [ ("Content-Length", Int.to_string i) ])
      ~default:[]
      content_len
    |> Header.of_list in
  let uri = complete_uri client pth in
  let body = Option.map ~f:body_of_json content in
  let call = Client.call ?ctx ~headers ?body meth in
  call uri >>= fun (_resp, body) -> body |> json_of_body

let login ?device_name client cred =
  Api.login ?device_name ?device_id:client.device_id client.user cred
  |> send client
  >|= fun j ->
  (* TODO: handling / call backs ? *)
  let open Yojson.Basic.Util in
  { client with
    user_id      = j |> member "user_id" |> to_string_option
  ; device_id    = j |> member "device_id" |> to_string_option
  ; access_token = j |> member "access_token" |> to_string_option
  }

let logout ?all_devices:(all_devices=false) client =
  match client.access_token with
  | None -> Lwt.return client
  | Some token ->
    Api.logout ~all_devices token
    |> send client
    >|= fun _ ->
    (* nio _handle_logout checks for ErrorResponse, doesn't clear if so... *)
    (* TODO: handling / call backs *)
    { client with access_token = None }

let joined_rooms client =
  logged_in client begin fun token ->
    Api.joined_rooms token
    |> send client
    >|= fun j ->
    (* TODO: handling / call backs *)
    Yojson.Basic.to_string j |> Stdio.print_endline;
    client
  end

let room_messages ?stop ?dir ?lim:(lim=10) ?filter client id start =
  logged_in client begin fun token ->
    Api.room_messages ?stop ?dir ~lim ?filter token id start
    |> send client
    >|= fun j ->
    (* TODO: handling / call backs *)
    Yojson.Basic.to_string j |> Stdio.print_endline;
    client
  end

let sync ?since ?timeout ?filter ?full_state:(full=false) ?set_presence client =
  logged_in client begin fun token ->
    Api.sync ?since ?timeout ?filter ~full_state:full ?set_presence token
    |> send client
    >|= fun j ->
    (* TODO: handling / call backs *)
    Yojson.Basic.to_string j |> Stdio.print_endline;
    client
  end
