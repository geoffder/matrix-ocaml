open Base

open Lwt
open Cohttp
open Cohttp_lwt_unix

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

(* ctx would come from client if actually needed? See where it is used in nio *)
(* Create call closure so that it can be recursively called until response is
 * received, as with _send in nio async_client. *)
let send ?ctx ?headers client (meth, pth, content) =
  let uri = complete_uri client pth in
  let body = Option.map ~f:body_of_json content in
  let call = Client.call ?ctx ?headers ?body meth in
  call uri >>= fun (_resp, body) -> body |> json_of_body

let login ?device_name client cred =
  Api.login ?device_name ?device_id:client.device_id client.user cred
  |> send client
  >|= fun j ->
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
    { client with access_token = None }

(* So I should make my own version of the send function that returns a json of
 * the body (in Lwt.t monad). The methods that I need are post, get, delete,
 * and put. They all have the same sig, except for put. So I can't just pass
 * the function simply, I'll need to pass a sum type. Then the send method
 * can do it's calling until answered (not 429 code). Callbacks can be called
 * after the json is returned back to the parent function, since it knows what
 * kind of a request it was (don't have another type just to tag it for send) *)
