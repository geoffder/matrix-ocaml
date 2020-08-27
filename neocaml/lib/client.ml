open Base
(* open Yojson *)
open Lwt
open Cohttp
open Cohttp_lwt_unix

type t = { homeserver      : string
         ; user            : string
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
  ; device_id
  ; store_path
  ; access_token
  ; rooms           = Map.empty (module String)
  ; encrypted_rooms = Set.empty (module String)
  }

(* TODO: Build request with Api.login and send to server.
 * Trying with copy of example request first... *)
let login ?device_name client cred =
  let pth, content =
    Api.login ?device_name ?device_id:client.device_id client.user cred in
  let body = content |> Yojson.Basic.to_string |> Cohttp_lwt.Body.of_string in
  Client.post ~body (Uri.of_string (client.homeserver ^ pth))
  >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  Stdio.printf "Response code: %d\n" code;
  Stdio.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  Stdio.printf "Body of length: %d\n" (String.length body);
  body
