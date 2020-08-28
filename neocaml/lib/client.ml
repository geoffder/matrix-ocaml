open Base
(* open Yojson *)
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

let login ?device_name client cred =
  let pth, content =
    Api.login ?device_name ?device_id:client.device_id client.user cred in
  Client.post ~body:(body_of_json content) (complete_uri client pth)
  >>= fun (_resp, body) ->
  body |> json_of_body >|= fun j ->
  let open Yojson.Basic.Util in
  { client with
    user_id      = j |> member "user_id" |> to_string_option
  ; device_id    = j |> member "device_id" |> to_string_option
  ; access_token = j |> member "access_token" |> to_string_option
  }
