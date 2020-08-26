open Base

type t = { homeserver      : string
         ; user            : string
         ; device_id       : string option
         ; store_path      : string option
         ; access_token    : string option
         ; rooms           : (string, Room.t, String.comparator_witness) Map.t
         ; encrypted_rooms : (string, String.comparator_witness) Set.t }

type credential = Password of string | AuthToken of string

(* TODO: Build request with Api.login and send to server. *)
let login ?device_name client cred = ()
