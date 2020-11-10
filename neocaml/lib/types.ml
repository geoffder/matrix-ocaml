open Core

module Credential = struct
  type t = Password of string | AuthToken of string
end

module Resize = struct
  type t = Scale | Crop

  let to_string = function | Scale -> "scale" | Crop -> "crop"
end

module Presence = struct
  type t = Offline | Online | Unavailable

  let to_string = function
    | Offline     -> "offline"
    | Online      -> "online"
    | Unavailable -> "unavailable"

  let of_yojson = function
    | `String "offline"     -> Result.return Offline
    | `String "online"      -> Result.return Online
    | `String "unavailable" -> Result.return Unavailable
    | `String s             -> Result.fail ("Invalid presence type: " ^ s)
    | _                     -> Result.fail "Presence must be Yojson string."
end

module MessageDirection = struct
  type t = Back | Forward

  let to_string = function | Back -> "b" | Forward -> "f"
end

module DownloadedFile = struct
  type t = { name     : string
           ; mimetype : string option
           ; bytes    : string
           }

  let disposition_regex = Str.regexp ".*filename="

  let of_cohttp_response ?filename (resp, body) =
    let open Lwt in
    Cohttp_lwt.Body.to_string body >>= fun bytes ->
    let name =
      match filename with
      | Some n -> n
      | None   ->
        Cohttp.Header.get resp.Cohttp.Response.headers "Content-Disposition"
        |> Option.map ~f:(Str.replace_first disposition_regex "")
        |> Option.value ~default:"neo_download"
    in
    let mimetype = Cohttp.Header.get_media_type resp.headers in
    Lwt.return_ok { name; mimetype; bytes }

  let save ?filename t path =
    let name = Option.value ~default:t.name filename in
    let oc   = Out_channel.create (Filename.concat path name) in
    fprintf oc "%s\n" t.bytes;
    Out_channel.close oc
end

module Signatures = struct
  open Yojson_helpers
  (* user -> algorithm:device_id -> signature *)
  type t = string StringMap.t StringMap.t [@@deriving yojson]
end

module DeviceKeys = struct
  (* algorithm:device_id -> key *)
  type keys = string Yojson_helpers.StringMap.t [@@deriving yojson]

  type t = { user_id    : string
           ; device_id  : string
           ; algorithms : string list
           ; keys       : keys
           ; signature  : Signatures.t
           } [@@deriving yojson]
end

module OneTimeKeys = struct
  type signed = { key : string
                ; signatures : Signatures.t
                } [@@deriving yojson]

  type one_time = Unsigned of string | Signed of signed

  let unsigned s = Unsigned s
  let signed s   = Signed s

  let one_time_to_yojson = function
    | Unsigned s -> `String s
    | Signed s   -> signed_to_yojson s

  let one_time_of_yojson = function
    | `Assoc _ as j -> signed_of_yojson j |> Result.map ~f:signed
    | `String s     -> Result.return (Unsigned s)
    | _             -> Result.fail "Invalid one_time object."

  type t = one_time Yojson_helpers.StringMap.t [@@deriving yojson]
end
