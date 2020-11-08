open Core

module Credential = struct
  type t = Password of string | AuthToken of string
end

module Resize = struct
  type t = Scale | Crop

  let to_string = function | Scale -> "scale" | Crop -> "crop"
end

module Presence = struct
  (* Do I bother making an invalid case? *)
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

  let of_cohttp_response ?filename (resp, body) =
    let open Lwt in
    Cohttp_lwt.Body.to_string body >>= fun bytes ->
    let name =
      match filename with
      | Some n -> n
      | None   ->
        Cohttp.Header.get resp.Cohttp.Response.headers "Content-Disposition"
        |> Option.value ~default:"neo_download"
    in
    let mimetype = Cohttp.Header.get_media_type resp.headers in
    Lwt.return_ok { name; mimetype; bytes }

  let save t path =
    (* TODO: Use proper path concactentation function. *)
    let oc = Out_channel.create (path ^ t.name) in
    fprintf oc "%s\n" t.bytes;
    Out_channel.close oc
end
