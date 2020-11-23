open Core

module Ext = struct
  type t = string option

  let image = [ "png"; "gif"; "jpg"; "jpeg"; "webp"; "tif"; "tiff"; "svg"; "bmp" ]
  let is_image = List.mem ~equal:String.equal image

  let video = [ "avi"; "mpeg"; "ogv"; "webm" ]
  let is_video = List.mem ~equal:String.equal video

  let audio = [ "aac"; "mid"; "midi"; "mp3"; "oga"; "opus"; "wav"; "weba" ]
  let is_audio = List.mem ~equal:String.equal audio

  let fix = function
    | "avi"  -> "x-msvideo"
    | "jpg"  -> "jpeg"
    | "mid"  -> "midi"
    | "mp3"  -> "mpeg"
    | "oga"  -> "ogg"
    | "ogv"  -> "ogg"
    | "svg"  -> "svg+xml"
    | "weba" -> "webm"
    | a      -> a

  let to_content_type = function
    | Some e when is_image e -> "image/" ^ fix e
    | Some e when is_video e -> "video/" ^ fix e
    | Some e when is_audio e -> "audio/" ^ fix e
    | _                      -> "appication/octet-stream"

  let to_msg_create ext =
    let open Event.Room.Message in
    match ext with
    | Some e when is_image e -> (fun ?url ?file body -> Image (Image.create ?url ?file body))
    | Some e when is_video e -> (fun ?url ?file body -> Video (Video.create ?url ?file body))
    | Some e when is_audio e -> (fun ?url ?file body -> Audio (Audio.create ?url ?file body))
    | _                      -> (fun ?url ?file body -> File  (File.create  ?url ?file body))
end
