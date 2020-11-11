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

  let to_msg_create = function
    | Some e when is_image e -> Event.Room.Message.Image.no_info_msg
    | Some e when is_video e -> Event.Room.Message.Video.no_info_msg
    | Some e when is_audio e -> Event.Room.Message.Audio.no_info_msg
    | _                      -> Event.Room.Message.File.no_info_msg
end
