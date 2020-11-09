open Base
open Yojson_helpers

type t

module Config = struct
  type vis = Private | Public

  let vis_to_string = function | Private -> "private" | Public -> "public"

  let vis_of_string = function | "private" -> Private | _ -> Public

  let vis_to_yojson = Fn.compose yo_string vis_to_string

  type preset = PrivateChat | TrustedPrivateChat | PublicChat

  let preset_to_string = function
    | PrivateChat        -> "private_chat"
    | TrustedPrivateChat -> "trusted_private_chat"
    | PublicChat         -> "public_chat"

  let preset_to_yojson = Fn.compose yo_string preset_to_string

  let preset_option_to_json p =
    p |> Option.map ~f:preset_to_string |> json_of_option yo_string

  let federate_to_json f = yo_assoc [ ("m.federate", yo_bool f) ]

  type t =
    { visibility   : vis
    ; alias        : string option
    ; name         : string option [@key "room_alias_name"]
    ; topic        : string option
    ; room_version : string option
    ; federate     : bool          [@key "creation_content"]
    ; preset       : preset option
    ; is_direct    : bool
    } [@@deriving to_yojson]

  let create
      ?(visibility=Private)
      ?alias
      ?name
      ?topic
      ?room_version
      ?(federate=true)
      ?preset
      ?(is_direct=false)
      ()
    =
    { visibility; alias; name; topic; room_version; federate; preset; is_direct }
end
