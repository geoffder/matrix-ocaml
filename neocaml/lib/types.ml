open Base

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
