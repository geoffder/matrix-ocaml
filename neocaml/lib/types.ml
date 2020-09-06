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

  (* Not sure if needed yet. *)
  let of_string = function
    | "offline"     -> Offline
    | "online"      -> Online
    | "unavailable"
    | _             -> Unavailable
end

module MessageDirection = struct
  type t = Back | Forward

  let to_string = function | Back -> "b" | Forward -> "f"
end
