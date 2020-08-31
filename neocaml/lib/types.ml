type credential = Password of string | AuthToken of string

type api_triple = Cohttp.Code.meth * string * Yojson.Basic.t option

module Presence = struct
  type t = Offline | Online | Unavailable

  let to_string = function
    | Offline     -> "offline"
    | Online      -> "online"
    | Unavailable -> "unavailable"

  (* Not sure if needed yet. *)
  let of_string = function
    | "offline"     -> Offline
    | "online"      -> Online
    | _             -> Unavailable
end

module Message_direction = struct
  type t = Back | Forward

  let to_string = function | Back -> "b" | Forward -> "f"
end

module Room_events = struct
  type t
end
