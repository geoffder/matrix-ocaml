type credential = Password of string | AuthToken of string

type api_triple = Cohttp.Code.meth * string * Yojson.Basic.t option

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

module MatrixEvent = struct
  (* TODO: Should this not be t, and rather t includes these common specs,
   * then a variant of records holds the "subtype" specific fields? In nio they
   * obviously use inheritance, but I'd like to do this in an OCaml way. *)
  type t = { source           : Yojson.Basic.t
           ; event_id         : string
           ; sender           : string
           ; server_timestamp : int
           ; decrypted        : bool
           ; verified         : bool
           ; sender_key       : string option
           ; session_id       : string option
           ; transaction_id   : string option
           }
end
