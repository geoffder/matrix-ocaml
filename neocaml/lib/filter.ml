open! Core

module Event = struct
  type t = { limit       : int option         [@default None]
           ; not_senders : string list option [@default None]
           ; not_types   : string list option [@default None]
           ; senders     : string list option [@default None]
           ; types       : string list option [@default None]
           } [@@deriving yojson]
end

module RoomEvent = struct
  type t = { limit                     : int option         [@default None]
           ; not_senders               : string list option [@default None]
           ; not_types                 : string list option [@default None]
           ; senders                   : string list option [@default None]
           ; types                     : string list option [@default None]
           ; lazy_load_members         : bool option        [@default None]
           ; include_redundant_members : bool option        [@default None]
           ; not_rooms                 : string list option [@default None]
           ; rooms                     : string list option [@default None]
           ; contains_url              : bool option        [@default None]
           } [@@deriving yojson]
end

module State = struct
  type t = { limit                     : int option         [@default None]
           ; not_senders               : string list option [@default None]
           ; not_types                 : string list option [@default None]
           ; senders                   : string list option [@default None]
           ; types                     : string list option [@default None]
           ; lazy_load_members         : bool option        [@default None]
           ; include_redundant_members : bool option        [@default None]
           ; not_rooms                 : string list option [@default None]
           ; rooms                     : string list option [@default None]
           ; contains_url              : bool option        [@default None]
           } [@@deriving yojson]
end

module Room = struct
  type t = { not_rooms     : string list option [@default None]
           ; rooms         : string list option [@default None]
           ; ephemeral     : RoomEvent.t option [@default None]
           ; include_leave : bool option        [@default None]
           ; state         : State.t option     [@default None]
           ; timeline      : RoomEvent.t option [@default None]
           ; account_data  : RoomEvent.t option [@default None]
           } [@@deriving yojson]
end

type event_format = Client | Federation

let event_format_to_string = function
  | Client     -> "client"
  | Federation -> "federation"

let event_format_to_yojson e = `String (event_format_to_string e)

let event_format_of_yojson = function
  | `String "client"     -> Result.return Client
  | `String "federation" -> Result.return Federation
  | _                    -> Result.fail "Invalid event_format."

type t = { event_fields : string list option  [@default None]
         ; event_format : event_format option [@default None]
         ; presence     : Event.t option      [@default None]
         ; account_data : Event.t option      [@default None]
         ; room         : Room.t option       [@default None]
         } [@@deriving yojson]
