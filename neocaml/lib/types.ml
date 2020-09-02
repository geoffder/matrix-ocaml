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
  module Common = struct
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

  module Room = struct
    module Message = struct
      type t
    end

    module Create = struct
      type t
    end

    module GuestAccess = struct
      type t
    end

    module JoinRules = struct
      type t
    end

    module HistoryVisibility = struct
      type t
    end

    module Member = struct
      type t
    end

    module CanonicalAlias = struct
      type t
    end

    module Name = struct
      type t
    end

    module Topic = struct
      type t
    end

    module Avatar = struct
      type t
    end

    module PowerLevels = struct
      type t
    end

    module PinnedEvents = struct
      type t
    end

    module Encryption = struct
      type t
    end

    module Redaction = struct
      type t
    end

    module Encrypted = struct
      type t
    end

    type t =
      | Message of Message.t
      | Create of Create.t
      | GuestAccess of GuestAccess.t
      | JoinRules of JoinRules.t
      | HistoryVisibility of HistoryVisibility.t
      | Member of Member.t
      | CanonicalAlias of CanonicalAlias.t
      | Name of Name.t
      | Topic of Topic.t
      | Avatar of Avatar.t
      | PowerLevels of PowerLevels.t
      | PinnedEvents of PinnedEvents.t
      | Encryption of Encryption.t
      | Redaction of Redaction.t
      | Encrypted of Encrypted.t
  end

  module Call = struct
    module Candidates = struct
      type t
    end

    module Invite = struct
      type t
    end

    module Answer = struct
      type t
    end

    module Hangup = struct
      type t
    end

    type t =
      | Candidates of Candidates.t
      | Invite of Invite.t
      | Answer of Answer.t
      | Hangup of Hangup.t
  end

  module Presence = struct
    type t
  end

  type t =
    | Room of Common.t * Room.t
    | Call of Common.t * Call.t
    | Presence of Common.t * Presence.t
    | Unknown of Common.t

  (* Standin. Haven't decided where this goes yet. Collect all of the matrix
   * evemt types that I find... Also, need to decide on breaking things up,
   * e.g. do the room types live as a different type or in another module? *)
  type tag =
    | RoomMessage
    | RoomCreate
    | RoomGuestAccess
    | RoomJoinRules
    | RoomHistoryVisibility
    | RoomMember
    | RoomCanonicalAlias
    | RoomName
    | RoomTopic
    | RoomAvatar
    | RoomPowerLevels
    | RoomPinnedEvents
    | RoomEncryption
    | RoomRedaction
    | RoomEncrypted
    | CallCandidates
    | CallInvite
    | CallAnswer
    | CallHangup
    | PresenceEvent
    | UnknownEvent

  let string_of_tag = function
    | RoomMessage           -> "m.room.message"
    | RoomCreate            -> "m.room.create"
    | RoomGuestAccess       -> "m.room.guest_access"
    | RoomJoinRules         -> "m.room.join_rules"
    | RoomHistoryVisibility -> "m.room.history_visibility"
    | RoomMember            -> "m.room.member"
    | RoomCanonicalAlias    -> "m.room.canonical_alias"
    | RoomName              -> "m.room.name"
    | RoomTopic             -> "m.room.topic"
    | RoomAvatar            -> "m.room.avatar"
    | RoomPowerLevels       -> "m.room.power_levels"
    | RoomPinnedEvents      -> "m.room.pinned_events"
    | RoomEncryption        -> "m.room.encryption"
    | RoomRedaction         -> "m.room.redaction"
    | RoomEncrypted         -> "m.room.encrypted"
    | CallCandidates        -> "m.call.candidates"
    | CallInvite            -> "m.call.invite"
    | CallAnswer            -> "m.call.answer"
    | CallHangup            -> "m.call.hangup"
    | PresenceEvent         -> "m.presence"
    | UnknownEvent          -> "unknown_event"

  let tag_of_string = function
    | "m.room.message"            -> RoomMessage
    | "m.room.create"             -> RoomCreate
    | "m.room.guest_access"       -> RoomGuestAccess
    | "m.room.join_rules"         -> RoomJoinRules
    | "m.room.history_visibility" -> RoomHistoryVisibility
    | "m.room.member"             -> RoomMember
    | "m.room.canonical_alias"    -> RoomCanonicalAlias
    | "m.room.name"               -> RoomName
    | "m.room.topic"              -> RoomTopic
    | "m.room.avatar"             -> RoomAvatar
    | "m.room.power_levels"       -> RoomPowerLevels
    | "m.room.pinned_events"      -> RoomPinnedEvents
    | "m.room.encryption"         -> RoomEncryption
    | "m.room.redaction"          -> RoomRedaction
    | "m.room.encrypted"          -> RoomEncrypted
    | "m.call.candidates"         -> CallCandidates
    | "m.call.invite"             -> CallInvite
    | "m.call.answer"             -> CallAnswer
    | "m.call.hangup"             -> CallHangup
    | "m.presence"                -> PresenceEvent
    | _                           -> UnknownEvent
end

(* NOTE: Just stand-in stuff, will need to get the module struct treatment
 * for each message type. *)
module RoomMessage = struct
  type tag =
    | Text
    | Emote
    | Notice
    | Image
    | File
    | Audio
    | Location
    | Video

  let string_of_tag = function
    | Text     -> "m.text"
    | Emote    -> "m.emote"
    | Notice   -> "m.notice"
    | Image    -> "m.image"
    | File     -> "m.file"
    | Audio    -> "m.audio"
    | Location -> "m.location"
    | Video    -> "m.video"

  (* NOTE: Do I have an unknown type, or throw an exception? *)
  let tag_of_string = function
    | "m.text"     -> Text
    | "m.emote"    -> Emote
    | "m.notice"   -> Notice
    | "m.image"    -> Image
    | "m.file"     -> File
    | "m.audio"    -> Audio
    | "m.location" -> Location
    | "m.video"    -> Video
    | _            -> Text
end
