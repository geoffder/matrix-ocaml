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

      (* Stand-in stuff. It seems like the room message fields are fairly
       * similar in the nio dataclass types, but some of the fields are taking
       * jsons/dicts, so they could have anything in them really. Means that I
       * have to learn and sort out the matrix schemas and create types. I'm
       * hoping to not give up and pass around yojsons like I'm dynamically
       * typing... *)
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

      (* NOTE: Do I have an unknown type? Seems like there is an unknown room
       * message type in nio so I will have to work something out. *)
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

    let to_type_string = function
      | Message           _ -> "m.room.message"
      | Create            _ -> "m.room.create"
      | GuestAccess       _ -> "m.room.guest_access"
      | JoinRules         _ -> "m.room.join_rules"
      | HistoryVisibility _ -> "m.room.history_visibility"
      | Member            _ -> "m.room.member"
      | CanonicalAlias    _ -> "m.room.canonical_alias"
      | Name              _ -> "m.room.name"
      | Topic             _ -> "m.room.topic"
      | Avatar            _ -> "m.room.avatar"
      | PowerLevels       _ -> "m.room.power_levels"
      | PinnedEvents      _ -> "m.room.pinned_events"
      | Encryption        _ -> "m.room.encryption"
      | Redaction         _ -> "m.room.redaction"
      | Encrypted         _ -> "m.room.encrypted"

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
end
