open Neo_infix

module U = Yojson.Safe.Util

(* NOTE: This wasn't based on looking at jsons, but rather from a dataclass
 * in nio. I'll focus on the actual jsons from matrix since I need to make
 * the records line up. *)
module Common = struct
  type t = { source           : Yojson.Safe.t
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
    (* Stand-in stuff. It seems like the room message fields are fairly
     * similar in the nio dataclass types, but some of the fields are taking
     * jsons/dicts, so they could have anything in them really. Means that I
     * have to learn and sort out the matrix schemas and create types. I'm
     * hoping to not give up and pass around yojsons like I'm dynamically
     * typing... *)

    (* Missing optional "state_key" field? *)
    module Common = struct
      type unsigned = { age : int; redacted_because : string option }
      [@@deriving of_yojson { exn = true }]

      type t = { event_id : string
               ; origin_server_ts : int
               ; room_id : string
               ; sender : string
               ; m_type : string
               ; unsigned : unsigned option
               } [@@deriving of_yojson { exn = true }]
    end

    module Text = struct
      type t = { body           : string
               ; format         : string option
               ; formatted_body : string option
               } [@@deriving of_yojson { exn = true }]
    end

    module Emote = struct
      type t = { body           : string
               ; format         : string option
               ; formatted_body : string option
               } [@@deriving of_yojson { exn = true }]
    end

    module Notice = struct
      type t = { body           : string
               ; format         : string option
               ; formatted_body : string option
               } [@@deriving of_yojson { exn = true }]
    end

    module Image = struct
      type t
    end

    module File = struct
      type t
    end

    module Audio = struct
      type t
    end

    module Location = struct
      type t
    end

    (* TODO: What is optional? Schema in nio only requires body, url, and
     * msgtype. The info stuff is left out, I got that from the client-server
     * API Swagger UI page. *)
    module Video = struct
      type thumbnail_info = { h        : int
                            ; w        : int
                            ; mimetype : string
                            ; size     : int
                            }

      type info = { duration       : int
                  ; h              : int
                  ; mimetype       : string
                  ; size           : int
                  ; thumbnail_info : thumbnail_info
                  ; thumnail_url   : string
                  ; w              : int
                  }

      type t = { body : string
               ; info : info
               ; url  : string
               }
    end

    type details =
      | Text of Text.t
      | Emote of Emote.t
      | Notice of Notice.t
      | Image of Image.t
      | File of File.t
      | Audio of Audio.t
      | Location of Location.t
      | Video of Video.t
      | Unknown

    type t = Common.t * details

    let to_mtype = function
      | Text     _ -> "m.text"
      | Emote    _ -> "m.emote"
      | Notice   _ -> "m.notice"
      | Image    _ -> "m.image"
      | File     _ -> "m.file"
      | Audio    _ -> "m.audio"
      | Location _ -> "m.location"
      | Video    _ -> "m.video"
      | Unknown    -> "unknown message type"

    (* NOTE: Using _exn here out of laziness right now, have to decide how I want
     * to structure wrt to error handling of yojson conversion. *)

    (* let of_yojson m_type j =
     *   let common = Common.of_yojson_exn j in
     *   let content = U.member "content" j in
     *   let details =
     *     match m_type with
     *     | "m.text"     -> Text (Text.of_yojson_exn content)
     *     | "m.emote"    -> Emote (Emote.of_yojson_exn content)
     *     | "m.notice"   -> Notice (Notice.of_yojson_exn content)
     *     | "m.image"    -> Image (Image.of_yojson_exn content)
     *     | "m.file"     -> File (File.of_yojson_exn content)
     *     | "m.audio"    -> Audio (Audio.of_yojson_exn content)
     *     | "m.location" -> Location (Location.of_yojson_exn content)
     *     | "m.video"    -> Video (Video.of_yojson_exn content)
     *     | _            -> Unknown in
     *   (common, details) *)

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
    type t = string

    let of_yojson = U.member "name" >> U.to_string
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
