open Base
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
  module Common = struct
    type unsigned = { age : int; redacted_because : string option }
    [@@deriving of_yojson]

    type t = { event_id         : string
             ; origin_server_ts : int
             ; room_id          : string
             ; sender           : string
             ; state_key        : string option
             ; m_type           : string
             ; unsigned         : unsigned option
             } [@@deriving of_yojson]
  end

  module Message = struct
    (* TODO: Many of these fields will likely have to be marked as optional. I
     * have inconsistently marked some already, based on what the matrix spec
     * says is required. Need to decided between proactive agressive "optioning"
     * or rolling with it and making things options as I run in to problems. *)
    type thumbnail_info = { h        : int
                          ; w        : int
                          ; mimetype : string
                          ; size     : int
                          } [@@deriving of_yojson]

    type json_web_key = { kty     : string
                        ; key_ops : string list
                        ; alg     : string
                        ; k       : string
                        ; ext     : bool
                        }[@@deriving of_yojson]

    type encrypted_file = { url    : string
                          ; key    : json_web_key
                          ; iv     : string
                          (* ; hashes : (string, string, String.comparator_witness) Map.t *)
                          ; hashes : string (* actually string -> string map *)
                          ; v      : string  (* must be = "v2" *)
                          } [@@deriving of_yojson]

    type image_info = { h              : int
                      ; w              : int
                      ; mimetype       : string
                      ; size           : int
                      ; thumbnail_info : thumbnail_info
                      ; thumbnail_url  : string option
                      ; thumbnail_file : encrypted_file option
                      } [@@deriving of_yojson]


    module Text = struct
      type t = { body           : string
               ; format         : string option
               ; formatted_body : string option
               } [@@deriving of_yojson]
    end

    module Emote = struct
      type t = { body           : string
               ; format         : string option
               ; formatted_body : string option
               } [@@deriving of_yojson]
    end

    module Notice = struct
      type t = { body           : string
               ; format         : string option
               ; formatted_body : string option
               } [@@deriving of_yojson]
    end

    module Image = struct
      (* NOTE: url is required if unencrypted, file is required if encrypted. *)
      type t = { body : string
               ; info : image_info option
               ; url  : string option
               ; file : encrypted_file option
               } [@@deriving of_yojson]
    end

    module File = struct
      type info = { mimetype : string
                  ; size     : int
                  ; thumbnail_url : string option
                  ; thumbnail_file : encrypted_file option
                  ; thumbnail_info : thumbnail_info option
                  } [@@deriving of_yojson]

      type t = { body     : string
               ; filename : string option
               ; info     : info option
               ; url      : string option
               ; file     : encrypted_file option
               } [@@deriving of_yojson]
    end

    module Audio = struct
      type info = { duration : int
                  ; mimetype : string
                  ; size     : int
                  } [@@deriving of_yojson]

      type t = { body     : string
               ; info     : info option
               ; url      : string option
               ; file     : encrypted_file option
               } [@@deriving of_yojson]
    end

    module Location = struct
      type info = { thumbnail_url  : string option
                  ; thumbnail_file : encrypted_file option
                  ; thumbnail_info : thumbnail_info option
                  } [@@deriving of_yojson]

      type t = { body     : string
               ; geo_uri  : string
               ; info     : info option
               } [@@deriving of_yojson]
    end

    module Video = struct
      type info = { duration       : int
                  ; h              : int
                  ; w              : int
                  ; mimetype       : string
                  ; size           : int
                  ; thumbnail_url  : string option
                  ; thumbnail_file : encrypted_file option
                  ; thumbnail_info : thumbnail_info option
                  } [@@deriving of_yojson]

      type t = { body : string
               ; info : info
               ; url  : string
               ; file : encrypted_file option
               } [@@deriving of_yojson]
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

    let text m     = Text m
    let emote m    = Emote m
    let notice m   = Notice m
    let image m    = Image m
    let file m     = File m
    let audio m    = Audio m
    let location m = Location m
    let video m    = Video m

    let of_yojson content =
      U.member "msgtype" content
      |> U.to_string_option
      |> Option.map ~f:begin
        function
        | "m.text"     -> Text.of_yojson content     |> Result.map ~f:text
        | "m.emote"    -> Emote.of_yojson content    |> Result.map ~f:emote
        | "m.notice"   -> Notice.of_yojson content   |> Result.map ~f:notice
        | "m.image"    -> Image.of_yojson content    |> Result.map ~f:image
        | "m.file"     -> File.of_yojson content     |> Result.map ~f:file
        | "m.audio"    -> Audio.of_yojson content    |> Result.map ~f:audio
        | "m.location" -> Location.of_yojson content |> Result.map ~f:location
        | "m.video"    -> Video.of_yojson content    |> Result.map ~f:video
        | _            -> Result.return Unknown
      end
      |> Option.value ~default:(Result.fail "Missing msgtype.")
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
