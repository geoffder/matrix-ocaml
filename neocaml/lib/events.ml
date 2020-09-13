open Base
(* open Neo_infix *)

module U = Yojson.Safe.Util

type json_web_key = { kty     : string
                    ; key_ops : string list
                    ; alg     : string
                    ; k       : string
                    ; ext     : bool
                    } [@@deriving of_yojson]

type encrypted_file = { url    : string
                      ; key    : json_web_key
                      ; iv     : string
                      (* ; hashes : (string, string, String.comparator_witness) Map.t *)
                      ; hashes : (string * string) list
                      ; v      : string  (* must be = "v2" *)
                      } [@@deriving of_yojson]

type thumbnail_info = { h        : int option
                      ; w        : int option
                      ; mimetype : string option
                      ; size     : int option
                      } [@@deriving of_yojson]

type image_info = { h              : int option
                  ; w              : int option
                  ; mimetype       : string option
                  ; size           : int option
                  ; thumbnail_info : thumbnail_info option
                  ; thumbnail_url  : string option option
                  ; thumbnail_file : encrypted_file option
                  } [@@deriving of_yojson]

(* NOTE: This wasn't based on looking at jsons, but rather from a dataclass
 * in nio. I'll focus on the actual jsons from matrix since I need to make
 * the records line up. *)
(* module Common = struct
 *   type t = { source           : Yojson.Safe.t
 *            ; event_id         : string
 *            ; sender           : string
 *            ; server_timestamp : int
 *            ; decrypted        : bool
 *            ; verified         : bool
 *            ; sender_key       : string option
 *            ; session_id       : string option
 *            ; transaction_id   : string option
 *            }
 * end *)

module Room = struct
  module Message = struct
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
      type info = { mimetype : string option
                  ; size     : int option
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
      type info = { duration : int option
                  ; mimetype : string option
                  ; size     : int option
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
      type info = { duration       : int option
                  ; h              : int option
                  ; w              : int option
                  ; mimetype       : string option
                  ; size           : int option
                  ; thumbnail_url  : string option
                  ; thumbnail_file : encrypted_file option
                  ; thumbnail_info : thumbnail_info option
                  } [@@deriving of_yojson]

      (* NOTE: url or file is required depending on encryption. *)
      type t = { body : string
               ; info : info option
               ; url  : string option
               ; file : encrypted_file option
               } [@@deriving of_yojson]
    end

    type t =
      | Text of Text.t
      | Emote of Emote.t
      | Notice of Notice.t
      | Image of Image.t
      | File of File.t
      | Audio of Audio.t
      | Location of Location.t
      | Video of Video.t
      | Unknown

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
    type previous_room = { room_id  : string
                         ; event_id : string
                         } [@@deriving of_yojson]

    type t = { creator      : string
             ; federate     : bool option
             ; room_version : string option
             ; predecessor  : previous_room option
             } [@@deriving of_yojson]
  end

  module GuestAccess = struct
    type access = CanJoin | Forbidden

    let access_of_yojson = function
      | `String "can_join"  -> Result.return CanJoin
      | `String "forbidden" -> Result.return Forbidden
      | `String s           -> Result.fail ("Invalid enum value: " ^ s)
      | _                   -> Result.fail "Missing/wrong-typed field."

    type t = { guest_access : access } [@@deriving of_yojson]
  end

  module JoinRules = struct
    type t = { join_rule : string } [@@deriving of_yojson]
  end

  module HistoryVisibility = struct
    type visibility = Invited | Joined | Shared | WorldReadable

    let visibility_of_yojson = function
      | `String "invited"        -> Result.return Invited
      | `String "joined"         -> Result.return Joined
      | `String "shared"         -> Result.return Shared
      | `String "world_readable" -> Result.return WorldReadable
      | `String s                -> Result.fail ("Invalid enum value: " ^ s)
      | _                        -> Result.fail "Missing/wrong-typed field."

    type t = { history_visibility : visibility } [@@deriving of_yojson]
  end

  module Member = struct
    type membership = Invite | Join | Knock | Leave | Ban

    let membership_of_yojson = function
      | `String "invite" -> Result.return Invite
      | `String "join"   -> Result.return Join
      | `String "knock"  -> Result.return Knock
      | `String "leave"  -> Result.return Leave
      | `String "ban"    -> Result.return Ban
      | `String s        -> Result.fail ("Invalid enum value: " ^ s)
      | _                -> Result.fail "Missing / invalid membership field."

    (* TODO: Need to read more into this... This is the basic structure though.
     * Probably should just make it a map then.
     * See: https://matrix.org/docs/spec/appendices#signing-json *)
    type signatures = (string * (string * string) list) list [@@deriving of_yojson]

    type signed = { mxid       : string
                  ; signatures : signatures
                  ; token      : string
                  } [@@deriving of_yojson]

    type invite = { display_name : string
                  ; signed       : signed
                  } [@@deriving of_yojson]

    (* TODO: This will require some extra care. The content is a room state
     * event, like m.room.name (referred to by m_type / "type"). Can I make the
     * content type Events.Room.t and use its of_yojson? I might not be able to
     * since it would require recursive types across the modules? Would I have
     * to move this module under all the state ones, then have a jank type
     * within this one that replicates the state parts of Events.Room.t ? *)
    type stripped_state = { content   : Yojson.Safe.t (* Events.Room.t *)
                          ; state_key : string
                          ; m_type    : string [@key "type"]
                          ; sender    : string
                          } [@@deriving of_yojson]

    type unsigned_data = { invite_room_state : stripped_state list option
                         } [@@deriving of_yojson]

    type t = { avatar_url         : string option
             ; displayname        : string option
             ; membership         : membership
             ; is_direct          : bool option
             ; third_party_invite : invite option
             ; unsigned           : unsigned_data option
             } [@@deriving of_yojson]
  end

  module CanonicalAlias = struct
    type t = { alias       : string option
             ; alt_aliases : string list option
             } [@@deriving of_yojson]
  end

  module Name = struct
    type t = { name : string } [@@deriving of_yojson]
  end

  module Topic = struct
    type t = { topic : string } [@@deriving of_yojson]
  end

  module Avatar = struct
    type t = { info : image_info option
             ; url  : string
             } [@@deriving of_yojson]
  end

  module PowerLevels = struct
    type notifications = { room : int option } [@@deriving of_yojson]

    type t = { ban            : int option
             ; events         : (string * int) list option (* make it a map? *)
             ; events_default : int option
             ; invite         : int option
             ; kick           : int option
             ; redact         : int option
             ; state_default  : int option
             ; users          : (string * int) list option (* make it a map? *)
             ; users_default  : int option
             ; notifications  : notifications option
             } [@@deriving of_yojson]
  end

  module PinnedEvents = struct
    type t = { pinned : string list } [@@deriving of_yojson]
  end

  module Encryption = struct
    (* algorithm is an enum that must be 'm.megolm.v1.aes-sha2' *)
    type t = { algorithm            : string
             ; rotation_period_ms   : int option
             ; rotation_period_msgs : int option
             } [@@deriving of_yojson]
  end

  module Redaction = struct
    type t = { reason : string option } [@@deriving of_yojson]
  end

  module Encrypted = struct
    (* algorithm is an enum that must be 'm.olm.v1.curve25519-aes-sha2' or
     * 'm.megolm.v1.aes-sha2' *)
    type ciphertext_info = { body     : string option
                           ; olm_type : int option
                           }

    let ciphertext_info_of_yojson j =
      let body = U.member "body" j |> U.to_string_option in
      let olm_type = U.member "type" j |> U.to_int_option in
      Result.return { body; olm_type }

    type ciphertext =
      | Cipher of string
      | CipherMap of (string * ciphertext_info) list

    (* TODO: add check that algorithm is in allowed set / type encode algos *)
    let ciphertext_of_yojson = function
      | `String s -> Result.return (Cipher s)
      | `Assoc l  ->
        List.map ~f:(fun (s, a) -> (s, ciphertext_info_of_yojson a)) l
        |> List.fold_result ~init:[] ~f:begin fun acc (s, r) ->
          match r with
          | Ok i -> Result.return ((s, i) :: acc)
          | Error _ as err -> err
        end
        |> Result.map ~f:(fun cm -> CipherMap cm)
      | _         -> Result.fail "Invalid ciphertext json."

    type t = { algorithm  : string
             ; ciphertext : ciphertext
             ; sender_key : string
             ; device_id  : string option
             ; session_id : string option
             } [@@deriving of_yojson]
  end

  module Tombstone = struct
    type t = { body             : string
             ; replacement_room : string
             } [@@deriving of_yojson]
  end

  module Content = struct
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
      | Tombstone of Tombstone.t

    let message r            = Message r
    let create r             = Create r
    let guest_access r       = GuestAccess r
    let join_rules r         = JoinRules r
    let history_visibility r = HistoryVisibility r
    let member r             = Member r
    let canonical_alias r    = CanonicalAlias r
    let name r               = Name r
    let topic r              = Topic r
    let avatar r             = Avatar r
    let power_levels r       = PowerLevels r
    let pinned_events r      = PinnedEvents r
    let encryption r         = Encryption r
    let redaction r          = Redaction r
    let encrypted r          = Encrypted r
    let tombstone r          = Tombstone r

    let of_yojson m_type c =
      let open Result in
      match m_type with
      | "m.message"            -> Message.of_yojson c           >>| message
      | "m.create"             -> Create.of_yojson c            >>| create
      | "m.guest_access"       -> GuestAccess.of_yojson c       >>| guest_access
      | "m.join_rules"         -> JoinRules.of_yojson c         >>| join_rules
      | "m.history_visibility" -> HistoryVisibility.of_yojson c >>| history_visibility
      | "m.member"             -> Member.of_yojson c            >>| member
      | "m.canonical_alias"    -> CanonicalAlias.of_yojson c    >>| canonical_alias
      | "m.name"               -> Name.of_yojson c              >>| name
      | "m.topic"              -> Topic.of_yojson c             >>| topic
      | "m.avatar"             -> Avatar.of_yojson c            >>| avatar
      | "m.power_levels"       -> PowerLevels.of_yojson c       >>| power_levels
      | "m.pinned_events"      -> PinnedEvents.of_yojson c      >>| pinned_events
      | "m.encryption"         -> Encryption.of_yojson c        >>| encryption
      | "m.redaction"          -> Redaction.of_yojson c         >>| redaction
      | "m.encrypted"          -> Encrypted.of_yojson c         >>| encrypted
      | "m.tombstone"          -> Tombstone.of_yojson c         >>| tombstone
      | m                      -> Result.fail ("Unknown matrix type: " ^ m)
  end

  type event_content =
    | Room of { content : Content.t }
    | State of { content : Content.t; prev_content : Content.t option }

  module Common = struct
    type unsigned = { age : int
                    ; redacted_because : string option
                    } [@@deriving of_yojson]

    type t = { m_type           : string [@key "type"]
             ; event_id         : string
             ; sender           : string
             ; origin_server_ts : int
             ; unsigned         : unsigned option
             ; room_id          : string
             ; state_key        : string option
             } [@@deriving of_yojson]
  end

  type t = Common.t * event_content

  let of_yojson j =
    let open Result in
    Common.of_yojson j >>= fun com ->
    let content = U.member "content" j |> Content.of_yojson com.m_type in
    if Option.is_none com.state_key then
      let prev_content = U.member "prev_content" j
                         |> Content.of_yojson com.m_type
                         |> Result.ok in
      content >>| fun c -> com, State { content = c; prev_content }
    else
      content >>| fun c -> com, Room { content = c }
end

module Call = struct
  module Invite = struct
    type session_type = Offer

    let session_type_of_yojson = function
      | `String "offer" -> Result.return Offer
      | `String s       -> Result.fail ("Type of session was not offer: " ^ s)
      | _               -> Result.fail "Type of session missing / not a string."

    type offer = { session_type : session_type [@key "type"]
                 ; sdp          : string
                 } [@@deriving of_yojson]

    type t = { call_id : string
             ; offer : offer
             ; version : int
             ; lifetime : int
             } [@@deriving of_yojson]
  end

  module Candidates = struct
    type candidate = { sdpMid        : string
                     ; sdpMLineIndex : int
                     ; candidate     : string
                     } [@@deriving of_yojson]

    type t = { call_id    : string
             ; candidates : candidate list
             ; version    : int
             } [@@deriving of_yojson]
  end

  module Answer = struct
    type session_type = Answer

    let session_type_of_yojson = function
      | `String "answer" -> Result.return Answer
      | `String s       -> Result.fail ("Type of session was not answer: " ^ s)
      | _               -> Result.fail "Type of session missing / not a string."

    type answer = { session_type : session_type [@key "type"]
                  ; sdp          : string
                  } [@@deriving of_yojson]

    type t = { call_id : string
             ; answer  : answer
             ; version : int
             } [@@deriving of_yojson]
  end

  module Hangup = struct
    type reason = IceFailed | InviteTimeout

    let reason_of_yojson = function
      | `String "ice_failed"     -> Result.return IceFailed
      | `String "invite_timeout" -> Result.return InviteTimeout
      | `String s -> Result.fail ("Reason not a valid enum value: " ^ s)
      | _         -> Result.fail "Reason was field not a string."

    type t = { call_id : string
             ; version : int
             ; reason : reason option
             } [@@deriving of_yojson]
  end
end
