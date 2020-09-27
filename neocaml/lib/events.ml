open Base
(* open Neo_infix *)
open Yojson_helpers

type 'a string_map = (string, 'a, String.comparator_witness) Map.t

module JsonWebKey = struct
  type t = { kty     : string
           ; key_ops : string list
           ; alg     : string
           ; k       : string
           ; ext     : bool
           } [@@deriving of_yojson]
end

module EncryptedFile = struct
  type hashes_map = string string_map

  let hashes_map_of_yojson = string_map_of_yojson string_of_yojson

  type t = { url    : string
           ; key    : JsonWebKey.t
           ; iv     : string
           ; hashes : hashes_map
           ; v      : string  (* must be = "v2" *)
           } [@@deriving of_yojson]
end

module ThumbnailInfo = struct
  type t = { h        : int option    [@default None]
           ; w        : int option    [@default None]
           ; mimetype : string option [@default None]
           ; size     : int option    [@default None]
           } [@@deriving of_yojson]
end

module ImageInfo = struct
  type t = { h              : int option             [@default None]
           ; w              : int option             [@default None]
           ; mimetype       : string option          [@default None]
           ; size           : int option             [@default None]
           ; thumbnail_info : ThumbnailInfo.t option [@default None]
           ; thumbnail_url  : string option          [@default None]
           ; thumbnail_file : EncryptedFile.t option [@default None]
           } [@@deriving of_yojson]
end

module rec Room : sig
  module Message : sig
    module Text : sig
      type in_reply = { event_id : string; }
      type relates = { in_reply_to : in_reply option; }
      type t = { body           : string
               ; format         : string option
               ; formatted_body : string option
               ; msgtype        : string
               ; relates_to     : relates option
               }
      val in_reply_of_yojson :
        Yojson.Safe.t -> in_reply Ppx_deriving_yojson_runtime.error_or
      val relates_of_yojson :
        Yojson.Safe.t -> relates Ppx_deriving_yojson_runtime.error_or
      val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
    end

    module Emote : sig
      type t = {
        body : string;
        format : string option;
        formatted_body : string option;
        msgtype : string;
      }
      val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
    end

    module Notice : sig
      type t = {
        body : string;
        format : string option;
        formatted_body : string option;
        msgtype : string;
      }
      val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
    end

    module Image : sig
      type t = {
        body : string;
        info : ImageInfo.t option;
        url : string option;
        file : EncryptedFile.t option;
        msgtype : string;
      }
      val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
    end

    module File : sig
      type info = {
        mimetype : string option;
        size : int option;
        thumbnail_url : string option;
        thumbnail_file : EncryptedFile.t option;
        thumbnail_info : ThumbnailInfo.t option;
      }
      type t = {
        body : string;
        filename : string option;
        info : info option;
        url : string option;
        file : EncryptedFile.t option;
        msgtype : string;
      }

      val info_of_yojson :
        Yojson.Safe.t -> info Ppx_deriving_yojson_runtime.error_or
      val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
    end

    module Audio : sig
      type info = {
        duration : int option;
        mimetype : string option;
        size : int option;
      }
      type t = {
        body : string;
        info : info option;
        url : string option;
        file : EncryptedFile.t option;
        msgtype : string;
      }

      val info_of_yojson :
        Yojson.Safe.t -> info Ppx_deriving_yojson_runtime.error_or
      val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
    end

    module Location : sig
      type info = {
        thumbnail_url : string option;
        thumbnail_file : EncryptedFile.t option;
        thumbnail_info : ThumbnailInfo.t option;
      }
      type t = {
        body : string;
        geo_uri : string;
        info : info option;
        msgtype : string;
      }

      val info_of_yojson :
        Yojson.Safe.t -> info Ppx_deriving_yojson_runtime.error_or
      val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
    end

    module Video : sig
      type info = {
        duration : int option;
        h : int option;
        w : int option;
        mimetype : string option;
        size : int option;
        thumbnail_url : string option;
        thumbnail_file : EncryptedFile.t option;
        thumbnail_info : ThumbnailInfo.t option;
      }
      type t = {
        body : string;
        info : info option;
        url : string option;
        file : EncryptedFile.t option;
        msgtype : string;
      }
      val info_of_yojson :
        Yojson.Safe.t -> info Ppx_deriving_yojson_runtime.error_or
      val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
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
      | Unknown of Yojson.Safe.t
    val to_mtype  : t -> string
    val text      : Text.t -> t
    val emote     : Emote.t -> t
    val notice    : Notice.t -> t
    val image     : Image.t -> t
    val file      : File.t -> t
    val audio     : Audio.t -> t
    val location  : Location.t -> t
    val video     : Video.t -> t
    val unknown   : Yojson.Safe.t -> t
    val of_yojson : Yojson.Safe.t -> (t, string) Result.t
  end

  module Create : sig
    type previous_room = {
      room_id : string;
      event_id : string;
    }
    type t = {
      creator : string;
      federate : bool option;
      room_version : string option;
      predecessor : previous_room option;
    }

    val previous_room_of_yojson :
      Yojson.Safe.t -> previous_room Ppx_deriving_yojson_runtime.error_or
    val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
  end

  module GuestAccess : sig
    type access = CanJoin | Forbidden
    type t = { guest_access : access }
    val access_of_yojson : Yojson.Safe.t -> (access, String.t) Result.t
    val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
  end

  module JoinRules : sig
    type t = { join_rule : string }
    val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
  end

  module HistoryVisibility : sig
    type visibility = Invited | Joined | Shared | WorldReadable
    val visibility_of_yojson : Yojson.Safe.t -> (visibility, String.t) Result.t
    type t = { history_visibility : visibility option; }
    val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
  end

  module Member : sig
    type membership = Invite | Join | Knock | Leave | Ban
    type signatures = string string_map string_map
    type signed = { mxid       : string
                  ; signatures : signatures
                  ; token      : string
                  }
    type invite = { display_name : string; signed : signed; }
    type t = { avatar_url         : string option
             ; displayname        : string option
             ; inviter            : string option
             ; membership         : membership
             ; is_direct          : bool option
             ; third_party_invite : invite option
             ; invite_room_state  : Room.t list option
             }
    val membership_of_yojson : Yojson.Safe.t -> (membership, String.t) Result.t
    val signatures_of_yojson :
      Yojson.Safe.t -> (string string_map string_map, string) Result.t
    val signed_of_yojson :
      Yojson.Safe.t -> signed Ppx_deriving_yojson_runtime.error_or
    val invite_of_yojson :
      Yojson.Safe.t -> invite Ppx_deriving_yojson_runtime.error_or
    val of_yojson :
      Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
  end

  module CanonicalAlias : sig
    type t = {
      alias : string option;
      alt_aliases : string list option;
    }
    val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
  end

  module Name : sig
    type t = { name : string; }
    val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
  end

  module Topic : sig
    type t = { topic : string; }
    val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
  end

  module Avatar : sig
    type t = { info : ImageInfo.t option; url : string; }
    val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
  end

  module PowerLevels : sig
    type notifications = { room : int option; }
    type int_string_map = int string_map
    type t = {
      ban : int option;
      events : int_string_map option;
      events_default : int option;
      invite : int option;
      kick : int option;
      redact : int option;
      state_default : int option;
      users : int_string_map option;
      users_default : int option;
      notifications : notifications option;
    }
    val notifications_of_yojson :
      Yojson.Safe.t -> notifications Ppx_deriving_yojson_runtime.error_or
    val int_string_map_of_yojson :
      Yojson.Safe.t -> (int string_map, string) Result.t
    val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
  end

  module PinnedEvents : sig
    type t = { pinned : string list; }
    val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
  end

  module Encryption : sig
    type t = {
      algorithm : string;
      rotation_period_ms : int option;
      rotation_period_msgs : int option;
    }
    val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
  end

  module Redaction : sig
    type t = { reason : string option; }
    val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
  end

  module Encrypted : sig
    type ciphertext_info = { body : string option
                           ; olm_type : int option
                           }
    type cipher_map = ciphertext_info string_map
    type ciphertext = Cipher of string | CipherMap of cipher_map
    type t = { algorithm  : string
             ; ciphertext : ciphertext
             ; sender_key : string
             ; device_id  : string option
             ; session_id : string option
             }
    val ciphertext_info_of_yojson : Yojson.Safe.t -> (ciphertext_info, 'a) Result.t
    val cipher_map_of_yojson :
      Yojson.Safe.t -> (ciphertext_info string_map, string) Result.t
    val ciphertext_of_yojson : Yojson.Safe.t -> (ciphertext, string) Result.t
    val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
  end

  module Tombstone : sig
    type t = { body : string; replacement_room : string; }
    val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
  end

  module Sticker : sig
    type t = { body : string
             ; info : ImageInfo.t
             ; url : string
             }
    val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
  end

  module Widgets : sig
    type data = { widgetSessionId : string; }
    type t = { name   : string
             ; m_type : string
             ; url    : string
             ; data   : data
             }
    val data_of_yojson : Yojson.Safe.t -> data Ppx_deriving_yojson_runtime.error_or
    val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
  end

  module PreviewUrls : sig
    type t = { disable : bool; }
    val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
  end

  module Content : sig
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
      | Sticker of Sticker.t
      | Widgets of Widgets.t
      | PreviewUrls of PreviewUrls.t

    val message            : Message.t -> t
    val create             : Create.t -> t
    val guest_access       : GuestAccess.t -> t
    val join_rules         : JoinRules.t -> t
    val history_visibility : HistoryVisibility.t -> t
    val member             : Member.t -> t
    val canonical_alias    : CanonicalAlias.t -> t
    val name               : Name.t -> t
    val topic              : Topic.t -> t
    val avatar             : Avatar.t -> t
    val power_levels       : PowerLevels.t -> t
    val pinned_events      : PinnedEvents.t -> t
    val encryption         : Encryption.t -> t
    val redaction          : Redaction.t -> t
    val encrypted          : Encrypted.t -> t
    val tombstone          : Tombstone.t -> t
    val sticker            : Sticker.t -> t
    val widgets            : Widgets.t -> t
    val preview_urls       : PreviewUrls.t -> t

    val of_yojson : String.t -> Yojson.Safe.t -> (t, String.t) Result.t
  end

  type event_content =
    | Event of { content : Content.t }
    | State of { content : Content.t; prev_content : Content.t option }

  module Common : sig
    type unsigned = { age              : int option
                    ; redacted_because : string option
                    ; transaction_id   : string option
                    ; replaces_state   : string option
                    ; prev_sender      : string option
                    }
    type t = { m_type           : string
             ; event_id         : string
             ; sender           : string
             ; origin_server_ts : int
             ; unsigned         : unsigned option
             ; room_id          : string option
             ; state_key        : string option
             }
    val unsigned_keys : unit -> (String.t, String.comparator_witness) Set.t
    val uncommon_keys : unit -> string list
    val unsigned_of_yojson :
      Yojson.Safe.t -> unsigned Ppx_deriving_yojson_runtime.error_or
    val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
  end

  type t = Common.t * event_content

  val extra_unsigned : Yojson.Safe.t -> Yojson.Safe.t
  val uncommon_unsigned : Yojson.Safe.t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> (Common.t * event_content, String.t) Result.t
end = struct
  module Message = struct
    module Text = struct
      type in_reply = { event_id : string } [@@deriving of_yojson]

      type relates =
        { in_reply_to : in_reply option [@key "m.in_reply_to"] [@default None]
        } [@@deriving of_yojson]

      type t =
        { body           : string
        ; format         : string option [@default None]
        ; formatted_body : string option [@default None]
        ; msgtype        : string
        ; relates_to     : relates option [@key "m.relates_to"] [@default None]
        } [@@deriving of_yojson]
    end

    module Emote = struct
      type t = { body           : string
               ; format         : string option [@default None]
               ; formatted_body : string option [@default None]
               ; msgtype        : string
               } [@@deriving of_yojson]
    end

    module Notice = struct
      type t = { body           : string
               ; format         : string option [@default None]
               ; formatted_body : string option [@default None]
               ; msgtype        : string
               } [@@deriving of_yojson]
    end

    module Image = struct
      (* NOTE: url is required if unencrypted, file is required if encrypted. *)
      type t = { body    : string
               ; info    : ImageInfo.t option     [@default None]
               ; url     : string option          [@default None]
               ; file    : EncryptedFile.t option [@default None]
               ; msgtype : string
               } [@@deriving of_yojson]
    end

    module File = struct
      type info = { mimetype : string option                [@default None]
                  ; size     : int option                   [@default None]
                  ; thumbnail_url : string option           [@default None]
                  ; thumbnail_file : EncryptedFile.t option [@default None]
                  ; thumbnail_info : ThumbnailInfo.t option [@default None]
                  } [@@deriving of_yojson]

      type t = { body     : string
               ; filename : string option          [@default None]
               ; info     : info option            [@default None]
               ; url      : string option          [@default None]
               ; file     : EncryptedFile.t option [@default None]
               ; msgtype  : string
               } [@@deriving of_yojson]
    end

    module Audio = struct
      type info = { duration : int option    [@default None]
                  ; mimetype : string option [@default None]
                  ; size     : int option    [@default None]
                  } [@@deriving of_yojson]

      type t = { body     : string
               ; info     : info option            [@default None]
               ; url      : string option          [@default None]
               ; file     : EncryptedFile.t option [@default None]
               ; msgtype  : string
               } [@@deriving of_yojson]
    end

    module Location = struct
      type info = { thumbnail_url  : string option          [@default None]
                  ; thumbnail_file : EncryptedFile.t option [@default None]
                  ; thumbnail_info : ThumbnailInfo.t option [@default None]
                  } [@@deriving of_yojson]

      type t = { body     : string
               ; geo_uri  : string
               ; info     : info option [@default None]
               ; msgtype  : string
               } [@@deriving of_yojson]
    end

    module Video = struct
      type info = { duration       : int option             [@default None]
                  ; h              : int option             [@default None]
                  ; w              : int option             [@default None]
                  ; mimetype       : string option          [@default None]
                  ; size           : int option             [@default None]
                  ; thumbnail_url  : string option          [@default None]
                  ; thumbnail_file : EncryptedFile.t option [@default None]
                  ; thumbnail_info : ThumbnailInfo.t option [@default None]
                  } [@@deriving of_yojson]

      (* NOTE: url or file is required depending on encryption. *)
      type t = { body    : string
               ; info    : info option            [@default None]
               ; url     : string option          [@default None]
               ; file    : EncryptedFile.t option [@default None]
               ; msgtype : string
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
      | Unknown of Yojson.Safe.t

    let to_mtype = function
      | Text     _ -> "m.text"
      | Emote    _ -> "m.emote"
      | Notice   _ -> "m.notice"
      | Image    _ -> "m.image"
      | File     _ -> "m.file"
      | Audio    _ -> "m.audio"
      | Location _ -> "m.location"
      | Video    _ -> "m.video"
      | Unknown  _ -> "unknown message type"

    let text m     = Text m
    let emote m    = Emote m
    let notice m   = Notice m
    let image m    = Image m
    let file m     = File m
    let audio m    = Audio m
    let location m = Location m
    let video m    = Video m
    let unknown m  = Unknown m

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
        | _            -> Result.return content      |> Result.map ~f:unknown
      end
      |> Option.value ~default:(Result.fail "Missing msgtype.")
  end

  module Create = struct
    type previous_room = { room_id  : string
                         ; event_id : string
                         } [@@deriving of_yojson]

    type t = { creator      : string
             ; federate     : bool option          [@default None]
             ; room_version : string option        [@default None]
             ; predecessor  : previous_room option [@default None]
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

    type t = { history_visibility : visibility option [@default None] }
    [@@deriving of_yojson]
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
     * See: https://matrix.org/docs/spec/appendices#signing-json *)
    type signatures = (string string_map) string_map

    let signatures_of_yojson =
      string_map_of_yojson (string_map_of_yojson string_of_yojson)

    type signed = { mxid       : string
                  ; signatures : signatures
                  ; token      : string
                  } [@@deriving of_yojson]

    type invite = { display_name : string
                  ; signed       : signed
                  } [@@deriving of_yojson]

    (* FIXME: invite_room_state : stripped_state list *)
    type t = { avatar_url         : string option        [@default None]
             ; displayname        : string option        [@default None]
             ; inviter            : string option        [@default None]
             ; membership         : membership
             ; is_direct          : bool option          [@default None]
             ; third_party_invite : invite option        [@default None]
             ; invite_room_state  : Room.t list option   [@default None]
             } [@@deriving of_yojson]
  end

  module CanonicalAlias = struct
    type t = { alias       : string option      [@default None]
             ; alt_aliases : string list option [@default None]
             } [@@deriving of_yojson]
  end

  module Name = struct
    type t = { name : string } [@@deriving of_yojson]
  end

  module Topic = struct
    type t = { topic : string } [@@deriving of_yojson]
  end

  module Avatar = struct
    type t = { info : ImageInfo.t option [@default None]
             ; url  : string
             } [@@deriving of_yojson]
  end

  module PowerLevels = struct
    type notifications =
      { room : int option [@default None] } [@@deriving of_yojson]

    type int_string_map = int string_map

    let int_string_map_of_yojson = string_map_of_yojson int_of_yojson

    type t = { ban            : int option            [@default None]
             ; events         : int_string_map option [@default None]
             ; events_default : int option            [@default None]
             ; invite         : int option            [@default None]
             ; kick           : int option            [@default None]
             ; redact         : int option            [@default None]
             ; state_default  : int option            [@default None]
             ; users          : int_string_map option [@default None]
             ; users_default  : int option            [@default None]
             ; notifications  : notifications option  [@default None]
             } [@@deriving of_yojson]
  end

  module PinnedEvents = struct
    type t = { pinned : string list } [@@deriving of_yojson]
  end

  module Encryption = struct
    (* algorithm is an enum that must be 'm.megolm.v1.aes-sha2' *)
    type t = { algorithm            : string
             ; rotation_period_ms   : int option [@default None]
             ; rotation_period_msgs : int option [@default None]
             } [@@deriving of_yojson]
  end

  module Redaction = struct
    type t = { reason : string option [@default None] } [@@deriving of_yojson]
  end

  module Encrypted = struct
    (* algorithm is an enum that must be 'm.olm.v1.curve25519-aes-sha2' or
     * 'm.megolm.v1.aes-sha2' *)
    type ciphertext_info = { body     : string option [@default None]
                           ; olm_type : int option    [@default None]
                           }

    let ciphertext_info_of_yojson j =
      let body = U.member "body" j |> U.to_string_option in
      let olm_type = U.member "type" j |> U.to_int_option in
      Result.return { body; olm_type }

    type cipher_map = ciphertext_info string_map

    let cipher_map_of_yojson = string_map_of_yojson ciphertext_info_of_yojson

    type ciphertext =
      | Cipher of string
      | CipherMap of cipher_map

    (* TODO: add check that algorithm is in allowed set / type encode algos *)
    let ciphertext_of_yojson = function
      | `String s         -> Result.return (Cipher s)
      | `Assoc _ as assoc -> cipher_map_of_yojson assoc
                             |> Result.map ~f:(fun cm -> CipherMap cm)
      | _         -> Result.fail "Invalid ciphertext json."

    type t = { algorithm  : string
             ; ciphertext : ciphertext
             ; sender_key : string
             ; device_id  : string option [@default None]
             ; session_id : string option [@default None]
             } [@@deriving of_yojson]
  end

  module Tombstone = struct
    type t = { body             : string
             ; replacement_room : string
             } [@@deriving of_yojson]
  end

  module Sticker = struct
    type t = { body : string
             ; info : ImageInfo.t
             ; url  : string
             } [@@deriving of_yojson]
  end

  module Widgets = struct
    (* NOTE: Very rough, the only example I have seen (which this is based on)
     * is Jitsi. I don't know what other members might be present / whether
     * these are optional. Don't make unstrict so I can catch the extras. *)
    type data = { widgetSessionId : string } [@@deriving of_yojson]

    type t = { name   : string
             ; m_type : string [@key "type"]
             ; url    : string
             ; data   : data
             } [@@deriving of_yojson]
  end

  module PreviewUrls = struct
    type t = { disable : bool } [@@deriving of_yojson]
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
      | Sticker of Sticker.t
      | Widgets of Widgets.t
      | PreviewUrls of PreviewUrls.t

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
    let sticker r            = Sticker r
    let widgets r            = Widgets r
    let preview_urls r       = PreviewUrls r

    let of_yojson m_type c =
      let open Result in
      match m_type with
      | "m.room.message"               -> Message.of_yojson c           >>| message
      | "m.room.create"                -> Create.of_yojson c            >>| create
      | "m.room.guest_access"          -> GuestAccess.of_yojson c       >>| guest_access
      | "m.room.join_rules"            -> JoinRules.of_yojson c         >>| join_rules
      | "m.room.history_visibility"    -> HistoryVisibility.of_yojson c >>| history_visibility
      | "m.room.member"                -> Member.of_yojson c            >>| member
      | "m.room.canonical_alias"       -> CanonicalAlias.of_yojson c    >>| canonical_alias
      | "m.room.name"                  -> Name.of_yojson c              >>| name
      | "m.room.topic"                 -> Topic.of_yojson c             >>| topic
      | "m.room.avatar"                -> Avatar.of_yojson c            >>| avatar
      | "m.room.power_levels"          -> PowerLevels.of_yojson c       >>| power_levels
      | "m.room.pinned_events"         -> PinnedEvents.of_yojson c      >>| pinned_events
      | "m.room.encryption"            -> Encryption.of_yojson c        >>| encryption
      | "m.room.redaction"             -> Redaction.of_yojson c         >>| redaction
      | "m.room.encrypted"             -> Encrypted.of_yojson c         >>| encrypted
      | "m.room.tombstone"             -> Tombstone.of_yojson c         >>| tombstone
      | "m.sticker"                    -> Sticker.of_yojson c           >>| sticker
      | "im.vector.modular.widgets"    -> Widgets.of_yojson c           >>| widgets
      | "org.matrix.room.preview_urls" -> PreviewUrls.of_yojson c       >>| preview_urls
      | m                              -> Result.fail ("Unknown matrix type: " ^ m)
  end

  type event_content =
    | Event of { content : Content.t }
    | State of { content : Content.t; prev_content : Content.t option }

  module Common = struct
    (* NOTE: redacted_because amd transaction_id seem to be common for room/timeline
     * events, though they aren't the only possible optional fields. *)
    type unsigned = { age              : int option    [@default None]
                    ; redacted_because : string option [@default None]
                    ; transaction_id   : string option [@default None]
                    ; replaces_state   : string option [@default None]
                    ; prev_sender      : string option [@default None]
                    } [@@deriving of_yojson { strict = false }]

    let unsigned_keys () = [ "age"
                           ; "redacted_because"
                           ; "transaction_id"
                           ; "replaces_state"
                           ; "prev_sender"
                           ; "prev_content" (* NOTE: Undecided on state event impl *)
                           ] |> Set.of_list (module String)

    (* NOTE: Tracking the keys I know of that aren't in unsigned_keys, if this
     * does not grow much, target these specifically rather than the even
     * hackier approach used in `extra_unsigned` below. *)
    let uncommon_keys () = [ "invite_room_state" ]

    type t = { m_type           : string [@key "type"]
             ; event_id         : string
             ; sender           : string
             ; origin_server_ts : int
             ; unsigned         : unsigned option [@default None]
             ; room_id          : string option   [@default None]
             ; state_key        : string option   [@default None]
             } [@@deriving of_yojson { strict = false }]
  end

  type t = Common.t * event_content

  (* NOTE: Bit of a hacky solution to the special unsigned_data fields issue.
   * Right now the only one I know of is invite_room_state for Member events.
   * This will make sure I don't miss any while I am working things out. *)
  let extra_unsigned j =
    Yojson.Safe.Util.keys j
    |> Set.of_list (module String)
    |> (fun s -> Set.diff s (Common.unsigned_keys ()))
    |> Set.to_list
    |> List.map ~f:(fun k -> [ (k, Yojson.Safe.Util.member k j) ] |> yo_assoc)
    |> List.fold ~init:(`Assoc []) ~f:U.combine

  (* Alternative implementation to extra_unsigned that targets particular keys
   * that only show up in unsigned alongside particular events. This is
   * obviously faster, should switch to this once I know what all the uncommon
   * ones are. Also, consider treating prev_convent like this rather than as a
   * special record field alongside content. *)
  let uncommon_unsigned j =
    Common.uncommon_keys ()
    |> List.map ~f:(fun k -> [ (k, Yojson.Safe.Util.member k j) ] |> yo_assoc)
    |> List.fold ~init:(`Assoc []) ~f:U.combine

  let of_yojson j =
    let open Result in
    Common.of_yojson j >>= fun com ->
    let unsigned = U.member "unsigned" j |> extra_unsigned in
    let content = U.member "content" j
                  |> U.combine unsigned
                  |> Content.of_yojson com.m_type in
    if Option.is_some com.state_key then
      let prev_content = U.member "prev_content" j
                         |> Content.of_yojson com.m_type
                         |> Result.ok in
      content >>| fun c -> com, State { content = c; prev_content }
    else
      content >>| fun c -> com, Event { content = c }
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
      | `String s        -> Result.fail ("Type of session was not answer: " ^ s)
      | _                -> Result.fail "Type of session missing / not string."

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
      | _         -> Result.fail "Reason field was not a string."

    type t = { call_id : string
             ; version : int
             ; reason : reason option [@default None]
             } [@@deriving of_yojson]
  end

  module Content = struct
    type t =
      | Invite of Invite.t
      | Candidates of Candidates.t
      | Answer of Answer.t
      | Hangup of Hangup.t

    let invite m     = Invite m
    let candidates m = Candidates m
    let answer m     = Answer m
    let hangup m     = Hangup m

    let of_yojson m_type c =
      match m_type with
      | "m.call.invite"     -> Invite.of_yojson c     |> Result.map ~f:invite
      | "m.call.candidates" -> Candidates.of_yojson c |> Result.map ~f:candidates
      | "m.call.answer"     -> Answer.of_yojson c     |> Result.map ~f:answer
      | "m.call.hangup"     -> Hangup.of_yojson c     |> Result.map ~f:hangup
      | _                   -> Result.fail "Unknown call event type."

  end

  module Common = struct
    type unsigned = { age              : int option    [@default None]
                    ; redacted_because : string option [@default None]
                    ; transaction_id   : string option [@default None]
                    } [@@deriving of_yojson]

    type t = { m_type           : string [@key "type"]
             ; event_id         : string
             ; sender           : string
             ; origin_server_ts : int
             ; unsigned         : unsigned option [@default None]
             ; room_id          : string
             } [@@deriving of_yojson { strict = false }]
  end

  type t = Common.t * Content.t

  let of_yojson j =
    let open Result in
    Common.of_yojson j >>= fun com ->
    let content = U.member "content" j |> Content.of_yojson com.m_type in
    content >>| fun c -> com, c
end

module Presence = struct
  type content = { avatar_url       : string option    [@default None]
                 ; displayname      : string option    [@default None]
                 ; last_active_ago  : int option       [@default None] (* in milliseconds *)
                 ; presence         : Types.Presence.t
                 ; currently_active : bool option      [@default None]
                 ; status_msg       : string option    [@default None]
                 } [@@deriving of_yojson]

  type t = { m_type  : string [@key "type"]
           ; sender  : string  (* indicates user that this applies to *)
           ; content : content
           } [@@deriving of_yojson]
end

module Typing = struct
  (* NOTE: An "Ephemeral" event. *)
  type content = { user_ids : string list } [@@deriving of_yojson]

  type t = { m_type  : string [@key "type"]
           ; room_id : string
           ; content : content
           } [@@deriving of_yojson]
end

module Receipt = struct
  (* NOTE: Ephemeral event. *)
  type receipt = { ts : int option [@default None] } [@@deriving of_yojson]

  type users = receipt string_map

  let users_of_yojson = string_map_of_yojson receipt_of_yojson

  type receipts =
    { read : users option [@key "m.read"] [@default None] } [@@deriving of_yojson]

  (* map from event_id to map from user_id to timestamp *)
  type content = receipts string_map

  let content_of_yojson = string_map_of_yojson receipts_of_yojson

  type t = { m_type  : string [@key "type"]
           ; room_id : string option [@default None]
           ; content : content
           } [@@deriving of_yojson]
end

module FullyRead = struct
  type content = { event_id : string } [@@deriving of_yojson]

  type t = { m_type  : string [@key "type"]
           ; room_id : string option [@default None]
           ; content : content
           } [@@deriving of_yojson]
end

module IdentityServer = struct
  type content = { base_url : string option [@default None] } [@@deriving of_yojson]

  type t = { m_type : string [@key "type"]
           ; content : content
           } [@@deriving of_yojson]
end

module Direct = struct
  (* map from user_id to list of room_ids indicating what rooms are considered
   * "direct" rooms for that user. *)
  type content = (string list) string_map

  let content_of_yojson =
    string_map_of_yojson (typed_list_of_yojson string_of_yojson)

  type t = { m_type  : string [@key "type"]
           ; content : content
           } [@@deriving of_yojson]
end

module IgnoredUserList = struct
  (* NOTE: The yojson object is empty at this time according to spec. *)
  type ignored_users = Yojson.Safe.t string_map

  let ignored_users_of_yojson = string_map_of_yojson (fun j -> Result.return j)

  type content = { ignored_users : ignored_users } [@@deriving of_yojson]

  type t = { m_type  : string [@key "type"]
           ; content : content
           } [@@deriving of_yojson]
end

module Tag = struct
  (* map from user defined tags to an order value that give the relative
   * position of the room under the given tag.
   * NOTE: Does this go in the room module since it pertains to a particular
   *  room? It's a bit unclear from what I've read so far where I should expect
   * this event to pop up. *)
  type tag = { order : float option [@default None] } [@@deriving of_yojson]

  type tag_map = tag string_map

  let tag_map_of_yojson = string_map_of_yojson tag_of_yojson

  type content = { tags : tag_map } [@@deriving of_yojson]

  type t = { m_type  : string [@key "type"]
           ; content : content
           } [@@deriving of_yojson]
end

module NewDevice = struct
  (* NOTE: Does this only ever occur in the ToDevice section of the sync
   *  response? Also what other events can actually happen in there? If they are
   * local to there only, then consider putting them (along with this) in the
   * relevant module in responses.ml rather than here. *)
  type content = { device_id : string
                 ; rooms     : string list
                 } [@@deriving of_yojson]

  type t = { m_type  : string [@key "type"]
           ; sender  : string
           ; content : content
           } [@@deriving of_yojson]
end

module PushRules = struct
  type push_condition = { kind    : string
                        ; key     : string option [@default None]
                        ; pattern : string option [@default None]
                        ; is      : string option [@default None]
                        } [@@deriving of_yojson]

  (* NOTE: I've seen string and bool for value so far. Consider change to
   *  something other than json when I have more complete picture. *)
  type action =
    | Action of string
    | Tweak of { set_tweak : string; value : Yojson.Safe.t }

  let action_of_yojson = function
    | `String s -> Action s |> Result.return
    | `Assoc _ as assoc ->
      let value = U.member "value" assoc in
      U.member "set_tweak" assoc
      |> U.to_string_option
      |> Result.of_option ~error:"Invalid push_rule action Tweak."
      |> Result.map ~f:(fun set_tweak -> Tweak { set_tweak; value })
    | _ -> Result.fail "Invalid push_rule action Tweak."

  type push_rule = { actions    : action list
                   ; default    : bool
                   ; enabled    : bool
                   ; rule_id    : string
                   ; conditions : push_condition list option [@default None]
                   ; pattern    : string option              [@default None]
                   } [@@deriving of_yojson]

  type ruleset = { content   : push_rule list option [@default None]
                 ; override  : push_rule list option [@default None]
                 ; room      : push_rule list option [@default None]
                 ; sender    : push_rule list option [@default None]
                 ; underride : push_rule list option [@default None]
                 } [@@deriving of_yojson]

  type devices = ruleset string_map

  let devices_of_yojson = string_map_of_yojson ruleset_of_yojson

  (* NOTE: device is probably a map, it's empty in the example I have. *)
  type content = { global : ruleset
                 ; device : devices option [@default None]
                 } [@@deriving of_yojson { strict = false }]

  type t = { m_type : string [@key "type"]
           ; content : content
           } [@@deriving of_yojson]
end

(* TODO: to_device events, like key_requests which are part of sync response. *)
(* TODO: To_Device module? *)

module KeyInfo = struct
  (* NOTE: Algo is enum that ust be 'm.megolm.v1.aes-sha2'. *)
  type t = { algorithm   : string
           ; room_id     : string
           ; session_id  : string
           ; session_key : string
           } [@@deriving of_yojson]
end

module RoomKey = struct
  (* NOTE: to-device event *)
  type t = { content : KeyInfo.t
           ; m_type  : string [@key "type"]
           } [@@deriving of_yojson]
end

module RoomKeyRequest = struct
  (* NOTE: to-device event *)
  type action = Request | RequestCancellation

  let action_of_yojson = function
    | `String "request"              -> Result.return Request
    | `String "request_cancellation" -> Result.return RequestCancellation
    | `String s                      -> Result.fail ("Invalid key action: " ^ s)
    | _                              -> Result.fail "Invalid key action type."

  (* body is required when action is request *)
  type content = { body                 : KeyInfo.t option [@default None]
                 ; action               : action
                 ; requesting_device_id : string
                 ; request_id           : string
                 } [@@deriving of_yojson]

  type t = { content : content
           ; m_type  : string [@key "type"]
           } [@@deriving of_yojson]
end

module ForwardedRoomKey = struct
  (* NOTE: to-device event *)
  type content = { algorithm                       : string
                 ; room_id                         : string
                 ; sender_key                      : string
                 ; session_id                      : string
                 ; session_key                     : string
                 ; sender_claimed_ed25519_key      : string
                 ; forwarding_curve25519_key_chain : string list
                 } [@@deriving of_yojson]

  type t = { content : content
           ; m_type  : string [@key "type"]
           } [@@deriving of_yojson]
end

module Dummy = struct
  (* NOTE: This event type is used to indicate new Olm sessions for end-to-end
   * encryption. Typically it is encrypted as an m.room.encrypted event, then sent
   * as a to-device event. *)
  (* NOTE: content is an emtpy object.*)
  type t = { content : Yojson.Safe.t
           ; m_type  : string [@key "type"]
           } [@@deriving of_yojson]
end

module KeyVerification = struct
  module Request = struct
    type t = { from_device    : string
             ; transaction_id : string
             ; methods        : string list
             ; timestamp      : int
             } [@@deriving of_yojson]
  end

  module Start = struct
    (* NOTE: Not sure from specs when (and if) this is used. *)
    type t = { from_device    : string
             ; transaction_id : string
             ; v_method       : string [@key "method"]
             ; next_method    : string option [@default None]
             } [@@deriving of_yojson]
  end

  module StartSAS = struct
    type t = { transaction_id                : string
             ; v_method                      : string [@key "method"]
             ; key_agreement_protocols       : string list
             ; hashes                        : string list
             ; message_authentication_codes  : string list
             ; short_authentication_string   : string list
             } [@@deriving of_yojson]
  end

  module Cancel = struct
    type t = { transaction_id : string
             ; reason         : string
             ; code           : string
             } [@@deriving of_yojson]
  end

  module Accept = struct
    (* NOTE: method must be "m.sas.v1" *)
    type t = { transaction_id              : string
             ; v_method                    : string [@key "method"]
             ; key_agreement_protocol      : string
             ; hash                        : string
             ; message_authentication_code : string
             ; short_authentication_string : string list
             ; commitment                  : string
             } [@@deriving of_yojson]
  end

  module Key = struct
    type t = { transaction_id : string
             ; key            : string
             } [@@deriving of_yojson]
  end

  module Mac = struct
    type mac = string string_map

    let mac_of_yojson = string_map_of_yojson string_of_yojson

    type t = { transaction_id : string
             ; mac            : mac
             ; keys           : string
             } [@@deriving of_yojson]
  end

  type t =
    | Request of Request.t
    | Cancel of Cancel.t
    | Start of Start.t
    | StartSAS of StartSAS.t
    | Accept of Accept.t
    | Key of Key.t
    | Mac of Mac.t

  let request e   = Request e
  let cancel e    = Cancel e
  let start e     = Start e
  let start_sas e = StartSAS e
  let accept e    = Accept e
  let key e       = Key e
  let mac e       = Mac e

  let is_sas c =
    try U.member "method" c |> U.to_string |> String.equal "m.sas.v1"
    with _ -> false

  let of_yojson j =
    let open Result in
    U.member "type" j |> string_of_yojson >>= fun m_type ->
    let c = U.member "content" j in
    match String.chop_prefix_if_exists ~prefix:"m.key.verification." m_type with
    | "request"              -> Request.of_yojson c   >>| request
    | "cancel"               -> Cancel.of_yojson c    >>| cancel
    | "start" when is_sas c  -> StartSAS.of_yojson c  >>| start_sas
    | "start"                -> Start.of_yojson c     >>| start
    | "accept" when is_sas c -> Accept.of_yojson c    >>| accept
    | "key"                  -> Key.of_yojson c       >>| key
    | "mac"                  -> Mac.of_yojson c       >>| mac
    | m                      -> Result.fail ("Unknown verification type: " ^ m)
end

type t =
  | Room of Room.t
  | Call of Call.t
  | Presence of Presence.t
  | Typing of Typing.t
  | Receipt of Receipt.t
  | FullyRead of FullyRead.t
  | IdentityServer of IdentityServer.t
  | Direct of Direct.t
  | IgnoredUserList of IgnoredUserList.t
  | Tag of Tag.t
  | NewDevice of NewDevice.t
  | PushRules of PushRules.t
  | KeyVerification of KeyVerification.t
  | Unknown of Yojson.Safe.t

let room e              = Room e
let call e              = Call e
let presence e          = Presence e
let typing e            = Typing e
let receipt e           = Receipt e
let fully_read e        = FullyRead e
let identity_server e   = IdentityServer e
let direct e            = Direct e
let ignored_user_list e = IgnoredUserList e
let tag e               = Tag e
let new_device e        = NewDevice e
let push_rules e        = PushRules e
let key_verification e  = KeyVerification e
let unknown e           = Unknown e

let is_room_type m =
  String.is_prefix m ~prefix:"m.room."
  || String.equal m "m.sticker"
  || String.equal m "im.vector.modular.widgets"
  || String.equal m "org.matrix.room.preview_urls"

let is_call_type m = String.is_prefix m ~prefix:"m.call."

let is_key_veri m = String.is_prefix m ~prefix:"m.key.verification."

let of_yojson j =
  let open Result in
  match U.member "type" j |> U.to_string_option with
  | Some m when is_room_type m -> Room.of_yojson j            >>| room
  | Some m when is_call_type m -> Call.of_yojson j            >>| call
  | Some m when is_key_veri m  -> KeyVerification.of_yojson j >>| key_verification
  | Some "m.presence"          -> Presence.of_yojson j        >>| presence
  | Some "m.typing"            -> Typing.of_yojson j          >>| typing
  | Some "m.receipt"           -> Receipt.of_yojson j         >>| receipt
  | Some "m.fully_read"        -> FullyRead.of_yojson j       >>| fully_read
  | Some "m.identity_server"   -> IdentityServer.of_yojson j  >>| identity_server
  | Some "m.direct"            -> Direct.of_yojson j          >>| direct
  | Some "m.ignored_user_list" -> IgnoredUserList.of_yojson j >>| ignored_user_list
  | Some "m.tag"               -> Tag.of_yojson j             >>| tag
  | Some "m.new_device"        -> NewDevice.of_yojson j       >>| new_device
  | Some "m.push_rules"        -> PushRules.of_yojson j       >>| push_rules
  (* | Some s                     -> Result.fail ("Invalid event type: " ^ s) *)
  | Some _                     -> Result.return j             >>| unknown
  | None                       -> Result.fail "Missing event type field."
