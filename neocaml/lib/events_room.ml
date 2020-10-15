open Base
open Yojson_helpers

module rec Room : sig
  module EncryptedFile : sig
    type json_web_key = { kty     : string
                        ; key_ops : string list
                        ; alg     : string
                        ; k       : string
                        ; ext     : bool
                        }
    type hashes_map = string StringMap.t
    type t = { url    : string
             ; key    : json_web_key
             ; iv     : string
             ; hashes : hashes_map
             ; v      : string
             }
    include DerivingYojson with type t := t
  end

  module ThumbnailInfo : sig
    type t = { h        : int option
             ; w        : int option
             ; mimetype : string option
             ; size     : int option
             }
    include DerivingYojson with type t := t
    val create : ?h:int -> ?w:int -> ?mimetype:string -> ?size:int -> unit -> t
  end

  module ImageInfo : sig
    type t = { h              : int option
             ; w              : int option
             ; mimetype       : string option
             ; size           : int option
             ; thumbnail_info : ThumbnailInfo.t option
             ; thumbnail_url  : string option
             ; thumbnail_file : EncryptedFile.t option
             }
    include DerivingYojson with type t := t
    val create
      :  ?h:int
      -> ?w:int
      -> ?mimetype:string
      -> ?size:int
      -> ?thumbnail_info:ThumbnailInfo.t
      -> ?thumbnail_url:string
      -> ?thumbnail_file:EncryptedFile.t
      -> unit
      -> t
  end

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
      include DerivingYojson with type t := t
      val create
        :  ?format:string
        -> ?formatted_body:string
        -> ?relates_to:relates
        -> string
        -> t
      val create_msg
        :  ?format:string
        -> ?formatted_body:string
        -> ?relates_to:relates
        -> string
        -> Room.Message.t
    end

    module Emote : sig
      type t = { body           : string
               ; format         : string option
               ; formatted_body : string option
               ; msgtype        : string
               }
      include DerivingYojson with type t := t
      val create : ?format:string -> ?formatted_body:string -> string -> t
      val create_msg : ?format:string -> ?formatted_body:string -> string -> Room.Message.t
    end

    module Notice : sig
      type t = { body           : string
               ; format         : string option
               ; formatted_body : string option
               ; msgtype        : string
               }
      include DerivingYojson with type t := t
      val create : ?format:string -> ?formatted_body:string -> string -> t
      val create_msg : ?format:string -> ?formatted_body:string -> string -> Room.Message.t
    end

    module Image : sig
      type t = { body    : string
               ; info    : ImageInfo.t option
               ; url     : string option
               ; file    : EncryptedFile.t option
               ; msgtype : string
               }
      include DerivingYojson with type t := t
      val create
        :  ?info:ImageInfo.t
        -> ?url:string
        -> ?file:EncryptedFile.t
        -> string
        -> t
      val create_msg
        :  ?info:ImageInfo.t
        -> ?url:string
        -> ?file:EncryptedFile.t
        -> string
        -> Room.Message.t
      val no_info_msg : ?url:string -> ?file:EncryptedFile.t -> string -> Room.Message.t
    end

    module File : sig
      type info = { mimetype       : string option
                  ; size           : int option
                  ; thumbnail_url  : string option
                  ; thumbnail_file : EncryptedFile.t option
                  ; thumbnail_info : ThumbnailInfo.t option
                  }
      type t = { body     : string
               ; filename : string option
               ; info     : info option
               ; url      : string option
               ; file     : EncryptedFile.t option
               ; msgtype  : string
               }
      include DerivingYojson with type t := t
      val create_info
        :  ?mimetype:string
        -> ?size:int
        -> ?thumbnail_url:string
        -> ?thumbnail_file:EncryptedFile.t
        -> ?thumbnail_info:ThumbnailInfo.t
        -> unit
        -> info
      val create
        :  ?filename:string
        -> ?info:info
        -> ?url:string
        -> ?file:EncryptedFile.t
        -> string
        -> t
      val create_msg
        :  ?filename:string
        -> ?info:info
        -> ?url:string
        -> ?file:EncryptedFile.t
        -> string
        -> Room.Message.t
      val no_info_msg : ?url:string -> ?file:EncryptedFile.t -> string -> Room.Message.t
    end

    module Audio : sig
      type info = { duration : int option
                  ; mimetype : string option
                  ; size     : int option
                  }
      type t = { body    : string
               ; info    : info option
               ; url     : string option
               ; file    : EncryptedFile.t option
               ; msgtype : string
               }
      include DerivingYojson with type t := t
      val create_info
        :  ?duration:int
        -> ?mimetype:string
        -> ?size:int
        -> unit
        -> info
      val create
        :  ?info:info
        -> ?url:string
        -> ?file:EncryptedFile.t
        -> string
        -> t
      val create_msg
        :  ?info:info
        -> ?url:string
        -> ?file:EncryptedFile.t
        -> string
        -> Room.Message.t
      val no_info_msg : ?url:string -> ?file:EncryptedFile.t -> string -> Room.Message.t
    end

    module Location : sig
      type info = { thumbnail_url  : string option
                  ; thumbnail_file : EncryptedFile.t option
                  ; thumbnail_info : ThumbnailInfo.t option
                  }
      type t = { body    : string
               ; geo_uri : string
               ; info    : info option
               ; msgtype : string
               }
      include DerivingYojson with type t := t
      val create_info
        :  ?thumbnail_url:string
        -> ?thumbnail_file:EncryptedFile.t
        -> ?thumbnail_info:ThumbnailInfo.t
        -> unit
        -> info
      val create : ?info:info -> geo_uri:string -> string -> t
      val create_msg : ?info:info -> geo_uri:string -> string -> Room.Message.t
    end

    module Video : sig
      type info = { duration       : int option
                  ; h              : int option
                  ; w              : int option
                  ; mimetype       : string option
                  ; size           : int option
                  ; thumbnail_url  : string option
                  ; thumbnail_file : EncryptedFile.t option
                  ; thumbnail_info : ThumbnailInfo.t option
                  }
      type t = { body    : string
               ; info    : info option
               ; url     : string option
               ; file    : EncryptedFile.t option
               ; msgtype : string
               }
      include DerivingYojson with type t := t
      val create_info
        :  ?duration:int
        -> ?h:int
        -> ?w:int
        -> ?mimetype:string
        -> ?size:int
        -> ?thumbnail_url:string
        -> ?thumbnail_file:EncryptedFile.t
        -> ?thumbnail_info:ThumbnailInfo.t
        -> unit
        -> info
      val create
        :  ?info:info
        -> ?url:string
        -> ?file:EncryptedFile.t
        -> string
        -> t
      val create_msg
        :  ?info:info
        -> ?url:string
        -> ?file:EncryptedFile.t
        -> string
        -> Room.Message.t
      val no_info_msg : ?url:string -> ?file:EncryptedFile.t -> string -> Room.Message.t
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
    val to_yojson : t -> Yojson.Safe.t
  end

  module Create : sig
    type previous_room = { room_id  : string
                         ; event_id : string
                         }
    type t = { creator      : string
             ; federate     : bool option
             ; room_version : string option
             ; predecessor  : previous_room option
             }
    include DerivingYojson with type t := t
  end

  module GuestAccess : sig
    type access = CanJoin | Forbidden
    type t = { guest_access : access }
    include DerivingYojson with type t := t
  end

  module JoinRules : sig
    type t = { join_rule : string }
    include DerivingYojson with type t := t
  end

  module HistoryVisibility : sig
    type visibility = Invited | Joined | Shared | WorldReadable
    type t = { history_visibility : visibility option; }
    include DerivingYojson with type t := t
  end

  module Member : sig
    type membership = Invite | Join | Knock | Leave | Ban
    type signatures = string StringMap.t StringMap.t
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
    include DerivingYojson with type t := t
  end

  module CanonicalAlias : sig
    type t = { alias       : string option
             ; alt_aliases : string list option
             }
    include DerivingYojson with type t := t
  end

  module Name : sig
    type t = { name : string; }
    include DerivingYojson with type t := t
  end

  module Topic : sig
    type t = { topic : string; }
    include DerivingYojson with type t := t
  end

  module Avatar : sig
    type t = { info : ImageInfo.t option; url : string; }
    include DerivingYojson with type t := t
  end

  module PowerLevels : sig
    type notifications = { room : int option; }
    type int_string_map = int StringMap.t
    type t = { ban            : int option
             ; events         : int_string_map option
             ; events_default : int option
             ; invite         : int option
             ; kick           : int option
             ; redact         : int option
             ; state_default  : int option
             ; users          : int_string_map option
             ; users_default  : int option
             ; notifications  : notifications option
             }
    include DerivingYojson with type t := t
  end

  module PinnedEvents : sig
    type t = { pinned : string list; }
    include DerivingYojson with type t := t
  end

  module Encryption : sig
    type t = { algorithm            : string
             ; rotation_period_ms   : int option
             ; rotation_period_msgs : int option
             }
    include DerivingYojson with type t := t
  end

  module Redaction : sig
    type t = { reason : string option; }
    include DerivingYojson with type t := t
  end

  module Encrypted : sig
    type ciphertext_info = { body : string option
                           ; olm_type : int option
                           }
    type cipher_map = ciphertext_info StringMap.t
    type ciphertext = Cipher of string | CipherMap of cipher_map
    type t = { algorithm  : string
             ; ciphertext : ciphertext
             ; sender_key : string
             ; device_id  : string option
             ; session_id : string option
             }
    include DerivingYojson with type t := t
  end

  module Tombstone : sig
    type t = { body : string; replacement_room : string; }
    include DerivingYojson with type t := t
  end

  module Sticker : sig
    type t = { body : string
             ; info : ImageInfo.t
             ; url : string
             }
    include DerivingYojson with type t := t
  end

  module Widgets : sig
    type data = { widgetSessionId : string; }
    type t = { name   : string
             ; m_type : string
             ; url    : string
             ; data   : data
             }
    include DerivingYojson with type t := t
  end

  module PreviewUrls : sig
    type t = { disable : bool; }
    include DerivingYojson with type t := t
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

    val to_m_type : t -> string
    val of_yojson : String.t -> Yojson.Safe.t -> (t, String.t) Result.t
    val to_yojson : t -> Yojson.Safe.t
  end

  type event_content =
    | Event of { content : Content.t }
    | State of { content : Content.t; prev_content : Content.t option }

  val get_contents : event_content -> Content.t * Content.t option

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
    include DerivingYojson with type t := t
  end

  type t = Common.t * event_content

  val extra_unsigned : Yojson.Safe.t -> Yojson.Safe.t
  val uncommon_unsigned : Yojson.Safe.t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> (t, String.t) Result.t
  val to_yojson : t -> Yojson.Safe.t
end = struct
  (* HACK: This enables use of to_yojson with optional default values. From what
   * I understand, this might be fixed in the next release of
   * ppx_deriving_yojson, try removing when the changes hit opam. Link to
   * relevant issue: __ *)
  let ( = ) = Stdlib.( = )

  module EncryptedFile = struct
    type json_web_key = { kty     : string
                        ; key_ops : string list
                        ; alg     : string
                        ; k       : string
                        ; ext     : bool
                        } [@@deriving yojson]

    type hashes_map = string StringMap.t

    let hashes_map_of_yojson = StringMap.of_yojson string_of_yojson
    let hashes_map_to_yojson = StringMap.to_yojson yo_string

    type t = { url    : string
             ; key    : json_web_key
             ; iv     : string
             ; hashes : hashes_map
             ; v      : string  (* must be = "v2" *)
             } [@@deriving yojson]
  end

  module ThumbnailInfo = struct
    type t = { h        : int option    [@default None]
             ; w        : int option    [@default None]
             ; mimetype : string option [@default None]
             ; size     : int option    [@default None]
             } [@@deriving yojson]

    let create ?h ?w ?mimetype ?size () =
      { h
      ; w
      ; mimetype
      ; size
      }
  end

  module ImageInfo = struct
    type t = { h              : int option             [@default None]
             ; w              : int option             [@default None]
             ; mimetype       : string option          [@default None]
             ; size           : int option             [@default None]
             ; thumbnail_info : ThumbnailInfo.t option [@default None]
             ; thumbnail_url  : string option          [@default None]
             ; thumbnail_file : EncryptedFile.t option [@default None]
             } [@@deriving yojson]

    let create
        ?h ?w ?mimetype ?size ?thumbnail_info ?thumbnail_url ?thumbnail_file () =
      { h
      ; w
      ; mimetype
      ; size
      ; thumbnail_info
      ; thumbnail_url
      ; thumbnail_file
      }
  end

  module Message = struct
    module Text = struct
      type in_reply = { event_id : string } [@@deriving yojson]

      type relates =
        { in_reply_to : in_reply option [@key "m.in_reply_to"] [@default None]
        } [@@deriving yojson]

      type t =
        { body           : string
        ; format         : string option [@default None]
        ; formatted_body : string option [@default None]
        ; msgtype        : string
        ; relates_to     : relates option [@key "m.relates_to"] [@default None]
        } [@@deriving yojson]

      let create ?format ?formatted_body ?relates_to body =
        { body
        ; format
        ; formatted_body
        ; msgtype = "m.text"
        ; relates_to
        }
      let create_msg ?format ?formatted_body ?relates_to body =
        create ?format ?formatted_body ?relates_to body |> Room.Message.text
    end

    module Emote = struct
      type t = { body           : string
               ; format         : string option [@default None]
               ; formatted_body : string option [@default None]
               ; msgtype        : string
               } [@@deriving yojson]

      let create ?format ?formatted_body body =
        { body
        ; format
        ; formatted_body
        ; msgtype = "m.emote"
        }
      let create_msg ?format ?formatted_body body =
        create ?format ?formatted_body body |> Room.Message.emote
    end

    module Notice = struct
      type t = { body           : string
               ; format         : string option [@default None]
               ; formatted_body : string option [@default None]
               ; msgtype        : string
               } [@@deriving yojson]

      let create ?format ?formatted_body body =
        { body
        ; format
        ; formatted_body
        ; msgtype = "m.notice"
        }
      let create_msg ?format ?formatted_body body =
        create ?format ?formatted_body body |> Room.Message.notice
    end

    module Image = struct
      (* NOTE: url is required if unencrypted, file is required if encrypted. *)
      type t = { body    : string
               ; info    : ImageInfo.t option     [@default None]
               ; url     : string option          [@default None]
               ; file    : EncryptedFile.t option [@default None]
               ; msgtype : string
               } [@@deriving yojson]

      let create ?info ?url ?file body =
        { body
        ; info
        ; url
        ; file
        ; msgtype = "m.image"
        }
      let create_msg ?info ?url ?file body =
        create ?info ?url ?file body |> Room.Message.image
      let no_info_msg ?url ?file body = create_msg ?url ?file body
    end

    module File = struct
      type info = { mimetype       : string option          [@default None]
                  ; size           : int option             [@default None]
                  ; thumbnail_url  : string option          [@default None]
                  ; thumbnail_file : EncryptedFile.t option [@default None]
                  ; thumbnail_info : ThumbnailInfo.t option [@default None]
                  } [@@deriving yojson]

      let create_info ?mimetype ?size ?thumbnail_url ?thumbnail_file ?thumbnail_info () =
        { mimetype
        ; size
        ; thumbnail_url
        ; thumbnail_file
        ; thumbnail_info
        }

      type t = { body     : string
               ; filename : string option          [@default None]
               ; info     : info option            [@default None]
               ; url      : string option          [@default None]
               ; file     : EncryptedFile.t option [@default None]
               ; msgtype  : string
               } [@@deriving yojson]

      let create ?filename ?info ?url ?file body =
        { body
        ; filename
        ; info
        ; url
        ; file
        ; msgtype = "m.file"
        }
      let create_msg ?filename ?info ?url ?file body =
        create ?filename ?info ?url ?file body |> Room.Message.file
      let no_info_msg ?url ?file body = create_msg ?url ?file body
    end

    module Audio = struct
      type info = { duration : int option    [@default None]
                  ; mimetype : string option [@default None]
                  ; size     : int option    [@default None]
                  } [@@deriving yojson]

      let create_info ?duration ?mimetype ?size () =
        { duration
        ; mimetype
        ; size
        }

      type t = { body    : string
               ; info    : info option            [@default None]
               ; url     : string option          [@default None]
               ; file    : EncryptedFile.t option [@default None]
               ; msgtype : string
               } [@@deriving yojson]

      let create ?info ?url ?file body =
        { body
        ; info
        ; url
        ; file
        ; msgtype = "m.audio"
        }
      let create_msg ?info ?url ?file body =
        create ?info ?url ?file body |> Room.Message.audio
      let no_info_msg ?url ?file body = create_msg ?url ?file body
    end

    module Location = struct
      type info = { thumbnail_url  : string option          [@default None]
                  ; thumbnail_file : EncryptedFile.t option [@default None]
                  ; thumbnail_info : ThumbnailInfo.t option [@default None]
                  } [@@deriving yojson]

      let create_info ?thumbnail_url ?thumbnail_file ?thumbnail_info () =
        { thumbnail_url
        ; thumbnail_file
        ; thumbnail_info
        }

      type t = { body    : string
               ; geo_uri : string
               ; info    : info option [@default None]
               ; msgtype : string
               } [@@deriving yojson]

      let create ?info ~geo_uri body =
        { body
        ; geo_uri
        ; info
        ; msgtype = "m.location"
        }
      let create_msg ?info ~geo_uri body =
        create ?info ~geo_uri body |> Room.Message.location
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
                  } [@@deriving yojson]

      let create_info
          ?duration ?h ?w ?mimetype ?size ?thumbnail_url ?thumbnail_file ?thumbnail_info ()
        =
        { duration
        ; h
        ; w
        ; mimetype
        ; size
        ; thumbnail_url
        ; thumbnail_file
        ; thumbnail_info
        }

      (* NOTE: url or file is required depending on encryption. *)
      type t = { body    : string
               ; info    : info option            [@default None]
               ; url     : string option          [@default None]
               ; file    : EncryptedFile.t option [@default None]
               ; msgtype : string
               } [@@deriving yojson]

      let create ?info ?url ?file body =
        { body
        ; info
        ; url
        ; file
        ; msgtype = "m.video"
        }
      let create_msg ?info ?url ?file body =
        create ?info ?url ?file body |> Room.Message.video
      let no_info_msg ?url ?file body = create_msg ?url ?file body
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

    let to_yojson = function
      | Text     c -> Text.to_yojson c
      | Emote    c -> Emote.to_yojson c
      | Notice   c -> Notice.to_yojson c
      | Image    c -> Image.to_yojson c
      | File     c -> File.to_yojson c
      | Audio    c -> Audio.to_yojson c
      | Location c -> Location.to_yojson c
      | Video    c -> Video.to_yojson c
      | Unknown  j -> j
  end

  module Create = struct
    type previous_room = { room_id  : string
                         ; event_id : string
                         } [@@deriving yojson]

    type t = { creator      : string
             ; federate     : bool option [@key "m.federate"] [@default None]
             ; room_version : string option                   [@default None]
             ; predecessor  : previous_room option            [@default None]
             } [@@deriving yojson]
  end

  module GuestAccess = struct
    type access = CanJoin | Forbidden

    let access_of_yojson = function
      | `String "can_join"  -> Result.return CanJoin
      | `String "forbidden" -> Result.return Forbidden
      | `String s           -> Result.fail ("Invalid enum value: " ^ s)
      | _                   -> Result.fail "Missing/wrong-typed field."

    let access_to_yojson = function
      | CanJoin -> yo_string "can_join"
      | Forbidden -> yo_string "forbidden"

    type t = { guest_access : access } [@@deriving yojson]
  end

  module JoinRules = struct
    type t = { join_rule : string } [@@deriving yojson]
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

    let visibility_to_yojson = function
      | Invited       -> yo_string "invited"
      | Joined        -> yo_string "joined"
      | Shared        -> yo_string "shared"
      | WorldReadable -> yo_string "world_readable"

    type t = { history_visibility : visibility option [@default None] }
    [@@deriving yojson]
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

    let membership_to_yojson = function
      | Invite -> yo_string "invite"
      | Join   -> yo_string "join"
      | Knock  -> yo_string "knock"
      | Leave  -> yo_string "leave"
      | Ban    -> yo_string "ban"

    (* TODO: Need to read more into this... This is the basic structure though.
     * See: https://matrix.org/docs/spec/appendices#signing-json *)
    type signatures = (string StringMap.t) StringMap.t

    let signatures_of_yojson =
      StringMap.of_yojson (StringMap.of_yojson string_of_yojson)

    let signatures_to_yojson =
      StringMap.to_yojson (StringMap.to_yojson yo_string)

    type signed = { mxid       : string
                  ; signatures : signatures
                  ; token      : string
                  } [@@deriving yojson]

    type invite = { display_name : string
                  ; signed       : signed
                  } [@@deriving yojson]

    type t = { avatar_url         : string option        [@default None]
             ; displayname        : string option        [@default None]
             ; inviter            : string option        [@default None]
             ; membership         : membership
             ; is_direct          : bool option          [@default None]
             ; third_party_invite : invite option        [@default None]
             ; invite_room_state  : Room.t list option   [@default None]
    (* ; invite_room_state  : Yojson.Safe.t *)
             } [@@deriving yojson]
  end

  module CanonicalAlias = struct
    type t = { alias       : string option      [@default None]
             ; alt_aliases : string list option [@default None]
             } [@@deriving yojson]
  end

  module Name = struct
    type t = { name : string } [@@deriving yojson]
  end

  module Topic = struct
    type t = { topic : string } [@@deriving yojson]
  end

  module Avatar = struct
    type t = { info : ImageInfo.t option [@default None]
             ; url  : string
             } [@@deriving yojson]
  end

  module PowerLevels = struct
    type notifications =
      { room : int option [@default None] } [@@deriving yojson]

    type int_string_map = int StringMap.t

    let int_string_map_of_yojson = StringMap.of_yojson int_of_yojson

    let int_string_map_to_yojson = StringMap.to_yojson yo_int

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
             } [@@deriving yojson]
  end

  module PinnedEvents = struct
    type t = { pinned : string list } [@@deriving yojson]
  end

  module Encryption = struct
    (* algorithm is an enum that must be 'm.megolm.v1.aes-sha2' *)
    type t = { algorithm            : string
             ; rotation_period_ms   : int option [@default None]
             ; rotation_period_msgs : int option [@default None]
             } [@@deriving yojson]
  end

  module Redaction = struct
    type t = { reason : string option [@default None] } [@@deriving yojson]
  end

  module Encrypted = struct
    (* algorithm is an enum that must be 'm.olm.v1.curve25519-aes-sha2' or
     * 'm.megolm.v1.aes-sha2' *)
    type ciphertext_info = { body     : string option [@default None]
                           ; olm_type : int option    [@default None]
                           } [@@deriving yojson]

    type cipher_map = ciphertext_info StringMap.t

    let cipher_map_of_yojson = StringMap.of_yojson ciphertext_info_of_yojson

    let cipher_map_to_yojson = StringMap.to_yojson ciphertext_info_to_yojson

    type ciphertext =
      | Cipher of string
      | CipherMap of cipher_map

    (* TODO: add check that algorithm is in allowed set / type encode algos *)
    let ciphertext_of_yojson = function
      | `String s         -> Result.return (Cipher s)
      | `Assoc _ as assoc -> cipher_map_of_yojson assoc
                             |> Result.map ~f:(fun cm -> CipherMap cm)
      | _         -> Result.fail "Invalid ciphertext json."

    let ciphertext_to_yojson = function
      | Cipher s    -> yo_string s
      | CipherMap m -> cipher_map_to_yojson m

    type t = { algorithm  : string
             ; ciphertext : ciphertext
             ; sender_key : string
             ; device_id  : string option [@default None]
             ; session_id : string option [@default None]
             } [@@deriving yojson]
  end

  module Tombstone = struct
    type t = { body             : string
             ; replacement_room : string
             } [@@deriving yojson]
  end

  module Sticker = struct
    type t = { body : string
             ; info : ImageInfo.t
             ; url  : string
             } [@@deriving yojson]
  end

  module Widgets = struct
    (* NOTE: Very rough, the only example I have seen (which this is based on)
     * is Jitsi. I don't know what other members might be present / whether
     * these are optional. Don't make unstrict so I can catch the extras. *)
    type data = { widgetSessionId : string } [@@deriving yojson]

    type t = { name   : string
             ; m_type : string [@key "type"]
             ; url    : string
             ; data   : data
             } [@@deriving yojson]
  end

  module PreviewUrls = struct
    type t = { disable : bool } [@@deriving yojson]
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

    let to_m_type = function
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
      | Tombstone         _ -> "m.room.tombstone"
      | Sticker           _ -> "m.sticker"
      | Widgets           _ -> "im.vector.modular.widgets"
      | PreviewUrls       _ -> "org.matrix.room.preview_urls"

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

    let to_yojson = function
      | Message           c -> Message.to_yojson c
      | Create            c -> Create.to_yojson c
      | GuestAccess       c -> GuestAccess.to_yojson c
      | JoinRules         c -> JoinRules.to_yojson c
      | HistoryVisibility c -> HistoryVisibility.to_yojson c
      | Member            c -> Member.to_yojson c
      | CanonicalAlias    c -> CanonicalAlias.to_yojson c
      | Name              c -> Name.to_yojson c
      | Topic             c -> Topic.to_yojson c
      | Avatar            c -> Avatar.to_yojson c
      | PowerLevels       c -> PowerLevels.to_yojson c
      | PinnedEvents      c -> PinnedEvents.to_yojson c
      | Encryption        c -> Encryption.to_yojson c
      | Redaction         c -> Redaction.to_yojson c
      | Encrypted         c -> Encrypted.to_yojson c
      | Tombstone         c -> Tombstone.to_yojson c
      | Sticker           c -> Sticker.to_yojson c
      | Widgets           c -> Widgets.to_yojson c
      | PreviewUrls       c -> PreviewUrls.to_yojson c
  end

  type event_content =
    | Event of { content : Content.t }
    | State of { content : Content.t; prev_content : Content.t option }

  let get_contents = function
    | Event { content }               -> content, None
    | State { content; prev_content } -> content, prev_content

  let get_yojson_contents event_content =
    let (content, prev) = get_contents event_content in
    ( Content.to_yojson content
    , Option.value_map ~f:Content.to_yojson ~default:(`Assoc []) prev
    )

  module Common = struct
    (* NOTE: redacted_because amd transaction_id seem to be common for room/timeline
     * events, though they aren't the only possible optional fields. *)
    type unsigned = { age              : int option    [@default None]
                    ; redacted_because : string option [@default None]
                    ; transaction_id   : string option [@default None]
                    ; replaces_state   : string option [@default None]
                    ; prev_sender      : string option [@default None]
                    } [@@deriving yojson { strict = false }]

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
             } [@@deriving yojson { strict = false }]
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
    let unsigned = U.member "unsigned" j in
    let extra = extra_unsigned unsigned in
    let content = U.member "content" j
                  |> U.combine extra
                  |> Content.of_yojson com.m_type in
    if Option.is_some com.state_key then
      let prev_content = U.member "prev_content" unsigned
                         |> Content.of_yojson com.m_type
                         |> Result.ok in
      content >>| fun c -> com, State { content = c; prev_content }
    else
      content >>| fun c -> com, Event { content = c }

  (* NOTE: I'm not very happy with this, and how my representation of Room.t
   * with event_content etc has lead to this. TODO: think about it more. *)
  let to_yojson (common, event_content) =
    let content, prev_content = get_yojson_contents event_content in
    let com = Common.to_yojson common
              |> U.to_assoc
              |> List.filter ~f:(fun (k, _) -> String.equal "unsigned" k |> not)
              |> yo_assoc in
    let unsigned =
      common.unsigned
      |> Option.value_map ~f:Common.unsigned_to_yojson ~default:(yo_assoc [])
      |> U.combine (yo_assoc [ ("prev_content", prev_content) ]) in
    com |> U.combine (yo_assoc [ ("content", content) ]) |> U.combine unsigned
end
