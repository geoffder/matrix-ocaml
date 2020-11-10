open Core
open Yojson_helpers

module Room = Events_room.Room

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
           ; room_id : string option [@default None]
           ; content : content
           } [@@deriving of_yojson]
end

module Receipt = struct
  (* NOTE: Ephemeral event. *)
  type receipt = { ts : int option [@default None] } [@@deriving of_yojson]

  type users = receipt StringMap.t [@@deriving of_yojson]

  type receipts =
    { read : users option [@key "m.read"] [@default None] } [@@deriving of_yojson]

  (* map from event_id to map from user_id to timestamp *)
  type content = receipts StringMap.t [@@deriving of_yojson]

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
  type content = (string list) StringMap.t [@@deriving of_yojson]

  type t = { m_type  : string [@key "type"]
           ; content : content
           } [@@deriving of_yojson]
end

module IgnoredUserList = struct
  (* NOTE: The yojson object is empty at this time according to spec. *)
  type ignored_users = Yojson.Safe.t StringMap.t [@@deriving of_yojson]

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

  type tag_map = tag StringMap.t [@@deriving of_yojson]

  type content = { tags : tag_map } [@@deriving of_yojson]

  type t = { m_type  : string [@key "type"]
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

  type devices = ruleset StringMap.t [@@deriving of_yojson]

  type content = { global : ruleset
                 ; device : devices option [@default None]
                 } [@@deriving of_yojson { strict = false }]

  type t = { m_type : string [@key "type"]
           ; content : content
           } [@@deriving of_yojson]
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
  | PushRules of PushRules.t
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
let push_rules e        = PushRules e
let unknown e           = Unknown e

let is_room_type m =
  String.is_prefix m ~prefix:"m.room."
  || String.equal m "m.sticker"
  || String.equal m "im.vector.modular.widgets"
  || String.equal m "org.matrix.room.preview_urls"

let is_call_type m = String.is_prefix m ~prefix:"m.call."

let of_yojson j =
  let open Result in
  match U.member "type" j |> U.to_string_option with
  | Some m when is_room_type m -> Room.of_yojson j            >>| room
  | Some m when is_call_type m -> Call.of_yojson j            >>| call
  | Some "m.presence"          -> Presence.of_yojson j        >>| presence
  | Some "m.typing"            -> Typing.of_yojson j          >>| typing
  | Some "m.receipt"           -> Receipt.of_yojson j         >>| receipt
  | Some "m.fully_read"        -> FullyRead.of_yojson j       >>| fully_read
  | Some "m.identity_server"   -> IdentityServer.of_yojson j  >>| identity_server
  | Some "m.direct"            -> Direct.of_yojson j          >>| direct
  | Some "m.ignored_user_list" -> IgnoredUserList.of_yojson j >>| ignored_user_list
  | Some "m.tag"               -> Tag.of_yojson j             >>| tag
  | Some "m.push_rules"        -> PushRules.of_yojson j       >>| push_rules
  (* | Some s                     -> Result.fail ("Invalid event type: " ^ s) *)
  | Some _                     -> Result.return j             >>| unknown
  | None                       -> Result.fail "Missing event type field."
