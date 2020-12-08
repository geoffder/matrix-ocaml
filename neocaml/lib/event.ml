open Core
open Yojson_helpers
open Result.Let_syntax

(** NOTE: Unimplemented events
 ** ### From spec ###
 ** - m.room.server_acl
 **
 ** Moderation policy lists (state events):
 ** - m.policy.rule.user
 ** - m.policy.rule.room
 ** - m.policy.rule.server
 **
 ** ### From sync response ###
 ** Account data:
 **
 ** To device:
 **  *)

module Room = Event_room.Room
module RoomState = Event_room.RoomState

module Ephemeral = struct
  module Typing = struct
    type t = { user_ids : string list } [@@deriving of_yojson]
  end

  module Receipt = struct
    (* NOTE: Ephemeral event. *)
    type receipt = { ts : int option [@default None] } [@@deriving of_yojson]
    type users = receipt StringMap.t [@@deriving of_yojson]

    type receipts = { read : users option [@key "m.read"] [@default None] }
    [@@deriving of_yojson]

    (* map from event_id to map from user_id to timestamp *)
    type t = receipts StringMap.t [@@deriving of_yojson]
  end

  module Content = struct
    type t =
      | Typing of Typing.t
      | Receipt of Receipt.t

    let typing e = Typing e
    let receipt e = Receipt e

    let of_yojson m_type c =
      match m_type with
      | "m.typing"  -> Typing.of_yojson c >>| typing
      | "m.receipt" -> Receipt.of_yojson c >>| receipt
      | m           -> Result.fail ("Unknown matrix type: " ^ m)
  end

  type t =
    { m_type : string
    ; content : Content.t
    ; room_id : string option
    }

  let of_yojson j =
    let%bind m_type = U.member "type" j |> string_of_yojson in
    let%map room_id = U.member "room_id" j |> opt_of_yojson string_of_yojson
    and content = U.member "content" j |> Content.of_yojson m_type in
    { m_type; content; room_id }
end

module Presence = struct
  type content =
    { avatar_url : string option [@default None]
    ; displayname : string option [@default None]
    ; last_active_ago : int option [@default None]
    ; presence : Types.Presence.t
    ; currently_active : bool option [@default None]
    ; status_msg : string option [@default None]
    }
  [@@deriving of_yojson]

  type t =
    { m_type : string [@key "type"]
    ; sender : string
    ; content : content
    }
  [@@deriving of_yojson]
end

module AccountData = struct
  module Direct = struct
    (* map from user_id to list of room_ids indicating what rooms are considered
     * "direct" rooms for that user. *)
    type t = string list StringMap.t [@@deriving of_yojson]
  end

  module FullyRead = struct
    type t = { event_id : string } [@@deriving of_yojson]
  end

  module PushRules = struct
    type push_condition =
      { kind : string
      ; key : string option [@default None]
      ; pattern : string option [@default None]
      ; is : string option [@default None]
      }
    [@@deriving of_yojson]

    (* NOTE: I've seen string and bool for value so far. Consider change to
     *  something other than json when I have more complete picture. *)
    type action =
      | Action of string
      | Tweak of
          { set_tweak : string
          ; value : Yojson.Safe.t
          }

    let action_of_yojson = function
      | `String s         -> Action s |> Result.return
      | `Assoc _ as assoc ->
        let value = U.member "value" assoc in
        U.member "set_tweak" assoc
        |> U.to_string_option
        |> Result.of_option ~error:"Invalid push_rule action Tweak."
        |> Result.map ~f:(fun set_tweak -> Tweak { set_tweak; value })
      | _                 -> Result.fail "Invalid push_rule action Tweak."

    type push_rule =
      { actions : action list
      ; default : bool
      ; enabled : bool
      ; rule_id : string
      ; conditions : push_condition list option [@default None]
      ; pattern : string option [@default None]
      }
    [@@deriving of_yojson]

    type ruleset =
      { content : push_rule list option [@default None]
      ; override : push_rule list option [@default None]
      ; room : push_rule list option [@default None]
      ; sender : push_rule list option [@default None]
      ; underride : push_rule list option [@default None]
      }
    [@@deriving of_yojson]

    type devices = ruleset StringMap.t [@@deriving of_yojson]

    type t =
      { global : ruleset
      ; device : devices option [@default None]
      }
    [@@deriving of_yojson { strict = false }]
  end

  module Tag = struct
    (* map from user defined tags to an order value that give the relative
     * position of the room under the given tag. *)
    type tag = { order : float option [@default None] } [@@deriving of_yojson]
    type tag_map = tag StringMap.t [@@deriving of_yojson]
    type t = { tags : tag_map } [@@deriving of_yojson]
  end

  module IgnoredUserList = struct
    (* NOTE: The yojson object is empty at this time according to spec.
     * TODO: Custom of yojson that will make ignored users a set instead. *)
    type ignored_users = Yojson.Safe.t StringMap.t [@@deriving of_yojson]
    type t = { ignored_users : ignored_users } [@@deriving of_yojson]
  end

  module IdentityServer = struct
    type t = { base_url : string option [@default None] } [@@deriving of_yojson]
  end

  module Widgets = struct
    (* TODO: Here is an example from a sync_response
       "type":"m.widgets",
       "content":{
          "75ffef59-9ddc-475e-802a-27474853d494":{
             "content":{
                "type":"m.stickerpicker",
                "url":"https://scalar.vector.im/api/widgets/id/75ffef59-9ddc-475e-802a-27474853d494/stickers.html",
                "name":"Stickerpack"
             },
             "sender":"@beheddard:matrix.org",
             "state_key":"75ffef59-9ddc-475e-802a-27474853d494",
             "type":"m.widget",
             "id":"75ffef59-9ddc-475e-802a-27474853d494"
          }
       }
    *)

  end

  module Content = struct
    type t =
      | Direct of Direct.t
      | FullyRead of FullyRead.t
      | PushRules of PushRules.t
      | Tag of Tag.t
      | IgnoredUserList of IgnoredUserList.t
      | IdentityServer of IdentityServer.t
      | Unknown of Yojson.Safe.t

    let direct e = Direct e
    let fully_read e = FullyRead e
    let push_rules e = PushRules e
    let tag e = Tag e
    let ignored_user_list e = IgnoredUserList e
    let identity_server e = IdentityServer e
    let unknown e = Unknown e

    let of_yojson m_type c =
      match m_type with
      | "m.direct"            -> Direct.of_yojson c >>| direct
      | "m.fully_read"        -> FullyRead.of_yojson c >>| fully_read
      | "m.push_rules"        -> PushRules.of_yojson c >>| push_rules
      | "m.tag"               -> Tag.of_yojson c >>| tag
      | "m.ignored_user_list" -> IgnoredUserList.of_yojson c >>| ignored_user_list
      | "m.identity_server"   -> IdentityServer.of_yojson c >>| identity_server
      | _                     -> Result.return (unknown c)
  end

  type t =
    { m_type : string
    ; content : Content.t
    }

  let of_yojson j =
    let%bind m_type = U.member "type" j |> string_of_yojson in
    let%map content = U.member "content" j |> Content.of_yojson m_type in
    { m_type; content }
end

module Timeline = struct
  type t =
    | Msg of Room.t
    | State of RoomState.t

  let msg e = Msg e
  let state e = State e

  let is_state j =
    match U.member "state_key" j with
    | `Null -> false
    | _     -> true

  let of_yojson j =
    if is_state j then RoomState.of_yojson j >>| state else Room.of_yojson j >>| msg
end

type t =
  | Room of Room.t
  | RoomState of RoomState.t
  | Ephemeral of Ephemeral.t
  | Presence of Presence.t
  | AccountData of AccountData.t
  | Unknown of Yojson.Safe.t

let room e = Room e
let room_state e = RoomState e
let ephemeral e = Ephemeral e
let presence e = Presence e
let account_data e = AccountData e
let unknown e = Unknown e
let is_room m = String.is_prefix m ~prefix:"m.room." || String.equal m "m.sticker"

let is_state j =
  match U.member "state_key" j with
  | `Null -> false
  | _     -> true

let is_ephemeral m = String.equal m "m.typing" || String.equal m "m.receipt"

let is_account m =
  match m with
  | "m.direct"
  | "m.fully_read"
  | "m.push_rules"
  | "m.tag"
  | "m.ignored_user_list"
  | "m.identity_server" -> true
  | _ -> false

let of_yojson j =
  match U.member "type" j |> U.to_string_option with
  | Some _ when is_state j -> RoomState.of_yojson j >>| room_state
  | Some m when is_room m -> Room.of_yojson j >>| room
  | Some m when is_ephemeral m -> Ephemeral.of_yojson j >>| ephemeral
  | Some m when is_account m -> AccountData.of_yojson j >>| account_data
  | Some "m.presence" -> Presence.of_yojson j >>| presence
  (* | Some s                     -> Result.fail ("Invalid event type: " ^ s) *)
  | Some _ -> Result.return j >>| unknown
  | None -> Result.fail "Missing event type field."
