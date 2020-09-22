open Base
open Yojson
open Neo_infix
open Yojson_helpers

(* module U = Yojson.Safe.Util *)
type 'a string_map = (string, 'a, String.comparator_witness) Map.t

module JoinedRooms = struct
  (* NOTE: This is just for the response to the joined_rooms API request, not to
   * be confused with JoinedRoom which is part of the sync response. *)
  type t = string list

  let of_yojson =
    Safe.Util.member "joined_rooms"
    >> Safe.Util.to_list
    >> List.map ~f:Safe.Util.to_string
end

module EventList = struct
  type t = { events : Events.t list } [@@deriving of_yojson]
end

module StateList = struct
  (* TODO: Figure out which uses of EventList actually only includes state
   * events (which are exclusively Room events). Replace those with this type. *)
  type t = { events : Events.Room.t list } [@@deriving of_yojson]
end

module RoomMember = struct
  type t = { user_id      : string
           ; display_name : string
           ; avatar_url   : string
           }
end

module Device = struct
  type t = { id             : string
           ; display_name   : string
           ; last_seen_ip   : string
           ; last_seen_date : string (* FIXME: Should be a datetime type? *)
           }
end

module Sync = struct
  module Timeline = struct
    type t = { events     : Events.t list option [@default None]
             ; limited    : bool option          [@default None]
             ; prev_batch : string
             } [@@deriving of_yojson]
  end

  module RoomSummary = struct
    type t =
      { heroes               : string list option [@key "m.heroes"]       [@default None]
      ; joined_member_count  : int option [@key "m.joined_member_count"]  [@default None]
      ; invited_member_count : int option [@key "m.invited_member_count"] [@default None]
      } [@@deriving of_yojson]
  end

  module UnreadNotificationCounts = struct
    type t = { highlight_count    : int option [@default None]
             ; notification_count : int option [@default None]
             } [@@deriving of_yojson]
  end

  module JoinedRooms = struct
    (* NOTE: Not sure whether to bother with weird fields like msc count below
     * or just resign to using non-strict everywhere... *)
    type info =
      { summary              : RoomSummary.t option [@default None]
      ; state                : StateList.t option   [@default None]
      ; timeline             : StateList.t option   [@default None]
      ; ephemeral            : EventList.t option   [@default None]
      ; account_data         : EventList.t option   [@default None]
      ; unread_notifications : UnreadNotificationCounts.t option [@default None]
      ; msc2654_unread_count : int option [@key "org.matrix.msc2654.unread_count"] [@default None]
      } [@@deriving of_yojson { strict = false }]

    type t = info string_map

    let of_yojson = string_map_of_yojson info_of_yojson
  end

  module InvitedRooms = struct
    type info = { invite_state : EventList.t option [@default None] }
    [@@deriving of_yojson]

    type t = info string_map

    let of_yojson = string_map_of_yojson info_of_yojson
  end

  module LeftRooms = struct
    type info = { state        : EventList.t option [@default None]
                ; timeline     : Timeline.t option  [@default None]
                ; account_data : EventList.t option [@default None]
                } [@@deriving of_yojson]

    type t = info string_map

    let of_yojson = string_map_of_yojson info_of_yojson
  end

  module Rooms = struct
    type t = { invite : InvitedRooms.t option [@default None]
             ; join   : JoinedRooms.t option  [@default None]
             ; leave  : LeftRooms.t option    [@default None]
             } [@@deriving of_yojson]
  end

  module DeviceLists = struct
    (* NOTE: E2E encryption related *)
    type t = { changed : string list option [@default None]
             ; left    : string list option [@default None]
             } [@@deriving of_yojson]
  end

  module OneTimeKeysCount = struct
    (* NOTE: E2E encryption related *)
    type t = int string_map

    let of_yojson = string_map_of_yojson int_of_yojson
  end

  type t =
    { next_batch                 : string
    ; rooms                      : Rooms.t option            [@default None]
    ; presence                   : EventList.t option        [@default None]
    ; account_data               : EventList.t option        [@default None]
    ; to_device                  : EventList.t option        [@default None]
    ; device_lists               : DeviceLists.t option      [@default None]
    ; device_one_time_keys_count : OneTimeKeysCount.t option [@default None]
    } [@@deriving of_yojson { strict = false }]
end
