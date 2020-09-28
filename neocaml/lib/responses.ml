open Base
open Neo_infix
open Yojson_helpers

module EventList = struct
  type t = { events : Events.t list } [@@deriving of_yojson]
end

module StateList = struct
  type t = { events : Events.Room.t list } [@@deriving of_yojson]
end

module JoinedRooms = struct
  (* NOTE: This is just for the response to the joined_rooms API request, not to
   * be confused with JoinedRoom which is part of the sync response. *)
  type t = string list

  let of_yojson =
    U.member "joined_rooms"
    >> typed_list_of_yojson string_of_yojson
end

module RoomMessages = struct
  (* NOTE: Call events appear in chunk, should merge the Events.Call module into
   * Events.Room? Otherwise would have to use Events.t here. *)
  type t  = { start_token : string [@key "start"]
            ; end_token   : string [@key "end"]
            ; chunk       : Events.t list
            ; state       : Events.Room.t list option [@default None]
            } [@@deriving of_yojson]
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
    (* NOTE: Try using Events.Room.t but might need to be Events.t *)
    type t = { events     : Events.Room.t list option [@default None]
             ; limited    : bool option               [@default None]
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
      ; timeline             : Timeline.t option    [@default None]
      ; ephemeral            : EventList.t option   [@default None]
      ; account_data         : EventList.t option   [@default None]
      ; unread_notifications : UnreadNotificationCounts.t option [@default None]
      ; msc2654_unread_count : int option [@key "org.matrix.msc2654.unread_count"] [@default None]
      } [@@deriving of_yojson { strict = false }]

    type t = info StringMap.t

    let of_yojson = StringMap.of_yojson info_of_yojson
  end

  module InvitedRooms = struct
    type info = { invite_state : EventList.t option [@default None] }
    [@@deriving of_yojson]

    type t = info StringMap.t

    let of_yojson = StringMap.of_yojson info_of_yojson
  end

  module LeftRooms = struct
    type info = { state        : EventList.t option [@default None]
                ; timeline     : Timeline.t option  [@default None]
                ; account_data : EventList.t option [@default None]
                } [@@deriving of_yojson]

    type t = info StringMap.t

    let of_yojson = StringMap.of_yojson info_of_yojson
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
    type t = int StringMap.t

    let of_yojson = StringMap.of_yojson int_of_yojson
  end

  (* NOTE: Arbitrary custom events created by other clients can be included in
   * account_data, so many Unknown events are expected. Those that seem useful
   * may be worth incorporating as this client is built out. *)
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
