open Base
open Yojson
open Neo_infix

type 'a string_map = (string, 'a, String.comparator_witness) Map.t

(* NOTE:
 * Upon looking over the setup in nio/responses.py I think that I will try to
 * avoid some of the ceremony and generate the appropriate events and other
 * expected pieces of data more directly from the main client functions. It seems
 * like Response objects are created from the series of calls in _send, like so:
 * TransportResponse from http -> Response from create_matrix_response. Then
 * the response is is hot-potatoed to receive_response, which then calls a
 * handling function based on the type of the response.
 *
 * Since I know what type of response it is, since the body is returned to the
 * relevant client function already, I will just call the appropruate handling
 * from there (which may invovle creating an event for callbacks etc.)
 *
 * Thus, for right now, I'll collect some types in here that will serve as jump
 * off points for what data I should be collecting from particular responses
 * from the server. *)

(* These types are tentative skeletons that may even be moved elsewhere. *)

(* TODO: It is probably worth building the sync response digestion here as one
 * of the first steps. I have come around a bit on the importance of a response
 * module for converting json responses from the server in to ocaml types. I
 * still think that some things are so simple that they should skip the
 * of_yojson step, such as login for example. *)

(* NOTE: This is just for the response to the joined_rooms API request, not to
 * be confused with JoinedRoom which is part of the sync response. *)
module JoinedRooms = struct
  type t = string list

  let of_yojson =
    Safe.Util.member "joined_rooms"
    >> Safe.Util.to_list
    >> List.map ~f:Safe.Util.to_string
end

module EventList = struct
  type t = { events : Events.t list } [@@deriving of_yojson]
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
    type t = { events     : Events.t list option
             ; limited    : bool option
             ; prev_batch : string
             } [@@deriving of_yojson]
  end

  module RoomSummary = struct
    type t = { heroes               : string list option [@key "m.heroes"]
             ; joined_member_count  : int option [@key "m.joined_member_count"]
             ; invited_member_count : int option [@key "m.invited_member_count"]
             } [@@deriving of_yojson]
  end

  module UnreadNotificationCounts = struct
    type t = { highlight_count    : int option
             ; notification_count : int option
             } [@@deriving of_yojson]
  end

  module JoinedRooms = struct
    type info = { summary              : RoomSummary.t option
                ; state                : EventList.t option
                ; timeline             : Timeline.t option
                ; ephemeral            : EventList.t option
                ; account_data         : EventList.t option
                ; unread_notifications : UnreadNotificationCounts.t option
                } [@@deriving of_yojson]

    type info_alist = (string * info) list [@@deriving of_yojson]

    type t = info string_map

    let of_yojson j =
      info_alist_of_yojson j |> Result.bind ~f:begin fun s ->
        try Map.of_alist_exn (module String) s |> Result.return
        with _ -> Result.fail "Invalid room_id -> joined room map."
      end
  end

  module InvitedRooms = struct
    type info = { invite_state : EventList.t option } [@@deriving of_yojson]

    type info_alist = (string * info) list [@@deriving of_yojson]

    type t = info string_map

    let of_yojson j =
      info_alist_of_yojson j |> Result.bind ~f:begin fun s ->
        try Map.of_alist_exn (module String) s |> Result.return
        with _ -> Result.fail "Invalid room_id -> invited room map."
      end
  end

  module LeftRooms = struct
    type info = { state        : EventList.t option
                ; timeline     : Timeline.t option
                ; account_data : EventList.t option
                } [@@deriving of_yojson]

    type info_alist = (string * info) list [@@deriving of_yojson]

    type t = info string_map

    let of_yojson j =
      info_alist_of_yojson j |> Result.bind ~f:begin fun s ->
        try Map.of_alist_exn (module String) s |> Result.return
        with _ -> Result.fail "Invalid room_id -> left room map."
      end
  end

  module Rooms = struct
    type t = { invite : InvitedRooms.t option
             ; join   : JoinedRooms.t option
             ; leave  : LeftRooms.t option
             } [@@deriving of_yojson]
  end

  module DeviceLists = struct
    (* NOTE: E2E encryption related *)
    type t = { changed : string list option
             ; left    : string list option
             } [@@deriving of_yojson]
  end

  module OneTimeKeysCount = struct
    (* NOTE: E2E encryption related *)
    type alist = (string * int) list [@@deriving of_yojson]

    type t = int string_map

    let of_yojson j =
      alist_of_yojson j |> Result.bind ~f:begin fun s ->
        try Map.of_alist_exn (module String) s |> Result.return
        with _ -> Result.fail "Invalid algorithm -> unclaimed count int map."
      end
  end

  type t = { next_batch                 : string
           ; rooms                      : Rooms.t option
           ; presence                   : EventList.t option
           ; account_data               : EventList.t option
           ; to_device                  : EventList.t option
           ; device_lists               : DeviceLists.t option
           ; device_one_time_keys_count : OneTimeKeysCount.t option
           } [@@deriving of_yojson]
end
