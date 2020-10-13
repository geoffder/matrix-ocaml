open Base
open Yojson_helpers

module ErrorResponse = struct
  module Standard = struct
    type t = { errcode        : string
             ; error          : string option [@default None]
             ; retry_after_ms : int option    [@default None]
             } [@@deriving of_yojson]
  end

  module Auth = struct
    type stages = { stages : string list } [@@deriving of_yojson]

    type params_map = (string StringMap.t) StringMap.t

    let params_map_of_yojson =
      StringMap.of_yojson (StringMap.of_yojson string_of_yojson)

    type t = { errcode   : string
             ; error     : string option [@default None]
             ; completed : string list
             ; flows     : stages list
             ; params    : params_map
             ; session   : string
             } [@@deriving of_yojson]
  end

  type t = StdErr of Standard.t | AuthErr of Auth.t | UnkErr of Yojson.Safe.t
  let std_err e  = StdErr e
  let auth_err e = AuthErr e
  let unk_err e  = UnkErr e

  let of_yojson j =
    let open Result in
    begin
      if Yojson.Safe.equal (U.member "session" j) `Null
      then Standard.of_yojson j >>| std_err
      else Auth.of_yojson j     >>| auth_err
    end |> function
    | Ok err  -> err
    | Error _ -> unk_err j
end

module NeoError = struct
  type t =
    | RespErr of ErrorResponse.t
    | JsonErr of string
    | Max429s     (* Client.send: too many requests *)
    | MaxTimeouts (* Client.send: timeout limit exceeded *)
    | NotLoggedIn

  let resp_err e = RespErr e
  let json_err e = JsonErr e
end

module EventList = struct
  type t = { events : Events.t list } [@@deriving of_yojson]
end

module StateList = struct
  type t = { events : Events.Room.t list } [@@deriving of_yojson]
end

module JoinedRooms = struct
  type t = { joined_rooms : string list } [@@deriving of_yojson]
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

module RoomSend = struct
  type t = { event_id : string } [@@deriving of_yojson]
end

module Upload = struct
  type t = { content_uri : string } [@@deriving of_yojson]
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
      { summary              : RoomSummary.t option              [@default None]
      ; state                : StateList.t option                [@default None]
      ; timeline             : Timeline.t option                 [@default None]
      ; ephemeral            : EventList.t option                [@default None]
      ; account_data         : EventList.t option                [@default None]
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

(* TODO: My error modelling is still not great, this way ends up ignoring the
 * error string from deriving_yojson that says which of_yojson failed. *)
let of_yojson (type a) (module M : DerivingOfYojson with type t = a) j =
  M.of_yojson j
  (* |> Result.map_error ~f:NeoError.json_err *)
  |> Result.map_error ~f:(fun _ -> ErrorResponse.of_yojson j |> NeoError.resp_err)
