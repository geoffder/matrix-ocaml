open Core
open Yojson_helpers

module Empty (M : sig val fail : string end) = struct
  type t = unit

  let of_yojson = function
    | `Assoc [] -> Result.return ()
    | _         -> Result.fail M.fail
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

module EventID = struct
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
  type t = { device_id    : string
           ; user_id      : string
           ; display_name : string option [@default None]
           ; last_seen_ts : int option    [@default None]
           ; last_seen_ip : string option [@default None]
           } [@@deriving of_yojson]
end

module Devices = struct
  type t = { devices : Device.t list } [@@deriving of_yojson]
end

module KeysQuery = struct
  type unsigned_device_info = { device_display_name : string } [@@deriving of_yojson]

  type device_keys = { user_id    : string
                     ; device_id  : string
                     ; algorithms : string list
                     ; keys       : string StringMap.t
                     ; signatures : string StringMap.t StringMap.t
                     ; unsigned   : unsigned_device_info option [@default None]
                     } [@@deriving of_yojson]

  type t = { failures    : Yojson.Safe.t StringMap.t
           ; device_keys : device_keys StringMap.t StringMap.t
           } [@@deriving of_yojson]
end

module UpdateDevice = Empty (struct let fail = "User has no device with given ID." end)

module DeleteDevices = Empty (struct let fail = "Additional authentication is required." end)

module JoinedMembers = struct
  type room_member = { display_name : string
                     ; avatar_url   : string
                     } [@@deriving of_yojson]

  type t = { joined : room_member list } [@@deriving of_yojson]
end

module RoomGetState = struct
  type t = Events.Room.t list [@@deriving of_yojson]
end

module RoomResolveAlias = struct
  type t = { room_id : string
           ; servers : string list
           } [@@deriving of_yojson]
end

module GetProfile = struct
  type t = { avatar_url   : string
           ; display_name : string
           } [@@deriving of_yojson]
end

module GetPresence = struct
  type t = { presence        : Types.Presence.t
           ; last_active_ago : int
           } [@@deriving of_yojson]
end

module SetPresence = Empty (struct let fail = "Could not set presence." end)

module GetDisplayName = struct
  type t = { display_name : string } [@@deriving of_yojson]
end

module SetDisplayName = Empty (struct let fail = "Could not set display name." end)

module GetAvatar = struct
  type t = { avatar_url : string } [@@deriving of_yojson]
end

module SetAvatar = Empty (struct let fail = "Could not set avatar url." end)

module Join = struct
  type t = { room_id : string } [@@deriving of_yojson]
end

module RoomInvite = Empty (struct let fail = "Invite failed." end)

module RoomLeave = Empty (struct let fail = "Failed to leave room." end)

module RoomForget = Empty (struct let fail = "Failed to forget room." end)

module RoomKick = Empty (struct let fail = "Failed to kick user from room." end)

module RoomBan = Empty (struct let fail = "Failed to ban user from room." end)

module RoomUnban = Empty (struct let fail = "Failed to unban user from room." end)

module RoomContext = struct
  (* TODO: Make sure my naming of these start and end tokens are consistent
   * with elsewhere. Check other occurences, Sync is one for sure. *)
  type t = { start_token   : string
           ; end_token     : string
           ; events_before : Events.Room.t list
           ; event         : Events.Room.t
           ; events_after  : Events.Room.t list
           ; state         : Events.Room.t list
           } [@@deriving of_yojson]
end

module RoomTyping = Empty (struct let fail = "Failed to send typing notification." end)

module UpdateReceiptMarker = Empty (struct let fail = "Failed to send receipt." end)

module RoomReadMarkers = Empty (struct let fail = "Failed to update read markers." end)

module ContentRepositoryConfig = struct
  type t = { max_upload_size : int [@key "m.upload.size"] } [@@deriving of_yojson]
end

module RoomCreate = struct
  type t = { room_id : string } [@@deriving of_yojson]
end

module UploadFilter = struct
  type t = { filter_id : string }[@@deriving of_yojson]
end

module WhoAmI = struct
  type t = { user_id : string }[@@deriving of_yojson]
end

module DiscoveryInfo = struct
  type t = string StringMap.t StringMap.t [@@deriving of_yojson]
end

module LoginInfo = struct
  type login_flow = { m_type : string [@key "type"] } [@@deriving of_yojson]
  type t = { flows : login_flow list } [@@deriving of_yojson]
end

module KeysClaim = struct
  type failures = string list

  let failures_of_yojson j =
    try U.keys j |> Result.return
    with _ -> Result.fail "Expected `Assoc of server contact failures."

  type t = { failures : failures
           ; one_time_keys : Types.OneTimeKeys.t StringMap.t
           } [@@deriving of_yojson]
end

module KeysUpload = struct
  type t = { one_time_key_counts : int StringMap.t } [@@deriving of_yojson]
end

(* TODO: Add an additional authentication required response (interactive
 * authentication API support) *)

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

let of_yojson payload_of_yojson j =
  payload_of_yojson j |> Result.map_error ~f:(fun e -> NeoError.Resp.of_yojson e j)
