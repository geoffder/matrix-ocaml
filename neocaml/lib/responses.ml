open Base
open Yojson
open Neo_infix

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

module JoinedRooms = struct
  type t = string list

  let of_yojson =
    Safe.Util.member "joined_rooms"
    >> Safe.Util.to_list
    >> List.map ~f:Safe.Util.to_string
end

module RoomMember = struct
  type t = { user_id      : string
           ; display_name : string
           ; avatar_url   : string
           }
end

module Device = struct
  type t = { id: string
           ; display_name : string
           ; last_seen_ip : string
           ; last_seen_date : string (* FIXME: Should be a datetime type *)
           }
end

module InviteInfo = struct
  type t
end

module RoomInfo = struct
  type t
end

module Rooms = struct
  type t = { invite : (string, InviteInfo.t, String.comparator_witness) Map.t
           ; join   : (string, RoomInfo.t, String.comparator_witness) Map.t
           ; leave  : (string, RoomInfo.t, String.comparator_witness) Map.t
           }
end

module DeviceList = struct
  type t
end
