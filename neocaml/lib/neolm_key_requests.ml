open! Core
open ToDevice

module OutgoingKeyRequest = struct
  type t = { request_id : string
           ; session_id : string
           ; room_id    : string
           ; algorithm  : string
           }

  let create request_id session_id room_id algorithm =
    { request_id; session_id; room_id; algorithm }

  let of_room_key_request RoomKeyRequest.{content; _} =
    match content with
    | { body = Some { room_id; session_id; algorithm; _ }; request_id; _ } ->
      Result.return { request_id; session_id; room_id; algorithm }
    | _ -> Result.fail (`Protocol "Must have KeyInfo (not a cancellation).")

  let from_database = ()

  let to_cancellation t user_id requesting_device_id =
    let event = RoomKeyRequest.create_cancellation t.request_id requesting_device_id
                |> room_key_request
    in
    to_message event user_id "*"
end

module IncomingKeyRequest = struct
  type t = { sender  : string
           ; request : RoomKeyRequest.t
           }

  let of_room_key_request sender request = { sender; request }
end
