open Core
open Yojson_helpers

module NewDevice = struct
  type content = { device_id : string
                 ; rooms     : string list
                 } [@@deriving of_yojson]

  type t = { m_type  : string [@key "type"]
           ; sender  : string
           ; content : content
           } [@@deriving of_yojson]
end

module KeyInfo = struct
  type algorithm = string

  let algorithm_of_yojson = function
    | `String ("m.megolm.v1.aes-sha2" as s) -> Result.return s
    | _ -> Result.fail "Key algorithm must be m.megolm.v1.aes-sha2."

  let algorithm_to_yojson s = `String s

  type t = { algorithm   : algorithm
           ; room_id     : string
           ; session_id  : string
           ; session_key : string
           } [@@deriving yojson]
end

module RoomKey = struct
  (* NOTE: to-device event *)
  type t = { content : KeyInfo.t
           ; m_type  : string [@key "type"]
           } [@@deriving yojson]
end

module RoomKeyRequest = struct
  (* NOTE: to-device event *)
  type action = Request | RequestCancellation

  let action_of_yojson = function
    | `String "request"              -> Result.return Request
    | `String "request_cancellation" -> Result.return RequestCancellation
    | `String s                      -> Result.fail ("Invalid key action: " ^ s)
    | _                              -> Result.fail "Invalid key action type."

  let action_to_yojson = function
    | Request             -> `String "request"
    | RequestCancellation -> `String "request_cancellation"

  (* body is required when action is request *)
  type content = { body                 : KeyInfo.t option [@default None]
                 ; action               : action
                 ; requesting_device_id : string
                 ; request_id           : string
                 } [@@deriving yojson]

  type t = { content : content
           ; m_type  : string [@key "type"]
           } [@@deriving yojson]
end

module ForwardedRoomKey = struct
  type content = { algorithm                       : string
                 ; room_id                         : string
                 ; sender_key                      : string
                 ; session_id                      : string
                 ; session_key                     : string
                 ; sender_claimed_ed25519_key      : string
                 ; forwarding_curve25519_key_chain : string list
                 } [@@deriving yojson]

  type t = { content : content
           ; m_type  : string [@key "type"]
           } [@@deriving yojson]
end

module Dummy = struct
  (* NOTE: This event type is used to indicate new Olm sessions for end-to-end
   * encryption. Typically it is encrypted as an m.room.encrypted event, then sent
   * as a to-device event. *)
  type content = unit

  let content_of_yojson = function
    | `Assoc [] -> Result.return ()
    | _         -> Result.fail "Dummy content should be empty."

  let content_to_yojson () = `Assoc []

  type t = { content : content
           ; m_type  : string [@key "type"]
           } [@@deriving yojson]
end

module KeyVerification = struct
  module Request = struct
    type t = { from_device    : string
             ; transaction_id : string
             ; methods        : string list
             ; timestamp      : int
             } [@@deriving yojson]
  end

  module Start = struct
    (* NOTE: Not sure from specs when (and if) this is used. *)
    type t = { from_device    : string
             ; transaction_id : string
             ; v_method       : string [@key "method"]
             ; next_method    : string option [@default None]
             } [@@deriving yojson]
  end

  module StartSAS = struct
    type t = { transaction_id                : string
             ; v_method                      : string [@key "method"]
             ; key_agreement_protocols       : string list
             ; hashes                        : string list
             ; message_authentication_codes  : string list
             ; short_authentication_string   : string list
             } [@@deriving yojson]
  end

  module Cancel = struct
    type t = { transaction_id : string
             ; reason         : string
             ; code           : string
             } [@@deriving yojson]
  end

  module Accept = struct
    type v_method = string

    let v_method_of_yojson = function
      |`String ("m.sas.v1" as s) -> Result.return s
      | _                        -> Result.fail "Accept method must by 'm.sas.v1'."

    let v_method_to_yojson s = `String s

    type t = { transaction_id              : string
             ; v_method                    : v_method [@key "method"]
             ; key_agreement_protocol      : string
             ; hash                        : string
             ; message_authentication_code : string
             ; short_authentication_string : string list
             ; commitment                  : string
             } [@@deriving yojson]
  end

  module Key = struct
    type t = { transaction_id : string
             ; key            : string
             } [@@deriving yojson]
  end

  module Mac = struct
    type mac = string StringMap.t [@@deriving yojson]

    type t = { transaction_id : string
             ; mac            : mac
             ; keys           : string
             } [@@deriving yojson]
  end

  type t =
    | Request of Request.t
    | Cancel of Cancel.t
    | Start of Start.t
    | StartSAS of StartSAS.t
    | Accept of Accept.t
    | Key of Key.t
    | Mac of Mac.t

  let request e   = Request e
  let cancel e    = Cancel e
  let start e     = Start e
  let start_sas e = StartSAS e
  let accept e    = Accept e
  let key e       = Key e
  let mac e       = Mac e

  let is_sas c =
    try U.member "method" c |> U.to_string |> String.equal "m.sas.v1"
    with _ -> false

  let of_yojson j =
    let open Result in
    U.member "type" j |> string_of_yojson >>= fun m_type ->
    let c = U.member "content" j in
    match String.chop_prefix_if_exists ~prefix:"m.key.verification." m_type with
    | "request"             -> Request.of_yojson c   >>| request
    | "cancel"              -> Cancel.of_yojson c    >>| cancel
    | "start" when is_sas c -> StartSAS.of_yojson c  >>| start_sas
    | "start"               -> Start.of_yojson c     >>| start
    | "accept"              -> Accept.of_yojson c    >>| accept
    | "key"                 -> Key.of_yojson c       >>| key
    | "mac"                 -> Mac.of_yojson c       >>| mac
    | m                     -> Result.fail ("Unknown verification type: " ^ m)
end


type t =
  | NewDevice of NewDevice.t
  | KeyInfo of KeyInfo.t
  | RoomKey of RoomKey.t
  | RoomKeyRequest of RoomKeyRequest.t
  | ForwardedRoomKey of ForwardedRoomKey.t
  | Dummy of Dummy.t
  | KeyVerification of KeyVerification.t
  | Unknown of Yojson.Safe.t

let new_device e        = NewDevice e
let key_verification e  = KeyVerification e
let unknown e           = Unknown e

let is_key_veri m = String.is_prefix m ~prefix:"m.key.verification."

let of_yojson j =
  let open Result in
  match U.member "type" j |> U.to_string_option with
  | Some m when is_key_veri m  -> KeyVerification.of_yojson j >>| key_verification
  | Some "m.new_device"        -> NewDevice.of_yojson j       >>| new_device
  (* | Some s                     -> Result.fail ("Invalid event type: " ^ s) *)
  | Some _                     -> Result.return j             >>| unknown
  | None                       -> Result.fail "Missing event type field."
