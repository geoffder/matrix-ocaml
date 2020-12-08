open Core
open Yojson_helpers
open Result.Let_syntax

module NewDevice = struct
  type content =
    { device_id : string
    ; rooms : string list
    }
  [@@deriving yojson]

  type t =
    { m_type : string [@key "type"]
    ; sender : string
    ; content : content
    }
  [@@deriving yojson]
end

module KeyInfo = struct
  type algorithm = string

  let algorithm_of_yojson = function
    | `String ("m.megolm.v1.aes-sha2" as s) -> Result.return s
    | _ -> Result.fail "Key algorithm must be m.megolm.v1.aes-sha2."

  let algorithm_to_yojson s = `String s

  type t =
    { algorithm : algorithm
    ; room_id : string
    ; session_id : string
    ; session_key : string
    }
  [@@deriving yojson]
end

module RoomKey = struct
  type t =
    { content : KeyInfo.t
    ; m_type : string [@key "type"]
    }
  [@@deriving yojson]
end

module RoomKeyRequest = struct
  type action =
    | Request
    | RequestCancellation

  let action_of_yojson = function
    | `String "request" -> Result.return Request
    | `String "request_cancellation" -> Result.return RequestCancellation
    | `String s -> Result.fail ("Invalid key action: " ^ s)
    | _ -> Result.fail "Invalid key action type."

  let action_to_yojson = function
    | Request             -> `String "request"
    | RequestCancellation -> `String "request_cancellation"

  (* body is required when action is request *)
  type content =
    { body : KeyInfo.t option [@default None]
    ; action : action
    ; requesting_device_id : string
    ; request_id : string
    }
  [@@deriving yojson]

  type t =
    { content : content
    ; m_type : string [@key "type"]
    }
  [@@deriving yojson]

  let create_request key_info request_id requesting_device_id =
    { content =
        { body = Some key_info; action = Request; requesting_device_id; request_id }
    ; m_type = "m.room_key_request"
    }

  let create_cancellation request_id requesting_device_id =
    { content =
        { body = None; action = RequestCancellation; requesting_device_id; request_id }
    ; m_type = "m.room_key_request"
    }
end

module ForwardedRoomKey = struct
  type content =
    { algorithm : string
    ; room_id : string
    ; sender_key : string
    ; session_id : string
    ; session_key : string
    ; sender_claimed_ed25519_key : string
    ; forwarding_curve25519_key_chain : string list
    }
  [@@deriving yojson]

  type t =
    { content : content
    ; m_type : string [@key "type"]
    }
  [@@deriving yojson]
end

module Dummy = struct
  (* NOTE: Used to indicate new Olm sessions for end-to-end encryption. Typically
   * it is encrypted as an m.room.encrypted event, then sent as a to-device event. *)
  type content = unit

  let content_of_yojson = function
    | `Assoc [] -> Result.return ()
    | _         -> Result.fail "Dummy content should be empty."

  let content_to_yojson () = `Assoc []

  type t =
    { content : content
    ; m_type : string [@key "type"]
    }
  [@@deriving yojson]
end

module KeyVerification = struct
  module KeyAgreementProtocol = struct
    type t =
      | V1
      | V2

    let of_string = function
      | "curve25519"             -> Result.return V1
      | "curve25519-hkdf-sha256" -> Result.return V2
      | _                        -> Result.fail "Invalid key agreement protocol."

    let to_string = function
      | V1 -> "curve25519"
      | V2 -> "curve25519-hkdf-sha256"

    let equal a b =
      match a, b with
      | V1, V1 -> true
      | V2, V2 -> true
      | _      -> false

    let of_yojson = function
      | `String s -> of_string s
      | _         -> Result.fail "Key agreement protocol should be string."

    let to_yojson t = `String (to_string t)
  end

  module MacMethod = struct
    type t =
      | Normal
      | Old

    let to_string = function
      | Normal -> "hkdf-hmac-sha256"
      | Old    -> "hmac-sha256"

    let of_string = function
      | "hkdf-hmac-sha256" -> Result.return Normal
      | "hmac-sha256"      -> Result.return Old
      | _                  -> Result.fail "Invalid message authentication code."

    let equal a b =
      match a, b with
      | Normal, Normal -> true
      | Old, Old       -> true
      | _              -> false

    let of_yojson = function
      | `String s -> of_string s
      | _         -> Result.fail "Message authentication code should be string."

    let to_yojson t = `String (to_string t)
  end

  module SasMethod = struct
    type t =
      | Emoji
      | Decimal

    let to_string = function
      | Emoji   -> "emoji"
      | Decimal -> "decimal"

    let of_string = function
      | "emoji"   -> Result.return Emoji
      | "decimal" -> Result.return Decimal
      | _         -> Result.fail "Invalid short string authentication string."

    let equal a b =
      match a, b with
      | Emoji, Emoji     -> true
      | Decimal, Decimal -> true
      | _                -> false

    let of_yojson = function
      | `String s -> of_string s
      | _         -> Result.fail "Short string authentication string should be string."

    let to_yojson t = `String (to_string t)
  end

  module Request = struct
    type t =
      { from_device : string
      ; transaction_id : string
      ; methods : string list
      ; timestamp : int
      }
    [@@deriving yojson]
  end

  module Start = struct
    (* NOTE: Not sure from specs when (and if) this is used. *)
    type t =
      { from_device : string
      ; transaction_id : string
      ; v_method : string [@key "method"]
      ; next_method : string option [@default None]
      }
    [@@deriving yojson]
  end

  module StartSAS = struct
    type v_method = string

    let v_method_of_yojson = function
      | `String ("m.sas.v1" as s) -> Result.return s
      | _                         -> Result.fail "Accept method must by 'm.sas.v1'."

    let v_method_to_yojson s = `String s

    type t =
      { transaction_id : string
      ; v_method : v_method [@key "method"]
      ; key_agreement_protocols : KeyAgreementProtocol.t list
      ; hashes : string list
      ; message_authentication_codes : MacMethod.t list
      ; short_authentication_string : SasMethod.t list
      }
    [@@deriving yojson]
  end

  module Cancel = struct
    type t =
      { transaction_id : string
      ; reason : string
      ; code : string
      }
    [@@deriving yojson]
  end

  module Accept = struct
    type v_method = string

    let v_method_of_yojson = function
      | `String ("m.sas.v1" as s) -> Result.return s
      | _                         -> Result.fail "Accept method must by 'm.sas.v1'."

    let v_method_to_yojson s = `String s

    type t =
      { transaction_id : string
      ; v_method : v_method [@key "method"]
      ; key_agreement_protocol : KeyAgreementProtocol.t
      ; hash : string
      ; message_authentication_code : MacMethod.t
      ; short_authentication_string : SasMethod.t list
      ; commitment : string
      }
    [@@deriving yojson]
  end

  module Key = struct
    type t =
      { transaction_id : string
      ; key : string
      }
    [@@deriving yojson]
  end

  module Mac = struct
    type mac = string StringMap.t [@@deriving yojson]

    type t =
      { transaction_id : string
      ; mac : mac
      ; keys : string
      }
    [@@deriving yojson]
  end

  type t =
    | Request of Request.t
    | Cancel of Cancel.t
    | Start of Start.t
    | StartSAS of StartSAS.t
    | Accept of Accept.t
    | Key of Key.t
    | Mac of Mac.t

  let request e = Request e
  let cancel e = Cancel e
  let start e = Start e
  let start_sas e = StartSAS e
  let accept e = Accept e
  let key e = Key e
  let mac e = Mac e

  let to_m_type = function
    | Request _  -> "m.key.verification.request"
    | Cancel _   -> "m.key.verification.cancel"
    | Start _    -> "m.key.verification.start"
    | StartSAS _ -> "m.key.verification.start"
    | Accept _   -> "m.key.verification.accept"
    | Key _      -> "m.key.verification.key"
    | Mac _      -> "m.key.verification.mac"

  let transaction_id = function
    | Request e  -> e.transaction_id
    | Cancel e   -> e.transaction_id
    | Start e    -> e.transaction_id
    | StartSAS e -> e.transaction_id
    | Accept e   -> e.transaction_id
    | Key e      -> e.transaction_id
    | Mac e      -> e.transaction_id

  let is_sas c =
    try U.member "method" c |> U.to_string |> String.equal "m.sas.v1" with
    | _ -> false

  let of_yojson j =
    let%bind m_type = U.member "type" j |> string_of_yojson in
    let c = U.member "content" j in
    match String.chop_prefix_if_exists ~prefix:"m.key.verification." m_type with
    | "request" -> Request.of_yojson c >>| request
    | "cancel" -> Cancel.of_yojson c >>| cancel
    | "start" when is_sas c -> StartSAS.of_yojson c >>| start_sas
    | "start" -> Start.of_yojson c >>| start
    | "accept" -> Accept.of_yojson c >>| accept
    | "key" -> Key.of_yojson c >>| key
    | "mac" -> Mac.of_yojson c >>| mac
    | m -> Result.fail ("Unknown verification type: " ^ m)

  let to_yojson t : Yojson.Safe.t =
    let content =
      match t with
      | Request e  -> Request.to_yojson e
      | Cancel e   -> Cancel.to_yojson e
      | Start e    -> Start.to_yojson e
      | StartSAS e -> StartSAS.to_yojson e
      | Accept e   -> Accept.to_yojson e
      | Key e      -> Key.to_yojson e
      | Mac e      -> Mac.to_yojson e
    in
    `Assoc [ "type", `String (to_m_type t); "content", content ]
end

type content =
  | NewDevice of NewDevice.t
  | RoomKey of RoomKey.t
  | RoomKeyRequest of RoomKeyRequest.t
  | ForwardedRoomKey of ForwardedRoomKey.t
  | Dummy of Dummy.t
  | KeyVerification of KeyVerification.t
  | Unknown of Yojson.Safe.t

let new_device e = NewDevice e
let room_key e = RoomKey e
let room_key_request e = RoomKeyRequest e
let forwarded_room_key e = ForwardedRoomKey e
let dummy e = Dummy e
let key_verification e = KeyVerification e
let unknown e = Unknown e

type t =
  { sender : string
  ; m_type : string
  ; content : content
  }

let to_m_type = function
  | NewDevice _        -> "m.new_device"
  | RoomKey _          -> "m.room_key"
  | RoomKeyRequest _   -> "m.room_key_request"
  | ForwardedRoomKey _ -> "m.forwarded_room_key"
  | Dummy _            -> "m.dummy"
  | Unknown _          -> "unknown"
  | KeyVerification e  -> KeyVerification.to_m_type e

let is_key_veri m = String.is_prefix m ~prefix:"m.key.verification."

let of_yojson j =
  let%bind m_type = U.member "type" j |> string_of_yojson in
  let%bind sender = U.member "sender" j |> string_of_yojson in
  begin
    match m_type with
    | m when is_key_veri m -> KeyVerification.of_yojson j >>| key_verification
    | "m.new_device" -> NewDevice.of_yojson j >>| new_device
    | "m.room_key" -> RoomKey.of_yojson j >>| room_key
    | "m.room_key_request" -> RoomKeyRequest.of_yojson j >>| room_key_request
    | "m.forwarded_room_key" -> ForwardedRoomKey.of_yojson j >>| forwarded_room_key
    | "m.dummy" -> Dummy.of_yojson j >>| dummy
    (* | s                      -> Result.fail ("Invalid event type: " ^ s) *)
    | _ -> Result.return j >>| unknown
  end
  >>| fun content -> { sender; m_type; content }

let content_to_yojson = function
  | NewDevice e        -> NewDevice.to_yojson e
  | RoomKey e          -> RoomKey.to_yojson e
  | RoomKeyRequest e   -> RoomKeyRequest.to_yojson e
  | ForwardedRoomKey e -> ForwardedRoomKey.to_yojson e
  | Dummy e            -> Dummy.to_yojson e
  | KeyVerification e  -> KeyVerification.to_yojson e
  | Unknown e          -> e

let to_message t recipient recipient_device : Yojson.Safe.t =
  `Assoc
    [ "messages", `Assoc [ recipient, `Assoc [ recipient_device, content_to_yojson t ] ] ]
