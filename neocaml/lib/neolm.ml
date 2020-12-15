open Core
open Yojson_helpers
open Result.Let_syntax
open Neolm_devices
open Neolm_sessions
open Neolm_key_requests
module Sas = Neolm_sas

let olm_algorithm = "m.olm.v1.curve25519-aes-sha2"
let megolm_algorithm = "m.megolm.v1.aes-sha2"
let algorithms = [ olm_algorithm; megolm_algorithm ]
let max_sas_life = Time.Span.of_min 20.0
let unwedging_interval = Time.Span.of_min 60.0
let max_to_device_messages_per_request = 20
let message_index_store_size = 100000

(* NOTE: recieved_key_requests might need to be ToDevice msg since I might require
 * the sender. Though I could easily just have a tuple of sender and request or something
 * (or break down and make a local type for it here...) *)
type t =
  { user_id : string
  ; device_id : string
  ; account : Account.t
  ; uploaded_key_count : int option
  ; users_for_key_query : (string, String.comparator_witness) Set.t
  ; device_store : DeviceStore.t
  ; session_store : SessionStore.t
  ; inbound_group_store : InboundGroupStore.t
  ; outbound_group_sessions : OutboundGroupSession.t StringMap.t
  ; tracked_users : (string, String.comparator_witness) Set.t
  ; outgoing_key_requests : OutgoingKeyRequest.t StringMap.t
  ; received_key_requests : IncomingKeyRequest.t StringMap.t
  ; key_request_from_untrusted : IncomingKeyRequest.t StringMap.t
  }

module Payloads = struct
  let olm_algorithm = `String olm_algorithm
  let megolm_algorithm = `String megolm_algorithm
  let algorithms = `List (List.map ~f:yo_string algorithms)
  let sign_json account j = Api.canonical_json j |> Account.sign account

  let device_keys t =
    let%bind ids = Account.identity_keys t.account in
    let keys =
      [ "algorithms", algorithms
      ; "device_id", `String t.device_id
      ; "user_id", `String t.user_id
      ; ( "keys"
        , [ "curve25519:" ^ t.device_id, `String ids.curve25519
          ; "ed25519:" ^ t.device_id, `String ids.ed25519
          ]
          |> yo_assoc )
      ]
      |> yo_assoc
    in
    let%map signature = sign_json t.account keys in
    `Assoc [ "signatures", `Assoc [ "ed25519:" ^ t.device_id, `String signature ] ]
    |> U.combine keys

  let one_time_keys t =
    let f acc ~key ~data =
      let%map signature = sign_json t.account (`Assoc [ "key", `String data ]) in
      let signatures =
        [ t.user_id, [ "ed25519:" ^ t.device_id, `String signature ] |> yo_assoc ]
        |> yo_assoc
      in
      ( "signed_curve25519:" ^ key
      , [ "key", `String data; "signatures", signatures ] |> yo_assoc )
      :: acc
    in
    let%bind { curve25519 = keys } = Account.one_time_keys t.account in
    Neolm_utils.map_fold_result ~init:[] ~f keys >>| yo_assoc

  let share_keys t =
    if t.account.shared
    then (
      let%map _ = Account.top_up_one_time_keys t.account 0
      and one_time = one_time_keys t
      and dev = device_keys t in
      `Assoc [ "device_keys", dev; "one_time_keys", one_time ] )
    else (
      let%bind count =
        Result.of_option
          t.uploaded_key_count
          ~error:(`Protocol "The uploaded key count is not known.")
      in
      let%map _ = Account.top_up_one_time_keys t.account count
      and one_time = one_time_keys t in
      `Assoc [ "one_time_keys", one_time ] )
end

(* Needs Room implemented first. *)
let update_tracked_users = ()

let add_changed_users t users =
  { t with users_for_key_query = Set.union t.users_for_key_query users }

let should_upload_keys t = not (Set.is_empty t)

let user_fully_verified t user_id =
  DeviceStore.active_user_devices t.device_store user_id
  |> function
  | None    -> false
  | Some ds ->
    (* NOTE: This is a bit different that nio (it looked weird). Check back! *)
    Sequence.for_all ds ~f:(fun d -> Device.verified d && (not @@ Device.blacklisted d))

let olm_encrypt t session (recipient_device : Device.t) message =
  let%bind keys = Olm.Account.identity_keys t.account.acc in
  let payload =
    `Assoc
      [ "sender", `String t.user_id
      ; "sender_device", `String t.device_id
      ; "keys", `Assoc [ "ed25519", `String keys.ed25519 ]
      ; "recipient", `String recipient_device.user_id
      ; "recipient_keys", `Assoc [ "ed25519", `String recipient_device.keys.ed25519 ]
      ; "type", `String (Event.Room.Content.to_m_type message)
      ; "content", Event.Room.Content.to_yojson message
      ]
  in
  (* TODO: SAVE SESSION IN DB AFTER ENCRYPT*)
  let%map olm_message = Session.encrypt session (Api.canonical_json payload) in
  (* NOTE: Not positive that this should be int. Should add a direct to int to the
   * Olm code though *)
  let msg_type = if Olm.Session.Message.is_pre_key olm_message then 0 else 1 in
  `Assoc
    [ "algorithm", `String olm_algorithm
    ; "sender_key", `String keys.curve25519
    ; ( "ciphertext"
      , `Assoc
          [ ( recipient_device.keys.curve25519
            , `Assoc
                [ "type", `Int msg_type
                ; "body", `String (Olm.Session.Message.ciphertext olm_message)
                ] )
          ] )
    ]

let queue_dummy_message = ()
let handle_to_device_event = ()
let handle_key_requests = ()
let encrypt_forwarding_key = ()
let reshare_key = ()
let share_with_ourselves = ()

let get_active_key_requests t user_id device_id =
  Map.data t.key_request_from_untrusted
  |> List.filter ~f:(fun e ->
         String.equal e.sender user_id
         && String.equal e.request.content.requesting_device_id device_id)

let continue_key_share = ()

let cancel_key_share t (event : IncomingKeyRequest.t) =
  { t with
    key_request_from_untrusted =
      Map.remove t.key_request_from_untrusted event.request.content.request_id
  }

let collect_single_key_share = ()
let collect_key_requests = ()
let handle_key_claiming = ()
let handle_key_query = ()
let mark_to_device_message_as_sent = ()
let handle_response = ()
let create_inbound_session = ()
let blacklist_device = ()
let unblacklist_device = ()
let verify_device = ()
let unverify_device = ()
let ignore_device = ()
let unignore_device = ()
let is_device_ignored = ()
let create_session = ()
let create_group_session = ()
let create_outbound_group_session = ()
let get_missing_sessions = ()
let get_users_for_key_claiming = ()
let mark_device_for_unwedging = ()
let try_decrypt = ()
let verify_olm_payload = ()
let handle_room_key_event = ()
let handle_forwarded_room_key_event = ()
let handle_olm_event = ()
let message_index_ok = ()
let check_if_wedged = ()
let decrypt_megolm_no_error = ()
let decrypt_megolm_event = ()
let decrypt_event = ()
let decrypt = ()
let rotate_outbound_group_session = ()
let should_share_group_session = ()
let group_encrypt = ()
let share_group_session_parallel = ()
let share_group_session = ()
let load = ()
let save_session = ()
let save_inbound_group_session = ()
let save_account = ()
let sign_json = ()
let verify_json = ()
let mark_keys_as_published = ()
let export_keys_static = ()
let export_keys = ()
let _import_group_session = ()
let import_keys_static = ()
let import_keys = ()
let clear_verifications = ()
let create_sas = ()
let get_active_sas = ()
let handle_key_verification = ()
