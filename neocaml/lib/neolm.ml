open Core
open Yojson_helpers
open! Result.Monad_infix

open Neolm_devices
open Neolm_sessions
open Neolm_key_requests
module Sas = Neolm_sas

let olm_algorithm                      = "m.olm.v1.curve25519-aes-sha2"
let megolm_algorithm                   = "m.megolm.v1.aes-sha2"
let algorithms                         = [ olm_algorithm; megolm_algorithm ]
let max_sas_life                       = Time.Span.of_min 20.0
let unwedging_interval                 = Time.Span.of_min 60.0
let max_to_device_messages_per_request = 20
let message_index_store_size           = 100000

(* NOTE: recieved_key_requests might need to be ToDevice msg since I might require
 * the sender. Though I could easily just have a tuple of sender and request or something
 * (or break down and make a local type for it here...) *)
type t = { user_id                 : string
         ; device_id               : string
         ; account                 : Account.t
         ; uploaded_key_count      : int
         ; users_for_key_query     : (string, String.comparator_witness) Set.t
         ; device_store            : DeviceStore.t
         ; session_store           : SessionStore.t
         ; inbound_group_store     : InboundGroupStore.t
         ; outbound_group_sessions : OutboundGroupSession.t StringMap.t
         ; tracked_users           : (string, String.comparator_witness) Set.t
         ; outgoing_key_requests   : OutgoingKeyRequest.t StringMap.t
         ; received_key_requests   : IncomingKeyRequest.t StringMap.t
         }

module Payloads = struct
  let olm_algorithm    = `String olm_algorithm
  let megolm_algorithm = `String megolm_algorithm
  let algorithms       = `List (List.map ~f:yo_string algorithms)

  let sign_json account j = Api.canonical_json j |> Account.sign account

  let device_keys t =
    Account.identity_keys t.account >>= fun ids ->
    let keys = [ ("algorithms", algorithms)
               ; ("device_id", `String t.device_id)
               ; ("user_id", `String t.user_id)
               ; ("keys", [ ("curve25519:" ^ t.device_id, `String ids.curve25519)
                          ; ("ed25519:" ^ t.device_id, `String ids.ed25519)
                          ] |> yo_assoc)
               ] |> yo_assoc
    in
    sign_json t.account keys >>| fun signature ->
    `Assoc [
      ("signatures", `Assoc [
          ("ed25519:" ^ t.device_id, `String signature) ]) ]
    |> U.combine keys

  let one_time_keys t =
    let f acc ~key ~data =
      sign_json t.account (`Assoc [ ("key", `String data) ]) >>| fun signature ->
      let signatures = [ (t.user_id, [ ("ed25519:" ^ t.device_id, `String signature)
                                     ] |> yo_assoc)
                       ] |> yo_assoc
      in
      ("signed_curve25519:" ^ key, [ ("key", `String data)
                                   ; ("signatures", signatures)
                                   ] |> yo_assoc)
      :: acc
    in
    Account.one_time_keys t.account >>= fun { curve25519 = keys } ->
    Neolm_utils.map_fold_result ~init:[] ~f keys >>| yo_assoc
end
