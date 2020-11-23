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
