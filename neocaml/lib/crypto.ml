open! Core

(* Not final anything for this module, but wanted to paste these definition
 * placeholders somewhere other than Client. Each of them calls Client.to_device
 * after fetching state such as pending messages, ongoing verifications, Olm
 * keys etc. Not sure that I want to be doing that when the client is otherwise
 * "pure" so far. They might go back in there yet, or become part of the future
 * Olm module itself, but I haven't thought about it enough yet.
 *
 * Perhaps they should go in the Client module, so that everything which makes
 * calls to the client are localized there. It will be simple to move them
 * back in though once I am decided. This way they are collected for now. Some
 * of these are also dependent on the stored representations of Rooms, which
 * may still end up living in Client.t. Since I have not actually done anything
 * with those placeholders yet, it's even more important that I make my decisions
 * regarding in-memory and persistent (stored) state before moving forward. *)

let share_group_session = ()

let request_room_key = ()

let export_keys = ()

let import_keys = ()

let start_key_verification = ()

let cancel_key_verification = ()

let accept_key_verification = ()

let confirm_short_auth_string = ()
