open! Core
open Result.Monad_infix

module Account = struct
  type t = { acc    : Olm.Account.t
           ; shared : bool
           }

  let pickle ?pass t = Olm.Account.pickle ?pass t.acc

  let from_pickle ?(shared=false) ?pass pickle =
    Olm.Account.from_pickle ?pass pickle >>| fun acc ->
    { acc; shared }

  let create ?(shared=false) () =
    Olm.Account.create () >>| fun acc ->
    { acc; shared }

  let identity_keys t = Olm.Account.identity_keys t.acc

  let sign t msg = Olm.Account.sign t.acc msg

  let max_one_time_keys t = Olm.Account.max_one_time_keys t.acc

  let mark_keys_as_published t = Olm.Account.mark_keys_as_published t.acc

  let generate_one_time_keys t n = Olm.Account.generate_one_time_keys t.acc n

  let one_time_keys t = Olm.Account.one_time_keys t.acc
end

module Session = struct
  (* NOTE: Not decided on whether I want to do mutable just to avoid having to
   * return a new t to update use_time... *)
  type t = { ses              : Olm.Session.t
           ; creation_time    : Time.t
           ; mutable use_time : Time.t
           }

  let create_inbound ?identity_key (acc : Account.t) msg =
    Olm.Session.create_inbound ?identity_key acc.acc msg >>| fun ses ->
    { ses; creation_time = Time.now (); use_time = Time.now () }

  let create_outbound (acc : Account.t) identity_key one_time_key =
    Olm.Session.create_outbound acc.acc identity_key one_time_key >>| fun ses ->
    { ses; creation_time = Time.now (); use_time = Time.now () }

  let pickle ?pass t = Olm.Session.pickle ?pass t.ses

  let from_pickle ?pass ?use_time creation_time pickle =
    Olm.Session.from_pickle ?pass pickle >>| fun ses ->
    { ses; creation_time; use_time = Option.value ~default:creation_time use_time }

  let decrypt ?ignore_unicode_errors t msg =
    t.use_time <- Time.now ();
    Olm.Session.decrypt ?ignore_unicode_errors t.ses msg

  let encrypt t plaintext =
    t.use_time <- Time.now ();
    Olm.Session.encrypt t.ses plaintext

  let id t = Olm.Session.id t.ses

  let matches ?identity_key t = Olm.Session.matches ?identity_key t.ses

  let remove_one_time_keys t (acc : Account.t) =
    Olm.Session.remove_one_time_keys t.ses acc.acc
end

module OutboundGroupSession = struct
  (* NOTE: Not decided on whether I want to do mutable just to avoid having to
   * return a new t to update message_count (etc?)... *)
  type t = { ogs                   : Olm.OutboundGroupSession.t
           ; creation_time         : Time.t
           ; mutable message_count : int
           ; users_shared_with     : (string, String.comparator_witness) Set.t
           ; users_ignored         : (string, String.comparator_witness) Set.t
           ; shared                : bool
           }

  let max_age      = Time.Span.of_day 7.0
  let max_messages = 100

  let create () =
    Olm.OutboundGroupSession.create () >>| fun ogs ->
    { ogs
    ; creation_time     = Time.now ()
    ; message_count     = 0
    ; users_shared_with = Set.empty (module String)
    ; users_ignored     = Set.empty (module String)
    ; shared            = false
    }

  let pickle ?pass t = Olm.OutboundGroupSession.pickle ?pass t.ogs

  (* NOTE: I get the feeling this is never used, given the message restrictions. *)
  let from_pickle
      ?pass
      pickle
      creation_time
      message_count
      users_shared_with
      users_ignored
      shared
    =
    Olm.OutboundGroupSession.from_pickle ?pass pickle >>| fun ogs ->
    { ogs; creation_time; message_count; users_shared_with; users_ignored; shared }

  let encrypt t plaintext =
    t.message_count <- t.message_count + 1;
    Olm.OutboundGroupSession.encrypt t.ogs plaintext

  let id t = Olm.OutboundGroupSession.id t.ogs

  let message_index t = Olm.OutboundGroupSession.message_index t.ogs

  let session_key t = Olm.OutboundGroupSession.session_key t.ogs

  let mark_as_shared t = { t with shared = true }

  let expired t =
    t.message_count >= max_messages
    || Time.Span.(Time.(diff (now ()) t.creation_time) >=. max_age)
end

module InboundGroupSession = struct
  type t = { igs              : Olm.InboundGroupSession.t
           ; ed25519          : string
           ; sender_key       : string
           ; room_id          : string
           ; forwarding_chain : string list
           }

  let create ?(forwarding_chain=[]) outbound_session_key signing_key sender_key room_id =
    Olm.InboundGroupSession.create outbound_session_key >>| fun igs ->
    { igs; ed25519 = signing_key; sender_key; room_id; forwarding_chain }

  let pickle ?pass t = Olm.InboundGroupSession.pickle ?pass t.igs

  let from_pickle ?(forwarding_chain=[]) ?pass pickle signing_key sender_key room_id =
    Olm.InboundGroupSession.from_pickle ?pass pickle >>| fun igs ->
    { igs; ed25519 = signing_key; sender_key; room_id; forwarding_chain }

  let decrypt ?ignore_unicode_errors t ciphertext =
    Olm.InboundGroupSession.decrypt ?ignore_unicode_errors t.igs ciphertext

  let id t = Olm.InboundGroupSession.id t.igs

  let first_known_index t = Olm.InboundGroupSession.first_known_index t.igs

  let export_session t message_index =
    Olm.InboundGroupSession.export_session t.igs message_index

  let import_session ?(forwarding_chain=[]) exported_key signing_key sender_key room_id =
    Olm.InboundGroupSession.import_session exported_key >>| fun igs ->
    { igs; ed25519 = signing_key; sender_key; room_id; forwarding_chain }

  let is_verified t = Olm.InboundGroupSession.is_verified t.igs
end