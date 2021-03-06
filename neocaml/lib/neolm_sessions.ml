open! Core
open Result.Let_syntax
open Yojson_helpers

module Account = struct
  type t =
    { acc : Olm.Account.t
    ; shared : bool
    }

  let pickle ?pass t = Olm.Account.pickle ?pass t.acc

  let from_pickle ?(shared = false) ?pass pickle =
    Olm.Account.from_pickle ?pass pickle >>| fun acc -> { acc; shared }

  let create ?(shared = false) () = Olm.Account.create () >>| fun acc -> { acc; shared }
  let identity_keys t = Olm.Account.identity_keys t.acc
  let sign t msg = Olm.Account.sign t.acc msg
  let max_one_time_keys t = Olm.Account.max_one_time_keys t.acc
  let mark_keys_as_published t = Olm.Account.mark_keys_as_published t.acc
  let generate_one_time_keys t n = Olm.Account.generate_one_time_keys t.acc n
  let one_time_keys t = Olm.Account.one_time_keys t.acc

  let top_up_one_time_keys t uploaded_key_count =
    let%bind max_keys = max_one_time_keys t in
    let n = (max_keys / 2) - uploaded_key_count in
    if n <= 0
    then Result.fail (`Protocol "Can't share any keys, too many already shared.")
    else generate_one_time_keys t n
end

module Session = struct
  (* NOTE: Not decided on whether I want to do mutable just to avoid having to
   * return a new t to update use_time... *)
  type t =
    { ses : Olm.Session.t
    ; id : string
    ; creation_time : Time.t
    ; mutable use_time : Time.t
    }

  let create_inbound ?identity_key (acc : Account.t) msg =
    let%bind ses = Olm.Session.create_inbound ?identity_key acc.acc msg in
    let%map id = Olm.Session.id ses in
    { ses; id; creation_time = Time.now (); use_time = Time.now () }

  let create_outbound (acc : Account.t) identity_key one_time_key =
    let%bind ses = Olm.Session.create_outbound acc.acc identity_key one_time_key in
    let%map id = Olm.Session.id ses in
    { ses; id; creation_time = Time.now (); use_time = Time.now () }

  let pickle ?pass t = Olm.Session.pickle ?pass t.ses

  let from_pickle ?pass ?use_time creation_time pickle =
    let%bind ses = Olm.Session.from_pickle ?pass pickle in
    let%map id = Olm.Session.id ses in
    let use_time = Option.value ~default:creation_time use_time in
    { ses; id; creation_time; use_time }

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

  let equal a b = phys_equal a.ses.ses b.ses.ses
end

module OutboundGroupSession = struct
  (* NOTE: Not decided on whether I want to do mutable just to avoid having to
   * return a new t to update message_count (etc?)... *)
  type t =
    { ogs : Olm.OutboundGroupSession.t
    ; id : string
    ; creation_time : Time.t
    ; mutable message_count : int
    ; users_shared_with : (string, String.comparator_witness) Set.t
    ; users_ignored : (string, String.comparator_witness) Set.t
    ; shared : bool
    }

  let max_age = Time.Span.of_day 7.0
  let max_messages = 100

  let create () =
    let%bind ogs = Olm.OutboundGroupSession.create () in
    let%map id = Olm.OutboundGroupSession.id ogs in
    { ogs
    ; id
    ; creation_time = Time.now ()
    ; message_count = 0
    ; users_shared_with = Set.empty (module String)
    ; users_ignored = Set.empty (module String)
    ; shared = false
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
    let%bind ogs = Olm.OutboundGroupSession.from_pickle ?pass pickle in
    let%map id = Olm.OutboundGroupSession.id ogs in
    { ogs; id; creation_time; message_count; users_shared_with; users_ignored; shared }

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
  type t =
    { igs : Olm.InboundGroupSession.t
    ; id : string
    ; ed25519 : string
    ; sender_key : string
    ; room_id : string
    ; forwarding_chain : string list
    }

  let create ?(forwarding_chain = []) outbound_session_key signing_key sender_key room_id =
    let%bind igs = Olm.InboundGroupSession.create outbound_session_key in
    let%map id = Olm.InboundGroupSession.id igs in
    { igs; id; ed25519 = signing_key; sender_key; room_id; forwarding_chain }

  let pickle ?pass t = Olm.InboundGroupSession.pickle ?pass t.igs

  let from_pickle ?(forwarding_chain = []) ?pass pickle signing_key sender_key room_id =
    let%bind igs = Olm.InboundGroupSession.from_pickle ?pass pickle in
    let%map id = Olm.InboundGroupSession.id igs in
    { igs; id; ed25519 = signing_key; sender_key; room_id; forwarding_chain }

  let decrypt ?ignore_unicode_errors t ciphertext =
    Olm.InboundGroupSession.decrypt ?ignore_unicode_errors t.igs ciphertext

  let id t = Olm.InboundGroupSession.id t.igs
  let first_known_index t = Olm.InboundGroupSession.first_known_index t.igs

  let export_session t message_index =
    Olm.InboundGroupSession.export_session t.igs message_index

  let import_session ?(forwarding_chain = []) exported_key signing_key sender_key room_id =
    let%bind igs = Olm.InboundGroupSession.import_session exported_key in
    let%map id = Olm.InboundGroupSession.id igs in
    { igs; id; ed25519 = signing_key; sender_key; room_id; forwarding_chain }

  let is_verified t = Olm.InboundGroupSession.is_verified t.igs
end

module SessionStore = struct
  type t = Session.t list StringMap.t

  let create sender session = StringMap.add_exn StringMap.empty ~key:sender ~data:session

  let use_time_sort (l : Session.t list) =
    List.sort ~compare:(fun a b -> -1 * Time.compare a.use_time b.use_time) l

  let add t sender session =
    let%map session_list =
      match Map.find t sender with
      | None   -> Result.return []
      | Some l ->
        if List.mem l ~equal:Session.equal session
        then Result.fail `SessionAlreadyStored
        else Result.return (use_time_sort l)
    in
    Map.set t ~key:sender ~data:(session :: session_list)

  let sessions t = List.join (Map.data t)
  let get t sender_key = Map.find t sender_key |> Option.map ~f:List.hd
end

module InboundGroupStore = struct
  type t = InboundGroupSession.t StringMap.t StringMap.t StringMap.t

  let create (igs : InboundGroupSession.t) =
    Map.add_exn
      StringMap.empty
      ~key:igs.room_id
      ~data:
        (Map.add_exn
           StringMap.empty
           ~key:igs.sender_key
           ~data:(Map.add_exn StringMap.empty ~key:igs.id ~data:igs))

  let add t (igs : InboundGroupSession.t) =
    let senders_opt = Map.find t igs.room_id in
    Option.bind ~f:(fun m -> Map.find m igs.sender_key) senders_opt
    |> Option.value ~default:StringMap.empty
    |> Map.add ~key:igs.id ~data:igs
    |> function
    | `Duplicate   -> Result.fail `Duplicate
    | `Ok sessions ->
      let senders =
        Option.value_map
          ~default:StringMap.empty
          ~f:(fun m -> Map.set m ~key:igs.sender_key ~data:sessions)
          senders_opt
      in
      Result.return @@ Map.set t ~key:igs.room_id ~data:senders

  let sessions t =
    let collect_data ~f acc m = Map.data m |> List.fold ~init:acc ~f in
    List.fold
      (Map.data t)
      ~init:[]
      ~f:(collect_data ~f:(collect_data ~f:(fun acc s -> s :: acc)))

  let get t room_id sender_key session_id =
    let%bind.Option senders = Map.find t room_id in
    let%bind.Option sessions = Map.find senders sender_key in
    Map.find sessions session_id
end
