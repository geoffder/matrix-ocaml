open! Core
open Yojson_helpers
open Result.Monad_infix

let util = Olm.Utility.create ()

let bin8_of_int_exn d =
  if d < 0 || d > 255
  then failwith "Must be unsigned 8bit int."
  else
    let rec aux acc = function
      | 0 -> acc
      | d -> aux (string_of_int (d land 1) :: acc) (d lsr 1)
    in
    let bin = String.concat (aux [] d) in
    let pad = String.init ~f:(fun _ -> '0') (8 - String.length bin) in
    pad ^ bin

let int_of_bin_exn s =
  let char_to_bit = function
    | '0' -> 0
    | '1' -> 1
    | _   -> failwith "Bitstring must only consist of 0s and 1s."
  in
  String.lstrip ~drop:(Char.equal '0') s
  |> String.rev
  |> String.foldi ~init:0
    ~f:(fun i sum c -> sum + (char_to_bit c * (Int.pow 2 i)))

let map_fold_result ~init ~f m =
  with_return begin fun { return } ->
    Result.return @@
    Map.fold m ~init ~f:begin fun ~key ~data acc ->
      match f acc ~key ~data with
      | Ok r         -> r
      | Error _ as e -> return e
    end
  end

module Device = struct
  type trust_state = Unset | Verified | Blacklisted | Ignored

  type keys = { ed25519    : string
              ; curve25519 : string
              }

  type t = { user_id      : string
           ; id           : string
           ; keys         : keys
           ; display_name : string option
           ; deleted      : bool
           ; trust_state  : trust_state
           }

  let create ?display_name user_id id ed25519 curve25519 =
    { user_id
    ; id
    ; keys         = { ed25519; curve25519 }
    ; display_name
    ; deleted      = false
    ; trust_state  = Unset
    }

  let curve25519 t = t.keys.curve25519

  let ed25519 t = t.keys.ed25519

  let verified = function
    | { trust_state = Verified; _ } -> true
    | _                             -> false

  let ignored = function
    | { trust_state = Ignored; _ } -> true
    | _                            -> false

  let blacklisted = function
    | { trust_state = Blacklisted; _ } -> true
    | _                                -> false

  let active t = not t.deleted
end

module DeviceMap = struct
  type t = Device.t StringMap.t StringMap.t

  let active_user_devices t user_id =
    let open Option in
    Map.find t user_id >>|
    Map.to_sequence    >>|
    Sequence.filter_map ~f:(fun (_, d) -> if Device.active d then Some d else None)

  let device_from_sender_key t user_id sender_key =
    active_user_devices t user_id
    |> Option.bind
      ~f:(Sequence.find ~f:(fun d ->
          String.equal sender_key (Device.curve25519 d)))

  let users t = Map.keys t

  let devices t user_id = Map.find t user_id |> Option.map ~f:Map.keys

  let add t (d : Device.t) =
    let open Result in
    match Map.find t d.user_id with
    | Some u ->
      begin
        match Map.add u ~key:d.id ~data:d with
        | `Ok u      -> Result.return u
        | `Duplicate -> Result.fail `DuplicateDevice
      end >>| fun data -> Map.set t ~key:d.user_id ~data
    | None ->
      Map.add_exn t
        ~key:d.user_id
        ~data:(Map.of_alist_exn (module String) [ (d.id, d) ])
      |> Result.return
end

module Sas = struct
  module KV = ToDevice.KeyVerification

  type reason =
    [ `UserCancel
    | `Timeout
    | `UnknownTransaction
    | `UnknownMethod
    | `UnexpectedMessage
    | `KeyMismatch
    | `UserMismatch
    | `InvalidMessage
    | `CommitmentMismatch
    | `SasMismatch
    | `Unknown of string
    ]

  type error =
    [ `Reason of reason
    | `Protocol of string
    | `EmojiFail of string
    | `DecimalsFail of string
    ]

  let reason_of_code = function
    | "m.user_cancel"         -> `UserCancel
    | "m.timeout"             -> `Timeout
    | "m.unknown_transaction" -> `UnknownTransaction
    | "m.unknown_method"      -> `UnknownMethod
    | "m.unexpected_message"  -> `UnexpectedMessage
    | "m.key_mismatch"        -> `KeyMismatch
    | "m.user_error"          -> `UserMismatch
    | "m.invalid_message"     -> `InvalidMessage
    | "m.commitment_mismatch" -> `CommitmentMismatch
    | "m.sas_mismatch"        -> `SasMismatch
    | s                       -> `Unknown s

  let reason_to_code = function
    |  `UserCancel         -> "m.user_cancel"
    |  `Timeout            -> "m.timeout"
    |  `UnknownTransaction -> "m.unknown_transaction"
    |  `UnknownMethod      -> "m.unknown_method"
    |  `UnexpectedMessage  -> "m.unexpected_message"
    |  `KeyMismatch        -> "m.key_mismatch"
    |  `UserMismatch       -> "m.user_error"
    |  `InvalidMessage     -> "m.invalid_message"
    |  `CommitmentMismatch -> "m.commitment_mismatch"
    |  `SasMismatch        -> "m.sas_mismatch"
    |  `Unknown s          -> s

  let reason_to_string = function
    |  `UserCancel         -> "Canceled by user"
    |  `Timeout            -> "Timed out"
    |  `UnknownTransaction -> "Unknown transaction"
    |  `UnknownMethod      -> "Unknown method"
    |  `UnexpectedMessage  -> "Unexpected message"
    |  `KeyMismatch        -> "Key mismatch"
    |  `UserMismatch       -> "User mismatch"
    |  `InvalidMessage     -> "Invalid message"
    |  `CommitmentMismatch -> "Mismatched commitment"
    |  `SasMismatch        -> "Mismatched short authentication string"
    |  `Unknown s          -> "Unknown reason: " ^ s

  type state =
    | Created
    | Started
    | Accepted
    | KeyReceived
    | MacReceived
    | Canceled of reason

  let emoji =
    [ ("🐶", "Dog")
    ; ("🐱", "Cat")
    ; ("🦁", "Lion")
    ; ("🐎", "Horse")
    ; ("🦄", "Unicorn")
    ; ("🐷", "Pig")
    ; ("🐘", "Elephant")
    ; ("🐰", "Rabbit")
    ; ("🐼", "Panda")
    ; ("🐓", "Rooster")
    ; ("🐧", "Penguin")
    ; ("🐢", "Turtle")
    ; ("🐟", "Fish")
    ; ("🐙", "Octopus")
    ; ("🦋", "Butterfly")
    ; ("🌷", "Flower")
    ; ("🌳", "Tree")
    ; ("🌵", "Cactus")
    ; ("🍄", "Mushroom")
    ; ("🌏", "Globe")
    ; ("🌙", "Moon")
    ; ("☁️", "Cloud")
    ; ("🔥", "Fire")
    ; ("🍌", "Banana")
    ; ("🍎", "Apple")
    ; ("🍓", "Strawberry")
    ; ("🌽", "Corn")
    ; ("🍕", "Pizza")
    ; ("🎂", "Cake")
    ; ("❤️", "Heart")
    ; ("😀", "Smiley")
    ; ("🤖", "Robot")
    ; ("🎩", "Hat")
    ; ("👓", "Glasses")
    ; ("🔧", "Wrench")
    ; ("🎅", "Santa")
    ; ("👍", "Thumbs up")
    ; ("☂️", "Umbrella")
    ; ("⌛", "Hourglass")
    ; ("⏰", "Clock")
    ; ("🎁", "Gift")
    ; ("💡", "Light Bulb")
    ; ("📕", "Book")
    ; ("✏️", "Pencil")
    ; ("📎", "Paperclip")
    ; ("✂️", "Scissors")
    ; ("🔒", "Lock")
    ; ("🔑", "Key")
    ; ("🔨", "Hammer")
    ; ("☎️", "Telephone")
    ; ("🏁", "Flag")
    ; ("🚂", "Train")
    ; ("🚲", "Bicycle")
    ; ("✈️", "Airplane")
    ; ("🚀", "Rocket")
    ; ("🏆", "Trophy")
    ; ("⚽", "Ball")
    ; ("🎸", "Guitar")
    ; ("🎺", "Trumpet")
    ; ("🔔", "Bell")
    ; ("⚓", "Anchor")
    ; ("🎧", "Headphones")
    ; ("📁", "Folder")
    ; ("📌", "Pin")
    ] |> List.mapi ~f:(fun i e -> (i, e))
    |> Map.of_alist_exn (module Int)

  let sas_vers          = "m.sas.v1"
  let hash_v1           = "sha256"
  let max_age           = Time.Span.of_min 5.0
  let max_event_timeout = Time.Span.of_min 1.0

  type t = { sas                     : Olm.Sas.t
           ; own_user                : string
           ; own_device              : string
           ; own_fp_key              : string
           ; other_olm_device        : Device.t
           ; transaction_id          : string
           ; sas_methods             : KV.SasMethod.t list
           ; mac_methods             : KV.MacMethod.t list
           ; chosen_mac_method       : KV.MacMethod.t option
           ; key_agreement_protocols : KV.KeyAgreementProtocol.t list
           ; chosen_key_agreement    : KV.KeyAgreementProtocol.t option
           ; state                   : state
           ; we_started_it           : bool
           ; sas_accepted            : bool
           ; commitment              : string option
           ; their_sas_key           : string option
           ; verified_devices        : string list
           ; creation_time           : Time.t
           ; last_event_time         : Time.t
           }

  let create
      ?tx_id
      ?(sas_methods=[ KV.SasMethod.Emoji; Decimal ])
      ?(mac_methods=[ KV.MacMethod.Normal; Old ])
      ?(key_agreement_protocols=[ KV.KeyAgreementProtocol.V1; V2 ])
      ?(we_started_it=true)
      own_user
      own_device
      own_fp_key
      other_olm_device
    =
    Olm.Sas.create () >>| fun sas ->
    let transaction_id =
      Option.value ~default:(Uuid_unix.create () |> Uuid.to_string) tx_id in
    let now = Time.now () in
    { sas                     = sas
    ; own_user
    ; own_device
    ; own_fp_key
    ; other_olm_device
    ; transaction_id
    ; sas_methods
    ; mac_methods
    ; chosen_mac_method       = None
    ; key_agreement_protocols
    ; chosen_key_agreement    = None
    ; state                   = Created
    ; we_started_it
    ; sas_accepted            = false
    ; commitment              = None
    ; their_sas_key           = None
    ; verified_devices        = []
    ; creation_time           = now
    ; last_event_time         = now
    }

  let from_key_verification_start
      own_user
      own_device
      own_fp_key
      other_olm_device
      (event : ToDevice.KeyVerification.StartSAS.t)
    =
    create
      ~tx_id:event.transaction_id
      ~we_started_it:false
      ~sas_methods:event.short_authentication_string
      ~mac_methods:event.message_authentication_codes
      ~key_agreement_protocols:event.key_agreement_protocols
      own_user
      own_device
      own_fp_key
      other_olm_device   >>= fun t ->
    Olm.Sas.pubkey t.sas >>= fun pubkey ->
    ToDevice.KeyVerification.StartSAS.to_yojson event
    |> Api.canonical_json
    |> ( ^ )  pubkey
    |> Olm.Utility.sha256 util >>| fun commitment ->
    { t with commitment = Some commitment }

  let canceled = function
    | { state = Canceled _; _ } -> true
    | _                         -> false

  let verified = function
    | { state = MacReceived; sas_accepted = true; _ } -> true
    | _                                               -> false

  let created = function
    | { state = Created; _} -> true
    | _                     -> false

  let cancel_reason = function
    | { state = Canceled reason; _ } -> Some reason
    | _                              -> None

  let supports_normal_mac t =
    List.exists t.mac_methods ~f:(function | Normal -> true | _ -> false)

  let supports_agreement_v2 t =
    List.exists t.key_agreement_protocols ~f:(function | V2 -> true | _ -> false)

  let apply_sas_error t = function
    | Ok _ as ok        -> ok
    | Error (`Reason r) -> Result.return { t with state = Canceled r }
    | Error _ as err    -> err

  let event_to_message t e =
    ToDevice.to_message e t.other_olm_device.user_id t.other_olm_device.id

  let time_out t =
    if verified t || canceled t
    then false, t
    else
      let n = Time.now () in
      if Time.Span.(Time.diff n t.creation_time >=. max_age
                    || Time.diff n t.last_event_time >=. max_event_timeout)
      then true, { t with state = Canceled `Timeout }
      else false, t

  let set_their_pubkey t pubkey =
    Olm.Sas.set_their_pubkey t.sas pubkey >>| fun _ ->
    { t with their_sas_key = Some pubkey }

  let accept_sas = function
    | { state = Canceled _; _ }   -> Result.fail (`Protocol "Already canceled.")
    | { their_sas_key = None; _ } -> Result.fail (`Protocol "Other key not even set.")
    | t                           -> Result.return { t with sas_accepted = true }

  let reject_sas = function
    | { their_sas_key = None; _ } -> Result.fail (`Protocol "Other key not even set.")
    | t -> Result.return { t with state = Canceled `SasMismatch }

  let cancel t = { t with state = Canceled `UserCancel }

  let grouper ?filler n str =
    let f i acc c =
      let s = String.of_char c in
      acc ^ if i > 0 && i mod n = 0 then " " ^ s else s in
    let padding =
      match String.length str mod n with
      | 0   -> ""
      | off -> Option.value_map filler ~default:""
                 ~f:(fun c -> String.init (n - off) ~f:(fun _ -> c))
    in
    String.foldi str ~f ~init:"" ^ padding
    |> String.split ~on:' '

  let extra_info_v1 t =
    let dev        = t.other_olm_device in
    let tx_id      = t.transaction_id in
    let our_info   = sprintf "%s%s" t.own_user t.own_device in
    let their_info = sprintf "%s%s" dev.user_id dev.id in
    if t.we_started_it
    then sprintf "MATRIX_KEY_VERIFICATION_SAS%s%s%s" our_info their_info tx_id
    else sprintf "MATRIX_KEY_VERIFICATION_SAS%s%s%s" their_info our_info tx_id

  let extra_info_v2 t =
    Result.of_option t.their_sas_key
      ~error:(`Protocol "Their SAS key is not set.") >>= fun their_key ->
    Olm.Sas.pubkey t.sas                             >>| fun pubkey ->
    let dev        = t.other_olm_device in
    let tx_id      = t.transaction_id in
    let our_info   = sprintf "%s|%s|%s" t.own_user t.own_device pubkey in
    let their_info = sprintf "%s|%s|%s" dev.user_id dev.id their_key in
    if t.we_started_it
    then sprintf "MATRIX_KEY_VERIFICATION_SAS|%s|%s|%s" our_info their_info tx_id
    else sprintf "MATRIX_KEY_VERIFICATION_SAS|%s|%s|%s" their_info our_info tx_id

  let extra_info t =
    match t.chosen_key_agreement with
    | None    -> Result.fail (`Protocol "No key agreement chosen.")
    | Some V2 -> extra_info_v2 t
    | Some V1 -> Result.return (extra_info_v1 t)

  let get_emojis t =
    extra_info t >>= fun info ->
    Olm.Sas.generate_bytes t.sas info 6 >>= fun bytes ->
    try
      Bytes.of_string bytes
      |> Bytes.fold ~init:"" ~f:(fun acc c ->
          acc ^ (Char.to_int c |> bin8_of_int_exn))
      |> Fn.flip String.prefix @@ 42
      |> grouper 6
      |> List.map ~f:(fun bits -> int_of_bin_exn bits |> Map.find_exn emoji)
      |> Result.return
    with e -> Result.fail (`EmojiFail (Exn.to_string e))

  let get_decimals t =
    extra_info t >>= fun info ->
    Olm.Sas.generate_bytes t.sas info 5 >>= fun bytes ->
    try
      Bytes.of_string bytes
      |> Bytes.fold ~init:"" ~f:(fun acc c ->
          acc ^ (Char.to_int c |> bin8_of_int_exn))
      |> Fn.flip String.drop_suffix @@ 1
      |> grouper 13
      |> List.map ~f:(fun bits -> int_of_bin_exn bits + 1000)
      |> Result.return
    with e -> Result.fail (`DecimalsFail (Exn.to_string e))

  let start_verification_event t =
    Result.ok_if_true t.we_started_it
      ~error:(`Protocol "Not started by us, can't send.") >>= fun () ->
    Result.ok_if_true (not @@ canceled t)
      ~error:(`Protocol "Verification was canceled, can't send.") >>= fun () ->
    ToDevice.KeyVerification
      (StartSAS
         { transaction_id                = t.transaction_id
         ; v_method                      = sas_vers
         ; key_agreement_protocols       = t.key_agreement_protocols
         ; hashes                        = [ hash_v1 ]
         ; message_authentication_codes  = t.mac_methods
         ; short_authentication_string   = t.sas_methods
         })
    |> Result.return

  let start_verification_message t =
    start_verification_event t >>| event_to_message t

  let accept_verification_event t =
    Result.ok_if_true (not t.we_started_it)
      ~error:(`Protocol "Started by us, can't accept offer.") >>= fun () ->
    Result.ok_if_true (not @@ canceled t)
      ~error:(`Protocol "Verification was canceled, can't accept offer.") >>= fun () ->
    Result.of_option t.commitment
      ~error:(`Protocol "No commitment, can't accept offer.") >>= fun commitment ->
    let chosen_mac = if supports_normal_mac t then KV.MacMethod.Normal else Old in
    let chosen_agreement =
      if supports_agreement_v2 t then KV.KeyAgreementProtocol.V2 else V1 in
    ToDevice.KeyVerification
      (Accept { transaction_id              = t.transaction_id
              ; v_method                    = sas_vers
              ; key_agreement_protocol      = chosen_agreement
              ; hash                        = hash_v1
              ; message_authentication_code = chosen_mac
              ; short_authentication_string = t.sas_methods
              ; commitment
              })
    |> Result.return

  let accept_verification_message t =
    accept_verification_event t >>| event_to_message t

  let check_commitment t key =
    match t.commitment with
    | None   -> Result.fail (`Protocol "No existing commitment.")
    | Some c ->
      accept_verification_event t >>= fun event ->
      let content = ToDevice.content_to_yojson event
                    |> Yojson.Safe.Util.member "content"
                    |> Api.canonical_json
      in
      Olm.Utility.sha256 util (key ^ content) >>= fun sha ->
      Result.ok_if_true (String.equal c sha) ~error:(`Reason `CommitmentMismatch)

  let share_key_event t =
    Result.ok_if_true (not @@ canceled t)
      ~error:(`Protocol "Verification was canceled, can't share key.") >>= fun () ->
    Olm.Sas.pubkey t.sas >>= fun key ->
    ToDevice.KeyVerification (Key { transaction_id = t.transaction_id; key })
    |> Result.return

  let share_key_message t = share_key_event t >>| event_to_message t

  let mac_event t =
    Result.ok_if_true t.sas_accepted
      ~error:(`Protocol "SAS string wasn't yet accepted.") >>= fun () ->
    Result.ok_if_true (canceled t)
      ~error:(`Protocol "Verification was canceled, can't generate MAC") >>= fun () ->
    Result.of_option t.chosen_mac_method
      ~error:(`Protocol "No MAC method has been chosen yet.") >>= fun mac_method ->
    let key_id = "ed25519:" ^ t.own_device in
    let info   =
      sprintf "MATRIX_KEY_VERIFICATION_MAC%s%s%s%s%s"
        t.own_user                 t.own_device
        t.other_olm_device.user_id t.other_olm_device.id
        t.transaction_id
    in
    let calc =
      match mac_method with
      | Normal -> Olm.Sas.calculate_mac
      | Old    -> Olm.Sas.calculate_mac_long_kdf
    in
    calc t.sas t.own_fp_key (info ^ key_id) >>= fun mac ->
    calc t.sas key_id (info ^ "KEY_IDS")    >>= fun keys ->
    ToDevice.KeyVerification
      (Mac
         { transaction_id = t.transaction_id
         ; mac            = Map.of_alist_exn (module String) [ ("key_id", mac) ]
         ; keys
         })
    |> Result.return

  let mac_message t = mac_event t >>| event_to_message t

  let cancellation_event t =
    Result.of_option (cancel_reason t)
      ~error:(`Protocol "Sas process isn't cancelled.") >>= fun err ->
    ToDevice.KeyVerification
      (Cancel
         { transaction_id = t.transaction_id
         ; code           = reason_to_code err
         ; reason         = reason_to_string err
         })
    |> Result.return

  let cancellation_message t = cancellation_event t >>| event_to_message t

  let event_ok t sender tx_id =
    Result.ok_if_true (canceled t) ~error:(`Protocol "Already canceled") >>= fun () ->
    Result.ok_if_true (String.equal t.transaction_id tx_id)
      ~error:(`Reason `UnknownTransaction) >>= fun () ->
    Result.ok_if_true (String.equal t.other_olm_device.user_id sender)
      ~error:(`Reason `UserMismatch)

  let receive_accept_event t sender (event : ToDevice.KeyVerification.Accept.t) =
    begin
      event_ok t sender event.transaction_id >>= fun () ->
      Result.ok_if_true (created t) ~error:(`Reason `UnexpectedMessage) >>= fun () ->
      Result.ok_if_true ~error:(`Reason `UnknownMethod) begin
        List.mem
          t.key_agreement_protocols
          event.key_agreement_protocol
          ~equal:KV.KeyAgreementProtocol.equal
        && String.equal event.hash hash_v1
        && List.mem
          t.mac_methods
          event.message_authentication_code
          ~equal:KV.MacMethod.equal
        && List.length event.short_authentication_string > 0
      end >>= fun () ->
      { t with
        commitment           = Some event.commitment
      ; chosen_mac_method    = Some event.message_authentication_code
      ; chosen_key_agreement = Some event.key_agreement_protocol
      ; sas_methods          = event.short_authentication_string
      ; state                = Accepted
      } |> Result.return
    end |> apply_sas_error t

  let receive_key_event t sender (event : ToDevice.KeyVerification.Key.t) =
    begin
      Result.ok_if_true ~error:(`Reason `UnexpectedMessage) begin
        Option.is_none t.their_sas_key
        && (t.state |> function Accepted | Started -> true | _ -> false)
      end >>= fun () ->
      event_ok t sender event.transaction_id >>= fun () ->
      begin
        if t.we_started_it
        then check_commitment t event.key
        else Result.return ()
      end >>= fun () ->
      set_their_pubkey t event.key >>| fun t ->
      { t with state = KeyReceived }
    end |> apply_sas_error t

  let verify_devices t calc_mac mac_map =
    let id_split key_id =
      match String.split ~on:':' key_id with
      | [ key_type; device_id ] -> Result.return (key_type, device_id)
      | _                       -> Result.fail (`Reason `InvalidMessage)
    in
    let verify devs ~key:key_id ~data:key_mac =
      id_split key_id >>= fun (key_type, device_id) ->
      Result.ok_if_true (String.equal key_type "ed25519")
        ~error:(`Reason `KeyMismatch) >>= fun () ->
      if String.equal device_id t.other_olm_device.id then
        calc_mac t.other_olm_device.keys.ed25519 >>= fun mac ->
        Result.ok_if_true ~error:(`Reason `KeyMismatch)
          (String.equal key_mac mac) >>| fun () ->
        device_id :: devs
      else Result.return devs
    in
    map_fold_result ~init:[] ~f:verify mac_map >>= fun devices ->
    Result.ok_if_true (List.length devices > 0) ~error:(`Reason `KeyMismatch) >>| fun () ->
    { t with verified_devices = devices }

  let receive_mac_event t sender (event : ToDevice.KeyVerification.Mac.t) =
    begin
      if verified t then Result.return t else
        event_ok t sender event.transaction_id >>= fun () ->
        Result.ok_if_true (t.state |> function KeyReceived -> true | _ -> false)
          ~error:(`Reason `UnexpectedMessage) >>= fun () ->
        Result.of_option t.chosen_mac_method
          ~error:(`Protocol "No MAC method has been chosen yet.") >>= fun mac_method ->
        let info =
          sprintf "MATRIX_KEY_VERIFICATION_MAC%s%s%s%s%s"
            t.other_olm_device.user_id t.other_olm_device.id
            t.own_user                 t.own_device
            t.transaction_id
        in
        let key_ids = Map.keys event.mac
                      |> List.sort ~compare:String.compare
                      |> String.concat ~sep:"," in
        let calc =
          match mac_method with
          | Normal -> Olm.Sas.calculate_mac
          | Old    -> Olm.Sas.calculate_mac_long_kdf
        in
        calc t.sas key_ids (info ^ "KEY_IDS") >>= fun mac ->
        Result.ok_if_true (String.equal event.keys mac)
          ~error:(`Reason `KeyMismatch) >>= fun () ->
        verify_devices t (fun fp_key -> calc t.sas fp_key info) event.mac >>| fun t ->
        { t with state = MacReceived }
    end |> apply_sas_error t
end
