open! Core
open Result.Let_syntax
open Neolm_devices
open Yojson_helpers
module KV = ToDevice.KeyVerification

type reason =
  | UserCancel
  | Timeout
  | UnknownTransaction
  | UnknownMethod
  | UnexpectedMessage
  | KeyMismatch
  | UserMismatch
  | InvalidMessage
  | CommitmentMismatch
  | SasMismatch
  | Unknown of string

type error =
  [ `Reason of reason
  | `Protocol of string
  | `EmojiFail of string
  | `DecimalsFail of string
  ]

let reason_of_code = function
  | "m.user_cancel"         -> UserCancel
  | "m.timeout"             -> Timeout
  | "m.unknown_transaction" -> UnknownTransaction
  | "m.unknown_method"      -> UnknownMethod
  | "m.unexpected_message"  -> UnexpectedMessage
  | "m.key_mismatch"        -> KeyMismatch
  | "m.user_error"          -> UserMismatch
  | "m.invalid_message"     -> InvalidMessage
  | "m.commitment_mismatch" -> CommitmentMismatch
  | "m.sas_mismatch"        -> SasMismatch
  | s                       -> Unknown s

let reason_to_code = function
  | UserCancel         -> "m.user_cancel"
  | Timeout            -> "m.timeout"
  | UnknownTransaction -> "m.unknown_transaction"
  | UnknownMethod      -> "m.unknown_method"
  | UnexpectedMessage  -> "m.unexpected_message"
  | KeyMismatch        -> "m.key_mismatch"
  | UserMismatch       -> "m.user_error"
  | InvalidMessage     -> "m.invalid_message"
  | CommitmentMismatch -> "m.commitment_mismatch"
  | SasMismatch        -> "m.sas_mismatch"
  | Unknown s          -> s

let reason_to_string = function
  | UserCancel         -> "Canceled by user"
  | Timeout            -> "Timed out"
  | UnknownTransaction -> "Unknown transaction"
  | UnknownMethod      -> "Unknown method"
  | UnexpectedMessage  -> "Unexpected message"
  | KeyMismatch        -> "Key mismatch"
  | UserMismatch       -> "User mismatch"
  | InvalidMessage     -> "Invalid message"
  | CommitmentMismatch -> "Mismatched commitment"
  | SasMismatch        -> "Mismatched short authentication string"
  | Unknown s          -> "Unknown reason: " ^ s

type state =
  | Created
  | Started
  | Accepted
  | KeyReceived
  | MacReceived
  | Canceled of reason

let emoji =
  [ "🐶", "Dog"
  ; "🐱", "Cat"
  ; "🦁", "Lion"
  ; "🐎", "Horse"
  ; "🦄", "Unicorn"
  ; "🐷", "Pig"
  ; "🐘", "Elephant"
  ; "🐰", "Rabbit"
  ; "🐼", "Panda"
  ; "🐓", "Rooster"
  ; "🐧", "Penguin"
  ; "🐢", "Turtle"
  ; "🐟", "Fish"
  ; "🐙", "Octopus"
  ; "🦋", "Butterfly"
  ; "🌷", "Flower"
  ; "🌳", "Tree"
  ; "🌵", "Cactus"
  ; "🍄", "Mushroom"
  ; "🌏", "Globe"
  ; "🌙", "Moon"
  ; "☁️", "Cloud"
  ; "🔥", "Fire"
  ; "🍌", "Banana"
  ; "🍎", "Apple"
  ; "🍓", "Strawberry"
  ; "🌽", "Corn"
  ; "🍕", "Pizza"
  ; "🎂", "Cake"
  ; "❤️", "Heart"
  ; "😀", "Smiley"
  ; "🤖", "Robot"
  ; "🎩", "Hat"
  ; "👓", "Glasses"
  ; "🔧", "Wrench"
  ; "🎅", "Santa"
  ; "👍", "Thumbs up"
  ; "☂️", "Umbrella"
  ; "⌛", "Hourglass"
  ; "⏰", "Clock"
  ; "🎁", "Gift"
  ; "💡", "Light Bulb"
  ; "📕", "Book"
  ; "✏️", "Pencil"
  ; "📎", "Paperclip"
  ; "✂️", "Scissors"
  ; "🔒", "Lock"
  ; "🔑", "Key"
  ; "🔨", "Hammer"
  ; "☎️", "Telephone"
  ; "🏁", "Flag"
  ; "🚂", "Train"
  ; "🚲", "Bicycle"
  ; "✈️", "Airplane"
  ; "🚀", "Rocket"
  ; "🏆", "Trophy"
  ; "⚽", "Ball"
  ; "🎸", "Guitar"
  ; "🎺", "Trumpet"
  ; "🔔", "Bell"
  ; "⚓", "Anchor"
  ; "🎧", "Headphones"
  ; "📁", "Folder"
  ; "📌", "Pin"
  ]
  |> List.mapi ~f:(fun i e -> i, e)
  |> Map.of_alist_exn (module Int)

let sas_vers = "m.sas.v1"
let hash_v1 = "sha256"
let max_age = Time.Span.of_min 5.0
let max_event_timeout = Time.Span.of_min 1.0

type t =
  { sas : Olm.Sas.t
  ; own_user : string
  ; own_device : string
  ; own_fp_key : string
  ; other_olm_device : Device.t
  ; transaction_id : string
  ; sas_methods : KV.SasMethod.t list
  ; mac_methods : KV.MacMethod.t list
  ; chosen_mac_method : KV.MacMethod.t option
  ; key_agreement_protocols : KV.KeyAgreementProtocol.t list
  ; chosen_key_agreement : KV.KeyAgreementProtocol.t option
  ; state : state
  ; we_started_it : bool
  ; sas_accepted : bool
  ; commitment : string option
  ; their_sas_key : string option
  ; verified_devices : string list
  ; creation_time : Time.t
  ; last_event_time : Time.t
  }

let create
    ?tx_id
    ?(sas_methods = [ KV.SasMethod.Emoji; Decimal ])
    ?(mac_methods = [ KV.MacMethod.Normal; Old ])
    ?(key_agreement_protocols = [ KV.KeyAgreementProtocol.V1; V2 ])
    ?(we_started_it = true)
    own_user
    own_device
    own_fp_key
    other_olm_device
  =
  Olm.Sas.create ()
  >>| fun sas ->
  let transaction_id =
    Option.value ~default:(Uuid_unix.create () |> Uuid.to_string) tx_id
  in
  let now = Time.now () in
  { sas
  ; own_user
  ; own_device
  ; own_fp_key
  ; other_olm_device
  ; transaction_id
  ; sas_methods
  ; mac_methods
  ; chosen_mac_method = None
  ; key_agreement_protocols
  ; chosen_key_agreement = None
  ; state = Created
  ; we_started_it
  ; sas_accepted = false
  ; commitment = None
  ; their_sas_key = None
  ; verified_devices = []
  ; creation_time = now
  ; last_event_time = now
  }

let from_key_verification_start
    own_user
    own_device
    own_fp_key
    other_olm_device
    (event : ToDevice.KeyVerification.StartSAS.t)
  =
  let%bind t =
    create
      ~tx_id:event.transaction_id
      ~we_started_it:false
      ~sas_methods:event.short_authentication_string
      ~mac_methods:event.message_authentication_codes
      ~key_agreement_protocols:event.key_agreement_protocols
      own_user
      own_device
      own_fp_key
      other_olm_device
  in
  let%bind pubkey = Olm.Sas.pubkey t.sas in
  ToDevice.KeyVerification.StartSAS.to_yojson event
  |> Api.canonical_json
  |> ( ^ ) pubkey
  |> Neolm_utils.sha256
  >>| fun commitment -> { t with commitment = Some commitment }

let canceled = function
  | { state = Canceled _; _ } -> true
  | _                         -> false

let verified = function
  | { state = MacReceived; sas_accepted = true; _ } -> true
  | _ -> false

let created = function
  | { state = Created; _ } -> true
  | _                      -> false

let cancel_reason = function
  | { state = Canceled reason; _ } -> Some reason
  | _ -> None

let supports_normal_mac t =
  List.exists t.mac_methods ~f:(function
      | Normal -> true
      | _      -> false)

let supports_agreement_v2 t =
  List.exists t.key_agreement_protocols ~f:(function
      | V2 -> true
      | _  -> false)

let apply_sas_error t = function
  | Ok _ as ok        -> ok
  | Error (`Reason r) -> Result.return { t with state = Canceled r }
  | Error _ as err    -> err

let event_to_message t e =
  ToDevice.to_message e t.other_olm_device.user_id t.other_olm_device.id

let time_out t =
  if verified t || canceled t
  then false, t
  else (
    let n = Time.now () in
    if Time.Span.(
         Time.diff n t.creation_time >=. max_age
         || Time.diff n t.last_event_time >=. max_event_timeout)
    then true, { t with state = Canceled Timeout }
    else false, t )

let set_their_pubkey t pubkey =
  Olm.Sas.set_their_pubkey t.sas pubkey
  >>| fun _ -> { t with their_sas_key = Some pubkey }

let accept_sas = function
  | { state = Canceled _; _ }   -> Result.fail (`Protocol "Already canceled.")
  | { their_sas_key = None; _ } -> Result.fail (`Protocol "Other key not even set.")
  | t                           -> Result.return { t with sas_accepted = true }

let reject_sas = function
  | { their_sas_key = None; _ } -> Result.fail (`Protocol "Other key not even set.")
  | t                           -> Result.return { t with state = Canceled SasMismatch }

let cancel t = { t with state = Canceled UserCancel }

let grouper ?filler n str =
  let f i acc c =
    let s = String.of_char c in
    acc ^ if i > 0 && i mod n = 0 then " " ^ s else s
  in
  let padding =
    match String.length str mod n with
    | 0   -> ""
    | off ->
      Option.value_map filler ~default:"" ~f:(fun c ->
          String.init (n - off) ~f:(fun _ -> c))
  in
  String.foldi str ~f ~init:"" ^ padding |> String.split ~on:' '

let extra_info_v1 t =
  let dev = t.other_olm_device in
  let tx_id = t.transaction_id in
  let our_info = sprintf "%s%s" t.own_user t.own_device in
  let their_info = sprintf "%s%s" dev.user_id dev.id in
  if t.we_started_it
  then sprintf "MATRIX_KEY_VERIFICATION_SAS%s%s%s" our_info their_info tx_id
  else sprintf "MATRIX_KEY_VERIFICATION_SAS%s%s%s" their_info our_info tx_id

let extra_info_v2 t =
  let%map their_key =
    Result.of_option t.their_sas_key ~error:(`Protocol "Their SAS key is not set.")
  and pubkey = Olm.Sas.pubkey t.sas in
  let dev = t.other_olm_device in
  let tx_id = t.transaction_id in
  let our_info = sprintf "%s|%s|%s" t.own_user t.own_device pubkey in
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
  let%bind info = extra_info t in
  let%bind bytes = Olm.Sas.generate_bytes t.sas info 6 in
  try
    Bytes.of_string bytes
    |> Bytes.fold ~init:"" ~f:(fun acc c ->
           acc ^ (Char.to_int c |> Neolm_utils.bin8_of_int_exn))
    |> Fn.flip String.prefix @@ 42
    |> grouper 6
    |> List.map ~f:(fun bits -> Neolm_utils.int_of_bin_exn bits |> Map.find_exn emoji)
    |> Result.return
  with
  | e -> Result.fail (`EmojiFail (Exn.to_string e))

let get_decimals t =
  let%bind info = extra_info t in
  let%bind bytes = Olm.Sas.generate_bytes t.sas info 5 in
  try
    Bytes.of_string bytes
    |> Bytes.fold ~init:"" ~f:(fun acc c ->
           acc ^ (Char.to_int c |> Neolm_utils.bin8_of_int_exn))
    |> Fn.flip String.drop_suffix @@ 1
    |> grouper 13
    |> List.map ~f:(fun bits -> Neolm_utils.int_of_bin_exn bits + 1000)
    |> Result.return
  with
  | e -> Result.fail (`DecimalsFail (Exn.to_string e))

let start_verification_event t =
  Result.ok_if_true t.we_started_it ~error:(`Protocol "Not started by us, can't send.")
  >>= fun () ->
  Result.ok_if_true
    (not @@ canceled t)
    ~error:(`Protocol "Verification was canceled, can't send.")
  >>= fun () ->
  ToDevice.KeyVerification
    (StartSAS
       { transaction_id = t.transaction_id
       ; v_method = sas_vers
       ; key_agreement_protocols = t.key_agreement_protocols
       ; hashes = [ hash_v1 ]
       ; message_authentication_codes = t.mac_methods
       ; short_authentication_string = t.sas_methods
       })
  |> Result.return

let start_verification_message t = start_verification_event t >>| event_to_message t

let accept_verification_event t =
  Result.ok_if_true
    (not t.we_started_it)
    ~error:(`Protocol "Started by us, can't accept offer.")
  >>= fun () ->
  Result.ok_if_true
    (not @@ canceled t)
    ~error:(`Protocol "Verification was canceled, can't accept offer.")
  >>= fun () ->
  let%map commitment =
    Result.of_option t.commitment ~error:(`Protocol "No commitment, can't accept offer.")
  in
  let chosen_mac = if supports_normal_mac t then KV.MacMethod.Normal else Old in
  let chosen_agreement =
    if supports_agreement_v2 t then KV.KeyAgreementProtocol.V2 else V1
  in
  ToDevice.KeyVerification
    (Accept
       { transaction_id = t.transaction_id
       ; v_method = sas_vers
       ; key_agreement_protocol = chosen_agreement
       ; hash = hash_v1
       ; message_authentication_code = chosen_mac
       ; short_authentication_string = t.sas_methods
       ; commitment
       })

let accept_verification_message t = accept_verification_event t >>| event_to_message t

let check_commitment t key =
  match t.commitment with
  | None   -> Result.fail (`Protocol "No existing commitment.")
  | Some c ->
    let%bind event = accept_verification_event t in
    let content =
      ToDevice.content_to_yojson event
      |> Yojson.Safe.Util.member "content"
      |> Api.canonical_json
    in
    let%bind sha = Neolm_utils.sha256 (key ^ content) in
    Result.ok_if_true (String.equal c sha) ~error:(`Reason CommitmentMismatch)

let share_key_event t =
  Result.ok_if_true
    (not @@ canceled t)
    ~error:(`Protocol "Verification was canceled, can't share key.")
  >>= fun () ->
  let%map key = Olm.Sas.pubkey t.sas in
  ToDevice.KeyVerification (Key { transaction_id = t.transaction_id; key })

let share_key_message t = share_key_event t >>| event_to_message t

let mac_event t =
  Result.ok_if_true t.sas_accepted ~error:(`Protocol "SAS string wasn't yet accepted.")
  >>= fun () ->
  Result.ok_if_true
    (canceled t)
    ~error:(`Protocol "Verification was canceled, can't generate MAC")
  >>= fun () ->
  let%bind mac_method =
    Result.of_option
      t.chosen_mac_method
      ~error:(`Protocol "No MAC method has been chosen yet.")
  in
  let key_id = "ed25519:" ^ t.own_device in
  let info =
    sprintf
      "MATRIX_KEY_VERIFICATION_MAC%s%s%s%s%s"
      t.own_user
      t.own_device
      t.other_olm_device.user_id
      t.other_olm_device.id
      t.transaction_id
  in
  let calc =
    match mac_method with
    | Normal -> Olm.Sas.calculate_mac
    | Old    -> Olm.Sas.calculate_mac_long_kdf
  in
  let%map mac = calc t.sas t.own_fp_key (info ^ key_id)
  and keys = calc t.sas key_id (info ^ "KEY_IDS") in
  ToDevice.KeyVerification
    (Mac
       { transaction_id = t.transaction_id
       ; mac = StringMap.of_alist_exn [ "key_id", mac ]
       ; keys
       })

let mac_message t = mac_event t >>| event_to_message t

let cancellation_event t =
  let%map err =
    Result.of_option (cancel_reason t) ~error:(`Protocol "Sas process isn't cancelled.")
  in
  ToDevice.KeyVerification
    (Cancel
       { transaction_id = t.transaction_id
       ; code = reason_to_code err
       ; reason = reason_to_string err
       })

let cancellation_message t = cancellation_event t >>| event_to_message t

let event_ok t sender tx_id =
  Result.ok_if_true (canceled t) ~error:(`Protocol "Already canceled")
  >>= fun () ->
  Result.ok_if_true
    (String.equal t.transaction_id tx_id)
    ~error:(`Reason UnknownTransaction)
  >>= fun () ->
  Result.ok_if_true
    (String.equal t.other_olm_device.user_id sender)
    ~error:(`Reason UserMismatch)

let receive_accept_event t sender (event : ToDevice.KeyVerification.Accept.t) =
  (let%bind () = event_ok t sender event.transaction_id in
   Result.ok_if_true (created t) ~error:(`Reason UnexpectedMessage)
   >>= fun () ->
   Result.ok_if_true
     ~error:(`Reason UnknownMethod)
     ( List.mem
         t.key_agreement_protocols
         event.key_agreement_protocol
         ~equal:KV.KeyAgreementProtocol.equal
     && String.equal event.hash hash_v1
     && List.mem t.mac_methods event.message_authentication_code ~equal:KV.MacMethod.equal
     && List.length event.short_authentication_string > 0 )
   >>| fun () ->
   { t with
     commitment = Some event.commitment
   ; chosen_mac_method = Some event.message_authentication_code
   ; chosen_key_agreement = Some event.key_agreement_protocol
   ; sas_methods = event.short_authentication_string
   ; state = Accepted
   })
  |> apply_sas_error t

let receive_key_event t sender (event : ToDevice.KeyVerification.Key.t) =
  let%bind () =
    Result.ok_if_true
      ~error:(`Reason UnexpectedMessage)
      ( Option.is_none t.their_sas_key
      && t.state
         |> function
         | Accepted | Started -> true
         | _                  -> false )
  in
  let%bind () = event_ok t sender event.transaction_id in
  let%bind () =
    if t.we_started_it then check_commitment t event.key else Result.return ()
  in
  set_their_pubkey t event.key
  >>| (fun t -> { t with state = KeyReceived })
  |> apply_sas_error t

let verify_devices t calc_mac mac_map =
  let id_split key_id =
    match String.split ~on:':' key_id with
    | [ key_type; device_id ] -> Result.return (key_type, device_id)
    | _                       -> Result.fail (`Reason InvalidMessage)
  in
  let verify devs ~key:key_id ~data:key_mac =
    let%bind key_type, device_id = id_split key_id in
    Result.ok_if_true (String.equal key_type "ed25519") ~error:(`Reason KeyMismatch)
    >>= fun () ->
    if String.equal device_id t.other_olm_device.id
    then (
      let%bind mac = calc_mac t.other_olm_device.keys.ed25519 in
      Result.ok_if_true ~error:(`Reason KeyMismatch) (String.equal key_mac mac)
      >>| fun () -> device_id :: devs )
    else Result.return devs
  in
  Neolm_utils.map_fold_result ~init:[] ~f:verify mac_map
  >>= function
  | devices when List.length devices > 0 -> Result.return devices
  | _ -> Result.fail (`Reason KeyMismatch)

let receive_mac_event t sender (event : ToDevice.KeyVerification.Mac.t) =
  begin
    if verified t
    then Result.return t
    else
      event_ok t sender event.transaction_id
      >>= fun () ->
      Result.ok_if_true
        ( t.state
        |> function
        | KeyReceived -> true
        | _           -> false )
        ~error:(`Reason UnexpectedMessage)
      >>= fun () ->
      let%bind mac_method =
        Result.of_option
          t.chosen_mac_method
          ~error:(`Protocol "No MAC method has been chosen yet.")
      in
      let info =
        sprintf
          "MATRIX_KEY_VERIFICATION_MAC%s%s%s%s%s"
          t.other_olm_device.user_id
          t.other_olm_device.id
          t.own_user
          t.own_device
          t.transaction_id
      in
      let key_ids =
        StringMap.keys event.mac
        |> List.sort ~compare:String.compare
        |> String.concat ~sep:","
      in
      let calc =
        match mac_method with
        | Normal -> Olm.Sas.calculate_mac
        | Old    -> Olm.Sas.calculate_mac_long_kdf
      in
      let%bind mac = calc t.sas key_ids (info ^ "KEY_IDS") in
      Result.ok_if_true (String.equal event.keys mac) ~error:(`Reason KeyMismatch)
      >>= fun () ->
      verify_devices t (fun fp_key -> calc t.sas fp_key info) event.mac
      >>| fun devs -> { t with verified_devices = devs; state = MacReceived }
  end
  |> apply_sas_error t
