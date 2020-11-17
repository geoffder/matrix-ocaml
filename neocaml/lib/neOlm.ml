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
  type state =
    | Created
    | Started
    | Accepted
    | KeyReceived
    | MacReceived
    | Canceled

  type error =
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
    | `UnknownError of string
    ]

  type sas_method = Emoji | Decimal

  type mac_method = Normal | Old

  type key_agreement_protocol = V1 | V2

  let key_agreement_protocol_to_string = function
    | V1 -> "curve25519"
    | V2 -> "curve25519-hkdf-sha256"

  let key_agreement_protocol_of_string = function
    | "curve25519"             -> Result.return V1
    | "curve25519-hkdf-sha256" -> Result.return V2
    | _                        -> Result.fail `UnknownMethod

  let mac_method_to_string = function
    | Normal -> "hkdf-hmac-sha256"
    | Old    -> "hmac-sha256"

  let mac_method_of_string = function
    | "hkdf-hmac-sha256" -> Result.return Normal
    | "hmac-sha256"      -> Result.return Old
    | _                  -> Result.fail `UnknownMethod

  let sas_method_to_string = function
    | Emoji   -> "emoji"
    | Decimal -> "decimal"

  let sas_method_of_string = function
    | "emoji"   -> Result.return Emoji
    | "decimal" -> Result.return Decimal
    | _         -> Result.fail `UnknownMethod

  let error_of_code = function
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
    | s                       -> `UnknownError s

  let error_to_code = function
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
    |  `UnknownError s     -> s

  let error_to_reason = function
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
    |  `UnknownError s     -> "Unknown error: " ^ s

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
           ; sas_methods             : sas_method list
           ; mac_methods             : mac_method list
           ; chosen_mac_method       : mac_method option
           ; key_agreement_protocols : key_agreement_protocol list
           ; chosen_key_agreement    : key_agreement_protocol option
           ; state                   : state
           ; we_started_it           : bool
           ; sas_accepted            : bool
           ; commitment              : string option
           ; cancel_reason           : error option
           ; their_sas_key           : string option
           ; verified_devices        : string list
           ; creation_time           : Time.t
           ; last_event_time         : Time.t
           }

  let create
      ?tx_id
      ?(sas_methods=[ Emoji; Decimal ])
      ?(mac_methods=[ Normal; Old ])
      ?(key_agreement_protocols=[ V1; V2 ])
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
    ; cancel_reason           = None
    ; their_sas_key           = None
    ; verified_devices        = []
    ; creation_time           = now
    ; last_event_time         = now
    }

  (* NOTE: Not sure if I should use this way, where you simply get an error, or
   * should return a t with the cancel code included, more like the nio vers. *)
  let from_key_verification_start
      own_user
      own_device
      own_fp_key
      other_olm_device
      (event : ToDevice.KeyVerification.StartSAS.t)
    =
    List.map ~f:sas_method_of_string event.short_authentication_string
    |> Result.all >>= fun sas_methods ->
    List.map ~f:mac_method_of_string event.message_authentication_codes
    |> Result.all >>= fun mac_methods ->
    List.map ~f:key_agreement_protocol_of_string event.key_agreement_protocols
    |> Result.all >>= fun key_agreement_protocols ->
    create
      ~tx_id:event.transaction_id
      ~we_started_it:false
      ~sas_methods
      ~mac_methods
      ~key_agreement_protocols
      own_user
      own_device
      own_fp_key
      other_olm_device >>= fun t ->
    Olm.Sas.pubkey t.sas >>= fun pubkey ->
    ToDevice.KeyVerification.StartSAS.to_yojson event
    |> Api.canonical_json
    |> ( ^ )  pubkey
    |> Olm.Utility.sha256 util >>= fun commitment ->
    Result.return { t with commitment = Some commitment }

  (* I don't really like this way, since you need to check the error field
   * to know if there was a problem, rather than knowing right away from the
   * Result type. *)
  let from_key_verification_start'
      own_user
      own_device
      own_fp_key
      other_olm_device
      (event : ToDevice.KeyVerification.StartSAS.t)
    =
    create
      ~tx_id:event.transaction_id
      ~we_started_it:false
      own_user
      own_device
      own_fp_key
      other_olm_device   >>= fun t ->
    Olm.Sas.pubkey t.sas >>= fun pubkey ->
    ToDevice.KeyVerification.StartSAS.to_yojson event
    |> Api.canonical_json
    |> ( ^ )  pubkey
    |> Olm.Utility.sha256 util >>= fun commitment ->
    let sas_vers_res =
      Result.ok_if_true (String.equal event.v_method sas_vers) ~error:`UnknownMethod
    in
    let methods_res =
      List.map ~f:sas_method_of_string event.short_authentication_string
      |> Result.all in
    let protocols_res =
      List.map ~f:key_agreement_protocol_of_string event.key_agreement_protocols
      |> Result.all in
    let macs_res =
      List.map ~f:mac_method_of_string event.message_authentication_codes
      |> Result.all in
    match sas_vers_res, methods_res, protocols_res, macs_res with
    | Ok (), Ok sas_methods, Ok key_agreement_protocols, Ok mac_methods ->
      Result.return { t with
                      sas_methods
                    ; mac_methods
                    ; key_agreement_protocols
                    ; commitment = Some commitment
                    }
    | _, _, _, _ ->
      Result.return { t with
                      sas_methods             = []
                    ; mac_methods             = []
                    ; key_agreement_protocols = []
                    ; commitment              = Some commitment
                    ; state                   = Canceled
                    ; cancel_reason           = Some `UnknownMethod
                    }

  let canceled = function
    | { state = Canceled; _ } -> true
    | _                       -> false

  let verified = function
    | { state = MacReceived; sas_accepted = true; _ } -> true
    | _                                               -> false

  let supports_normal_mac t =
    List.exists t.mac_methods ~f:(function | Normal -> true | _ -> false)

  let supports_agreement_v2 t =
    List.exists t.key_agreement_protocols ~f:(function | V2 -> true | _ -> false)

  let event_to_message t e =
    ToDevice.to_message e t.other_olm_device.user_id t.other_olm_device.id

  let time_out t =
    if verified t || canceled t
    then false, t
    else
      let n = Time.now () in
      if Time.Span.(Time.diff n t.creation_time >=. max_age
                    || Time.diff n t.last_event_time >=. max_event_timeout)
      then true, { t with state = Canceled; cancel_reason = Some `Timeout }
      else false, t

  let set_their_pubkey t pubkey =
    Olm.Sas.set_their_pubkey t.sas pubkey >>| fun _ ->
    { t with their_sas_key = Some pubkey }

  let accept_sas = function
    | { state = Canceled; _ }     -> Result.fail (`Protocol "Already canceled.")
    | { their_sas_key = None; _ } -> Result.fail (`Protocol "Other key not even set.")
    | t                           -> Result.return { t with sas_accepted = true }

  let reject_sas = function
    | { their_sas_key = None; _ } -> Result.fail (`Protocol "Other key not even set.")
    | t -> Result.return { t with state = Canceled; cancel_reason = Some `SasMismatch }

  let cancel t =
    { t with state = Canceled; cancel_reason = Some `UserCancel }

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
    let key_agreement_protocols =
      List.map ~f:key_agreement_protocol_to_string t.key_agreement_protocols in
    ToDevice.KeyVerification
      (StartSAS
         { transaction_id                = t.transaction_id
         ; v_method                      = sas_vers
         ; key_agreement_protocols
         ; hashes                        = [ hash_v1 ]
         ; message_authentication_codes  = List.map ~f:mac_method_to_string t.mac_methods
         ; short_authentication_string   = List.map ~f:sas_method_to_string t.sas_methods
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
    let sas_methods = List.map ~f:sas_method_to_string t.sas_methods in
    let chosen_mac =
      mac_method_to_string (if supports_normal_mac t then Normal else Old) in
    let chosen_agreement =
      key_agreement_protocol_to_string (if supports_agreement_v2 t then V2 else V1) in
    ToDevice.KeyVerification
      (Accept { transaction_id              = t.transaction_id
              ; v_method                    = sas_vers
              ; key_agreement_protocol      = chosen_agreement
              ; hash                        = hash_v1
              ; message_authentication_code = chosen_mac
              ; short_authentication_string = sas_methods
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
      let content = ToDevice.to_yojson event
                    |> Yojson.Safe.Util.member "content"
                    |> Api.canonical_json
      in
      Olm.Utility.sha256 util (key ^ content) >>| String.equal c

  let share_key_event t =
    Result.ok_if_true (not @@ canceled t)
      ~error:(`Protocol "Verification was canceled, can't share key.") >>= fun () ->
    Olm.Sas.pubkey t.sas >>= fun key ->
    ToDevice.KeyVerification (Key { transaction_id = t.transaction_id; key })
    |> Result.return

  let share_key_message t = share_key_event t >>| event_to_message t
end
