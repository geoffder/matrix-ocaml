open! Core
open Yojson_helpers
open Result.Monad_infix

module Device = struct
  type trust_state = Unset | Verified | Blacklisted | Ignored

  type keys = { ed25519    : string
              ; curve25519 : string
              }

  type t = { user_id      : string
           ; device_id    : string
           ; keys         : keys
           ; display_name : string option
           ; deleted      : bool
           ; trust_state  : trust_state
           }

  let create ?display_name user_id device_id ed25519 curve25519 =
    { user_id
    ; device_id
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
        match Map.add u ~key:d.device_id ~data:d with
        | `Ok u      -> Result.return u
        | `Duplicate -> Result.fail `DuplicateDevice
      end >>| fun data -> Map.set t ~key:d.user_id ~data
    | None ->
      Map.add_exn t
        ~key:d.user_id
        ~data:(Map.of_alist_exn (module String) [ (d.device_id, d) ])
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
    | `UnexpectedMethod
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

  let sas_method_of_string = function
    | "emoji"   -> Result.return Emoji
    | "decimal" -> Result.return Decimal
    | _         -> Result.fail `UnknownMethod

  let error_of_string = function
    | "m.user_cancel"         -> `UserCancel
    | "m.timeout"             -> `Timeout
    | "m.unknown_transaction" -> `UnknownTransaction
    | "m.unknown_method"      -> `UnknownMethod
    | "m.unexpected_method"   -> `UnexpectedMethod
    | "m.key_mismatch"        -> `KeyMismatch
    | "m.user_mismatch"       -> `UserMismatch
    | "m.invalid_message"     -> `InvalidMessage
    | "m.commitment_mismatch" -> `CommitmentMismatch
    | "m.sas_mismatch"        -> `SasMismatch
    | s                       -> `UnknownError s

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
    |> Olm.Utility.sha256 (Olm.Utility.create ()) >>= fun commitment ->
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
    |> Olm.Utility.sha256 (Olm.Utility.create ()) >>= fun commitment ->
    let methods_res =
      List.map ~f:sas_method_of_string event.short_authentication_string
      |> Result.all in
    let protocols_res =
      List.map ~f:key_agreement_protocol_of_string event.key_agreement_protocols
      |> Result.all in
    let macs_res =
      List.map ~f:mac_method_of_string event.message_authentication_codes
      |> Result.all in
    match methods_res, protocols_res, macs_res with
    | Ok sas_methods, Ok key_agreement_protocols, Ok mac_methods ->
      Result.return { t with
                      sas_methods
                    ; mac_methods
                    ; key_agreement_protocols
                    ; commitment = Some commitment
                    }
    | _, _, _ ->
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
end
