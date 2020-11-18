open! Core
open Yojson_helpers

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
