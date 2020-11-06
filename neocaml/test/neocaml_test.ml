open Core
open Neocaml
open Neocaml.Neo_infix

let client = Client.create "https://shakeandwake.xyz" "@test_user:matrix.shakeandwake.xyz"

(* For simple relative pathing from the running directory, which for this module
 * is neocaml/bin/default/test. *)
let up_dir ?(n=1) pth =
  String.chop_suffix_if_exists ~suffix:"/" pth
  |> String.split ~on:'/'
  |> (fun l -> List.sub ~pos:0 ~len:(List.length l - n) l)
  |> String.concat ~sep:"/"
  |> fun s -> s ^ "/"

let logged =
  let pass = "/home/" ^ Unix.getlogin () ^ "/.secrets/neocaml/pass.json"
             |> Yojson.Safe.from_file
             |> Yojson.Safe.Util.member "password"
             |> Yojson.Safe.Util.to_string in
  Client.login (Types.Credential.Password pass) client

let room_id = "!IYzufTUjKQbVlmtONr:matrix.shakeandwake.xyz"
let prev_batch = ""

let messages = logged >>=? Client.room_messages room_id prev_batch

let%test "room messages" = Lwt_main.run messages |> Result.is_ok

let sync_resp = logged >>=? Client.sync

let%test "sync response" = Lwt_main.run sync_resp |> Result.is_ok

let send_poggers () =
  let open Events.Room in
  let pogchamp =
    "<img src='mxc://matrix.shakeandwake.xyz/JTlEcEaBsmCuZzVDNdVgSvQN'" ^
    " alt=':pogchamp:' width=32 height=32>" in
  let formatted = sprintf "%s wew %s lad %s" pogchamp pogchamp pogchamp in
  let content =
    Message.Text.create
      ~format:"org.matrix.custom.html"
      ~formatted_body:formatted
      ":pogchamp: wew :pogchamp: lad :pogchamp:"
    |> Message.text in
  logged >>=? Client.room_send room_id (Content.Message content)

let send_emote () =
  let open Events.Room in
  let content = Message.Emote.create "smiles creepily." |> Message.emote in
  logged >>=? Client.room_send room_id (Content.Message content)

let send_notice () =
  let open Events.Room in
  let content = Message.Notice.create "You're on notice!" |> Message.notice in
  logged >>=? Client.room_send room_id (Content.Message content)

let img_pth = "/home/" ^ Unix.getlogin () ^ "/Pictures/emojis/bigben.png"
let send_image () =
  let monitor = Monitor.stdout_bar () in
  logged >>=? Client.send_image ~monitor img_pth room_id

let webm_pth = "/home/" ^ Unix.getlogin () ^ "/Videos/webms/chad_wants_two.webm"
let send_webm () =
  let monitor = Monitor.stdout_bar () in
  logged >>=? Client.room_upload ~monitor webm_pth room_id
