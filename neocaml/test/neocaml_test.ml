open Core
open Neocaml
open Neocaml.Neo_infix

let client = Client.make "https://shakeandwake.xyz" "@test_user:matrix.shakeandwake.xyz"

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
  Types.Credential.Password pass |> Client.login client

let room_id = "!IYzufTUjKQbVlmtONr:matrix.shakeandwake.xyz"
let prev_batch = ""

let messages = logged >>=? fun c -> Client.room_messages c room_id prev_batch

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
  logged >>=? fun c -> Client.room_send c room_id (Content.Message content)

let send_emote () =
  let open Events.Room in
  let content = Message.Emote.create "smiles creepily." |> Message.emote in
  logged >>=? fun c -> Client.room_send c room_id (Content.Message content)

let send_notice () =
  let open Events.Room in
  let content = Message.Notice.create "You're on notice!" |> Message.notice in
  logged >>=? fun c -> Client.room_send c room_id (Content.Message content)

let img_pth = "/home/" ^ Unix.getlogin () ^ "/Pictures/emojis/bigben.png"

let send_image () =
  let monitor = { Client.Monitor.def with step = fun n -> printf "%i bytes\n" n } in
  logged >>=? Client.send_image ~monitor img_pth room_id
