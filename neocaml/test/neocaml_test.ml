open Base
open Neocaml
open Neocaml.Neo_infix

let client = Client.make "https://matrix.org" "@beheddard:matrix.org"

let up_dir ?(n=1) pth =
  String.split ~on:'/' pth
  |> (fun l -> List.sub ~pos:0 ~len:(List.length l - n) l)
  |> String.concat ~sep:"/"
  |> fun s -> s ^ "/"

let logged =
  let pass = (Unix.getcwd () |> up_dir ~n:3) ^ "test/pass.json"
             |> Yojson.Safe.from_file
             |> Yojson.Safe.Util.member "password"
             |> Yojson.Safe.Util.to_string in
  Types.Credential.Password pass |> Client.login client

let room_id = "!UDTBpvFlRSZlyIbhvr:matrix.org"
let prev_batch =
  "t38-1427171613_757284957_4873410_582557651_411860784_1530319_90440135_287505593_127848"

let messages = logged >=> fun c -> Client.room_messages c room_id prev_batch

let%test "room messages" = Lwt_main.run messages |> Result.is_ok

let sync_resp = logged >=> Client.sync

let%test "sync response" = Lwt_main.run sync_resp |> Result.is_ok