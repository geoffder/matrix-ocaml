open Core
open Yojson_helpers

module Resp = struct
  module Standard = struct
    type t = { errcode        : string
             ; error          : string option [@default None]
             ; retry_after_ms : int option    [@default None]
             } [@@deriving of_yojson]
  end

  module Auth = struct
    type stages = { stages : string list } [@@deriving of_yojson]

    type params_map = (string StringMap.t) StringMap.t

    let params_map_of_yojson =
      StringMap.of_yojson (StringMap.of_yojson string_of_yojson)

    type t = { errcode   : string
             ; error     : string option [@default None]
             ; completed : string list
             ; flows     : stages list
             ; params    : params_map
             ; session   : string
             } [@@deriving of_yojson]
  end

  type t =
    [ `RespErr of Standard.t
    | `AuthErr of Auth.t
    | `JsonErr of string * Yojson.Safe.t
    ]

  let std_err e  = `RespErr e
  let auth_err e = `AuthErr e

  let of_yojson og_error j =
    Result.(
      if Yojson.Safe.equal (U.member "session" j) `Null
      then Standard.of_yojson j >>| std_err
      else Auth.of_yojson j     >>| auth_err
    ) |> function
    | Ok err  -> err
    | Error _ -> `JsonErr (og_error, j)
end

type t =
  [ Resp.t
  | `Max429s     (* Client.send: too many requests *)
  | `MaxTimeouts (* Client.send: timeout limit exceeded *)
  | `NotLoggedIn
  ]
