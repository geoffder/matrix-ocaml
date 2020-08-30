type credential = Password of string | AuthToken of string

type api_triple = Cohttp.Code.meth * string * Yojson.Basic.t option

type message_direction = Back | Forward
