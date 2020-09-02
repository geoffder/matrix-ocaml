open Base

let yo_assoc l  : Yojson.Basic.t = `Assoc l
let yo_string s : Yojson.Basic.t = `String s
let yo_float f  : Yojson.Basic.t = `Float f
let yo_int i    : Yojson.Basic.t = `Int i
let yo_bool b   : Yojson.Basic.t = `Bool b
let yo_list l   : Yojson.Basic.t = `List l

let json_of_option con opt : Yojson.Basic.t =
  Option.value_map ~f:con ~default:`Null opt
