open Base

let yo_assoc l  : Yojson.Safe.t = `Assoc l
let yo_string s : Yojson.Safe.t = `String s
let yo_float f  : Yojson.Safe.t = `Float f
let yo_int i    : Yojson.Safe.t = `Int i
let yo_bool b   : Yojson.Safe.t = `Bool b
let yo_list l   : Yojson.Safe.t = `List l

let json_of_option con opt : Yojson.Safe.t =
  Option.value_map ~f:con ~default:`Null opt
