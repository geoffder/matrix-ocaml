open Base

let yo_assoc l  = `Assoc l
let yo_string s = `String s
let yo_float f  = `Float f
let yo_int i    = `Int i
let yo_bool b   = `Bool b

let json_of_option con opt : Yojson.Basic.t =
  Option.value_map ~f:con ~default:`Null opt
