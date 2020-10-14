open Base

(* Infix of Fn.compose *)
let ( >> ) g f x = f (g x)

(* Using Base infix for map since that isn't opened at the top level. *)
let ( >>|? ) a f = Lwt_result.map f a

(* Lwt_result.bind takes value as first arg, and func as second. So this cleans
 * things up a bit for me. *)
let ( >>=? ) = Lwt_result.bind

(* Bind a Result within the Lwt monad. *)
let ( >>|=? ) lwt_res f = Lwt.map (fun a -> Result.bind ~f a) lwt_res
