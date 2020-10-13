(* Using Base infix for map since that isn't opened at the top level. *)
let ( >>|? ) a f = Lwt_result.map f a

(* Lwt_result.bind takes value as first arg, and func as second. So this cleans
 * things up a bit for me. *)
let ( >>=? ) = Lwt_result.bind

(* Infix of Fn.compose *)
let ( >> ) g f x = f (g x)

(* NOTE: Should be unnecessary now, but keeping for a bit in case. *)
(* Since Cohttp_lwt.Body lives in Lwt, double flat mapping is required. *)
let ( >>|= ) r_lwt f = Lwt_result.map (fun lwt -> Lwt.map f lwt) r_lwt
