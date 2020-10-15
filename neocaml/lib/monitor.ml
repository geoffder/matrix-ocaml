open Core

(* TODO: Decide on how I actually want to do this.
 * For one thing, the send / repeat functions are not aware of this, which
 * they will need to be in order to call an initiating / reseting call back.
 * How much is done with the monitor, and what is the responsibility of the caller? *)

type t = { init   : int -> unit
         ; step   : int -> unit
         ; finish : unit -> unit
         }

let def = { init   = (fun _ -> ())
          ; step   = (fun _ -> ())
          ; finish = (fun _ -> ())
          }

let stdout_bar ?(len=50) () =
  let tick_sz    = ref 0 in
  let uploaded   = ref 0 in
  let incr bytes = uploaded := !uploaded + bytes in
  let n_ticks () = !uploaded / !tick_sz in
  let draw n =
    printf "\r[%s%s]%!" (String.make n '=') (String.make (len - n) ' ')
  in
  { init   = (fun size  -> tick_sz := size / len; print_endline "Uploading...")
  ; step   = (fun bytes -> incr bytes; n_ticks () |> draw)
  ; finish = (fun ()    -> print_endline "\nDone!")
  }
