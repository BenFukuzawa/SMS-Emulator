type t = unit

let create () = ()

(* Tone, noise and attenuation writes all arrive here and are dropped. There
   is nothing to latch until the channels exist. *)
let write _t _byte = ()
