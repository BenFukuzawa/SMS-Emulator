open Uints

(* What the I/O bus needs from the PSG (SN76489 sound chip). It is write-only
   from the CPU's side: any write to ports 0x40-0x7F lands here. *)
module type S = sig
  type t

  val write : t -> uint8 -> unit
end
