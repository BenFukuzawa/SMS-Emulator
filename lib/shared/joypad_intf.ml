open Uints

(* What the I/O bus needs from the controller ports.

     0xDC  read  -> read_port_a   (pad 1, plus low bits of pad 2)
     0xDD  read  -> read_port_b   (rest of pad 2, reset button, TH inputs)
     0x3F  write -> write_control  (I/O port control: TH output levels) *)
module type S = sig
  type t

  val read_port_a : t -> uint8
  val read_port_b : t -> uint8
  val write_control : t -> uint8 -> unit
end
