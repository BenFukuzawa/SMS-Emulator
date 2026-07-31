open Uints

(* What the I/O bus needs from the VDP. The concrete VDP (your partner's
   module) implements this; the I/O bus is a functor over it, so it compiles
   and is tested independently of the VDP's internals.

   Port map:
     0x7E  read  -> read_v_counter
     0x7F  read  -> read_h_counter
     0xBE  read  -> read_data      / write -> write_data
     0xBF  read  -> read_control   / write -> write_control  (status / command) *)
module type S = sig
  type t

  val read_data : t -> uint8
  val write_data : t -> uint8 -> unit
  val read_control : t -> uint8
  val write_control : t -> uint8 -> unit
  val read_v_counter : t -> uint8
  val read_h_counter : t -> uint8
end
