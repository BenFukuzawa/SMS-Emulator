open Uints
(* registers.mli *)

type t

(* identifiers of the 8-bit registers *)
type r =
  | A
  | B
  | C
  | D
  | E
  | H
  | L
  | IXL
  | IXY

(* identifiers for the 16-bit registers *)
type rr =
  | AF
  | BC
  | DE
  | HL
  | IX
  | IY
  | SP

(* read/write functions for the above registers. carry, add/subtract,
   parity/overflow, unused, half carry, unused, zero, sign *)
type flag =
  | Carry
  | Subtraction (* 1 = subtraction, 0 = addition *)
  | Parity_overflow
  | Flag_y
  | Half_carry
  | Flag_x
  | Zero
  | Sign

val create : unit -> t
val read_r : t -> r -> uint8
val write_r : t -> r -> uint8 -> unit
val read_rr : t -> rr -> uint16
val write_rr : t -> rr -> uint16 -> unit
val read_flag : t -> flag -> bool
val set_flag : t -> flag -> unit

val set_flags
  :  t
  -> ?c:bool
  -> ?n:bool
  -> ?p:bool
  -> ?y:bool
  -> ?h:bool
  -> ?x:bool
  -> ?z:bool
  -> ?s:bool
  -> unit
  -> unit

val unset_flag : t -> flag -> unit
val clear_flags : t -> unit
val show : t -> string
val show_r : r -> string
val show_rr : rr -> string
