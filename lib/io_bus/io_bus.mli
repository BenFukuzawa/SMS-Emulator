(* The I/O bus is a functor over the three peripheral interfaces, mirroring
   how the Z80 core is a functor over the memory bus. The concrete VDP/PSG/
   joypad modules are supplied when the machine is assembled. *)
module Make (Vdp : Vdp_intf.S) (Psg : Psg_intf.S) (Joypad : Joypad_intf.S) : sig
  type t

  val create : vdp:Vdp.t -> psg:Psg.t -> joypad:Joypad.t -> t

  include Port_io_intf.S with type t := t
end
