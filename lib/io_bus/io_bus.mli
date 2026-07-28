module Make : sig
  type t

  val create : vdp:Vdp.t -> psg:Psg.t -> joypad:Joypad.t -> t

  include Port_io_intf.S with type t := t (* memory: byte + word *)
end
