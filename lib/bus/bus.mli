module Make (Cartridge : Addressable_intf.S) : sig

  type t

  val create :
    cartridge:Cartridge.t ->
    wram:Ram.t ->
    vdp:Vdp.t ->
    psg:Psg.t ->
    shadow_ram:Shadow_ram.t ->
    joypad:Joypad.t ->
    ic:Interrupt_controller.t ->
    t
  include Word_addressable_intf.S with type t := t   (* memory: byte + word *)
  include Port_io_intf.S          with type t := t   (* I/O ports: byte only *)

end