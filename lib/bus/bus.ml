open Uints
module Make (Cartridge : Addressable_intf.S) = struct
  type t = {
    cartridge: Cartridge.t;
    wram:Ram.t;
    vdp:Vdp.t;
    psg:Psg.t;
    shadow_ram:Shadow_ram.t;
    joypad:Joypad.t;
    ic:Interrupt_controller.t;
  }
end

let create ~cartridge ~wram ~vdp ~psg ~shadow_ram ~joypad ~ic = {
  cartridge;
  wram;
  vdp;
  psg;
  shadow_ram;
  joypad;
  ic
}