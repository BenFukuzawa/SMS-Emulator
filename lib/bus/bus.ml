open Stdint
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

let read_byte t ~addr = 
  match addr with
    | _ when Cartridge.accepts t.cartridge addr     -> Cartridge.read_byte t.cartridge addr
    | _ when Ram.accepts t.wram       addr          -> Ram.read_byte t.wram addr
    | _ when Ram.accepts t.zero_page  addr          -> Ram.read_byte t.zero_page addr
    | _ when Gpu.accepts t.gpu        addr          -> Gpu.read_byte t.gpu addr
    | _ when Joypad.accepts t.joypad addr           -> Joypad.read_byte t.joypad addr
    | _ when Shadow_ram.accepts t.shadow_ram addr   -> Shadow_ram.read_byte t.shadow_ram addr
    | _ when Serial_port.accepts t.serial_port addr -> Serial_port.read_byte t.serial_port addr
    | _ when Interrupt_controller.accepts t.ic addr -> Interrupt_controller.read_byte t.ic addr
    | _ when Timer.accepts t.timer addr             -> Timer.read_byte t.timer addr
    | _ when Mmap_register.accepts t.dt addr        -> Mmap_register.read_byte t.dt addr
    | _ ->
      (* Undocumented IO registers should always return 0xFF. Blargg's cpu_insrs fail without this.
       * https://www.reddit.com/r/EmuDev/comments/ipap0w/comment/g76m04i/?utm_source=share&utm_medium=web2x&context=3 *)
      Uint8.of_int 0xFF


let write_byte t ~(addr : uint16) ~(data : uint8) = 