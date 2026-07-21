open Uints

module Make (Cartridge : Addressable_intf.S) = struct
  type t =
    { cartridge : Cartridge.t
    ; ram : Ram.t
    ; vdp : Vdp.t
    ; psg : Psg.t
    ; joypad : Joypad.t
    ; ic : Interrupt_controller.t
    }

  let create ~cartridge ~ram ~vdp ~psg ~joypad ~ic =
    { cartridge; ram; vdp; psg; joypad; ic }
  ;;

  let read_byte t addr =
    match addr with
    | _ when Cartridge.accepts t.cartridge addr ->
      Cartridge.read_byte t.cartridge addr
    | _ when Ram.accepts t.ram addr -> Ram.read_byte t.ram addr
    | _ when Vdp.accepts t.vdp addr -> Vdp.read_byte t.vdp addr
    | _ when Psg.accepts t.psg addr -> Psg.read_byte t.psg addr
    | _ when Joypad.accepts t.joypad addr -> Joypad.read_byte t.joypad addr
    | _ when Interrupt_controller.accepts t.ic addr ->
      Interrupt_controller.read_byte t.ic addr
    | _ -> Uint8.of_int 0xFF
  ;;

  let write_byte t ~(addr : uint16) ~(data : uint8) =
    let dma_transfer (source : uint8) =
      let source =
        (* source: $XX00-$XX9F ;XX = $00 to $DF *)
        Uint16.(of_uint8 source lsl 8)
      in
      for i = 0 to 0x9F do
        let data = read_byte t Uint16.(source + of_int i) in
        Gpu.write_oam_with_offset t.gpu ~offset:i ~data
      done
    in
    match addr with
    | _ when Cartridge.accepts t.cartridge addr ->
      Cartridge.write_byte t.cartridge ~addr ~data
    | _ when Ram.accepts t.wram addr -> Ram.write_byte t.wram ~addr ~data
    | _ when Ram.accepts t.zero_page addr ->
      Ram.write_byte t.zero_page ~addr ~data
    | _ when Gpu.accepts t.gpu addr -> Gpu.write_byte t.gpu ~addr ~data
    | _ when Joypad.accepts t.joypad addr ->
      Joypad.write_byte t.joypad ~addr ~data
    | _ when Shadow_ram.accepts t.shadow_ram addr ->
      Shadow_ram.write_byte t.shadow_ram ~addr ~data
    | _ when Serial_port.accepts t.serial_port addr ->
      Serial_port.write_byte t.serial_port ~addr ~data
    | _ when Interrupt_controller.accepts t.ic addr ->
      Interrupt_controller.write_byte t.ic ~addr ~data
    | _ when Timer.accepts t.timer addr ->
      Timer.write_byte t.timer ~addr ~data
    | _ when Mmap_register.accepts t.dt addr ->
      Mmap_register.write_byte t.dt ~addr ~data;
      dma_transfer data
    | _ -> ()
  ;;

  let accepts t addr =
    Cartridge.accepts t.cartridge addr
    || Ram.accepts t.wram addr
    || Gpu.accepts t.gpu addr
    || Ram.accepts t.zero_page addr
    || Shadow_ram.accepts t.shadow_ram addr
    || Shadow_ram.accepts t.shadow_ram addr
    || Joypad.accepts t.joypad addr
    || Serial_port.accepts t.serial_port addr
    || Interrupt_controller.accepts t.ic addr
    || Timer.accepts t.timer addr
    || Mmap_register.accepts t.dt addr
  ;;

  let read_word t addr =
    let lo = Uint8.to_int (read_byte t addr) in
    let hi = Uint8.to_int (read_byte t Uint16.(succ addr)) in
    (hi lsl 8) + lo |> Uint16.of_int
  ;;

  let write_word t ~addr ~(data : uint16) =
    let data = Uint16.to_int data in
    let hi = data lsr 8 |> Uint8.of_int in
    let lo = data land 0xFF |> Uint8.of_int in
    write_byte t ~addr ~data:lo;
    write_byte t ~addr:Uint16.(succ addr) ~data:hi
  ;;
end
