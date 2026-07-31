open Uints

(* The SMS CPU memory map:

     0x0000-0xBFFF   cartridge (ROM through the Sega mapper, or cart RAM)
     0xC000-0xDFFF   8 KB system RAM
     0xE000-0xFFFF   mirror of system RAM

   The mapper control registers sit at 0xFFFC-0xFFFF, i.e. inside the RAM
   mirror. A write there updates BOTH the RAM cell (so the value reads back)
   and the cartridge's paging, so we forward it to both. *)
module Make (Cartridge : Addressable_intf.S) = struct
  type t =
    { cartridge : Cartridge.t
    ; ram : Ram.t (* covers 0xC000-0xDFFF *)
    }

  let create ~cartridge ~ram = { cartridge; ram }

  (* Fold any address at/above 0xC000 into the 8 KB RAM window, giving the
     0xE000-0xFFFF mirror for free. *)
  let ram_addr addr = Uint16.of_int (0xC000 lor (Uint16.to_int addr land 0x1FFF))

  let read_byte t addr =
    if Uint16.to_int addr < 0xC000
    then Cartridge.read_byte t.cartridge addr
    else Ram.read_byte t.ram (ram_addr addr)
  ;;

  let write_byte t ~addr ~data =
    let a = Uint16.to_int addr in
    if a < 0xC000
    then Cartridge.write_byte t.cartridge ~addr ~data
    else (
      Ram.write_byte t.ram ~addr:(ram_addr addr) ~data;
      (* Mapper registers overlay the top of RAM; let the cartridge snoop. *)
      if a >= 0xFFFC then Cartridge.write_byte t.cartridge ~addr ~data)
  ;;

  let accepts _ _ = true

  let read_word t addr =
    let lo = Uint8.to_int (read_byte t addr) in
    let hi = Uint8.to_int (read_byte t Uint16.(succ addr)) in
    (hi lsl 8) lor lo |> Uint16.of_int
  ;;

  let write_word t ~addr ~data =
    let data = Uint16.to_int data in
    write_byte t ~addr ~data:(Uint8.of_int (data land 0xFF));
    write_byte t ~addr:Uint16.(succ addr) ~data:(Uint8.of_int (data lsr 8))
  ;;
end
