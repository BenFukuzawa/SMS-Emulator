open Uints

module Make (Cartridge : Addressable_intf.S) = struct
  type t =
    { cartridge : Cartridge.t
    ; ram : Ram.t
    }

  let create ~cartridge ~ram = { cartridge; ram }

  let read_byte t addr =
    if Ram.accepts t.ram addr
    then Ram.read_byte t.ram addr
    else if Cartridge.accepts t.cartridge addr
    then Cartridge.read_byte t.cartridge addr
    else Uint8.of_int 0xFF
  ;;

  let write_byte t ~(addr : uint16) ~(data : uint8) =
    if Ram.accepts t.ram addr
    then Ram.write_byte t.ram ~addr ~data
    else if Cartridge.accepts t.cartridge addr
    then Cartridge.write_byte t.cartridge ~addr ~data
  ;;

  let accepts t addr =
    Cartridge.accepts t.cartridge addr || Ram.accepts t.ram addr
  ;;

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
