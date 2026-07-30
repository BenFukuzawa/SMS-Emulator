open Uints

(* Sega mapper cartridge.

   The CPU's low 48 KB (0x0000-0xBFFF) is three 16 KB slots into ROM, each
   pointed at a bank by a control register that lives at the very top of the
   address space (4th space is taken up by the RAM):

   slot 0 0x0000-0x3FFF bank = reg 0xFFFD slot 1 0x4000-0x7FFF bank = reg
   0xFFFE slot 2 0x8000-0xBFFF bank = reg 0xFFFF, or cartridge RAM (reg
   0xFFFC)

   The first 1 KB (0x0000-0x03FF) is *always* ROM bank 0 regardless of reg
   0xFFFD, so the RST/interrupt vectors can never be paged out.

   Reg 0xFFFC controls on-cartridge RAM: bit 3 maps it into slot 2, bit 2
   selects which of the two 16 KB RAM banks. *)

let bank_size = 0x4000

type t =
  { rom : Bytes.t
  ; num_banks : int
  ; cart_ram : Bytes.t (* two 16 KB banks *)
  ; mutable page0 : int (* reg 0xFFFD *)
  ; mutable page1 : int (* reg 0xFFFE *)
  ; mutable page2 : int (* reg 0xFFFF *)
  ; mutable ram_control : int (* reg 0xFFFC *)
  }

let create ~rom =
  let len = Bytes.length rom in
  let num_banks = max 1 ((len + bank_size - 1) / bank_size) in
  { rom
  ; num_banks
  ; cart_ram = Bytes.make (2 * bank_size) '\x00'
  ; page0 = 0
  ; page1 = 1
  ; page2 = 2
  ; ram_control = 0
  }
;;

let of_file path =
  let ic = open_in_bin path in
  let len = in_channel_length ic in
  let rom = Bytes.create len in
  really_input ic rom 0 len;
  close_in ic;
  create ~rom
;;

let pages t = t.page0, t.page1, t.page2

(* Cartridge RAM is enabled in slot 2 by bit 3 of the control register; bit 2
   selects the bank. *)
let ram_in_slot2 t = t.ram_control land 0x08 <> 0
let ram_bank t = (t.ram_control lsr 2) land 1

let read_rom t bank offset =
  let bank = bank mod t.num_banks in
  let idx = (bank * bank_size) + offset in
  if idx < Bytes.length t.rom
  then Bytes.get t.rom idx |> Uint8.of_char
  else Uint8.of_int 0xFF
;;

let read_byte t addr =
  let a = Uint16.to_int addr in
  if a < 0x0400
  then read_rom t 0 a (* fixed to bank 0 *)
  else if a < 0x4000
  then read_rom t t.page0 a
  else if a < 0x8000
  then read_rom t t.page1 (a - 0x4000)
  else if a < 0xC000
  then
    if ram_in_slot2 t
    then
      Bytes.get t.cart_ram ((ram_bank t * bank_size) + (a - 0x8000))
      |> Uint8.of_char
    else read_rom t t.page2 (a - 0x8000)
  else Uint8.of_int 0xFF
;;

let write_byte t ~addr ~data =
  let a = Uint16.to_int addr in
  if a >= 0xFFFC
  then (
    let d = Uint8.to_int data in
    match a with
    | 0xFFFC -> t.ram_control <- d
    | 0xFFFD -> t.page0 <- d
    | 0xFFFE -> t.page1 <- d
    | _ -> t.page2 <- d)
  else if a >= 0x8000 && a < 0xC000 && ram_in_slot2 t
  then
    Bytes.set
      t.cart_ram
      ((ram_bank t * bank_size) + (a - 0x8000))
      (Uint8.to_char data)
;;

(* ROM is not writable; other writes are ignored. *)

let accepts _ addr =
  let a = Uint16.to_int addr in
  a < 0xC000 || a >= 0xFFFC
;;
