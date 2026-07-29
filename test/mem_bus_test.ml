(* Exercises the Sega mapper + SMS memory map. Each ROM bank is filled with
   its own index so a read tells you which bank a slot points at. *)
open Uints

module Bus = Mem_bus.Make (Cartridge)

let failures = ref 0

let check name ~expect ~got =
  if expect = got
  then Printf.printf "ok   %-26s 0x%02X\n" name got
  else (
    incr failures;
    Printf.printf "FAIL %-26s expected 0x%02X got 0x%02X\n" name expect got)
;;

let () =
  let nbanks = 8 in
  let rom = Bytes.create (nbanks * 0x4000) in
  for b = 0 to nbanks - 1 do
    Bytes.fill rom (b * 0x4000) 0x4000 (Char.chr b)
  done;
  let cart = Cartridge.create ~rom in
  let ram =
    Ram.create
      ~start_addr:(Uint16.of_int 0xC000)
      ~end_addr:(Uint16.of_int 0xDFFF)
  in
  let bus = Bus.create ~cartridge:cart ~ram in
  let rd a = Uint8.to_int (Bus.read_byte bus (Uint16.of_int a)) in
  let wr a v = Bus.write_byte bus ~addr:(Uint16.of_int a) ~data:(Uint8.of_int v) in
  (* default pages: slot0=0, slot1=1, slot2=2 *)
  check "slot0 read" ~expect:0 ~got:(rd 0x2000);
  check "slot1 read" ~expect:1 ~got:(rd 0x4000);
  check "slot2 read" ~expect:2 ~got:(rd 0x8000);
  (* bank switching *)
  wr 0xFFFE 3;
  check "slot1 -> bank 3" ~expect:3 ~got:(rd 0x4000);
  wr 0xFFFF 5;
  check "slot2 -> bank 5" ~expect:5 ~got:(rd 0x8000);
  (* slot 0 pages, but the first 1 KB is fixed to bank 0 *)
  wr 0xFFFD 4;
  check "slot0 -> bank 4" ~expect:4 ~got:(rd 0x0400);
  check "first 1KB fixed" ~expect:0 ~got:(rd 0x0000);
  (* system RAM and its 0xE000 mirror *)
  wr 0xC123 0x55;
  check "ram read" ~expect:0x55 ~got:(rd 0xC123);
  check "ram mirror" ~expect:0x55 ~got:(rd 0xE123);
  (* mapper registers read back from the RAM they overlay *)
  check "reg 0xFFFE readback" ~expect:3 ~got:(rd 0xFFFE);
  (* cartridge RAM paged into slot 2 (control bit 3) *)
  wr 0xFFFC 0x08;
  wr 0x8000 0x99;
  check "cart RAM read" ~expect:0x99 ~got:(rd 0x8000);
  wr 0xFFFC 0x00;
  check "slot2 ROM again" ~expect:5 ~got:(rd 0x8000);
  if !failures = 0
  then Printf.printf "\nmem_bus: ALL PASS\n"
  else (
    Printf.printf "\nmem_bus: %d FAILED\n" !failures;
    exit 1)
;;
