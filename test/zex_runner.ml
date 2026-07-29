(* ZEXDOC / ZEXALL harness.

   These are CP/M .com programs (the Z80 Instruction Set Exerciser). They run
   in a flat 64 KB address space, are loaded at 0x0100, and print their output
   via CP/M BDOS calls (CALL 5):

     C = 2  -> print the character in E
     C = 9  -> print the '$'-terminated string at DE
     C = 0  -> warm boot (exit)

   The program finishes by jumping to 0x0000 (warm boot). We trap PC = 0x0005
   in software, service the BDOS call, and simulate the RET ourselves, so no
   real CP/M is needed.

   Usage: zex_runner [path-to.com]   (defaults to test/roms/zexdoc.com) *)

open Uints

(* Flat 64 KB RAM: every address is plain memory, no mapper or mirroring. *)
module Flat_memory = struct
  type t = { bytes : Bytes.t }

  let create () = { bytes = Bytes.make 0x10000 '\x00' }
  let accepts _ _ = true

  let read_byte t addr =
    Bytes.get t.bytes (Uint16.to_int addr) |> Uint8.of_char
  ;;

  let write_byte t ~addr ~data =
    Bytes.set t.bytes (Uint16.to_int addr) (Uint8.to_char data)
  ;;

  let read_word t addr =
    let lo = Uint8.to_int (read_byte t addr) in
    let hi = Uint8.to_int (read_byte t Uint16.(succ addr)) in
    Uint16.of_int ((hi lsl 8) lor lo)
  ;;

  let write_word t ~addr ~data =
    let data = Uint16.to_int data in
    write_byte t ~addr ~data:(Uint8.of_int (data land 0xFF));
    write_byte t ~addr:Uint16.(succ addr) ~data:(Uint8.of_int (data lsr 8))
  ;;
end

(* Nothing is plugged into the I/O space; reads see open bus ($FF). *)
module Null_io = struct
  type t = unit

  let create () = ()
  let read_port () ~port:_ = Uint8.of_int 0xFF
  let write_port () ~port:_ ~data:_ = ()
end

module Cpu = Z80.Make (Flat_memory) (Null_io)

let load_com bus path ~addr =
  let ic = open_in_bin path in
  let len = in_channel_length ic in
  let buf = Bytes.create len in
  really_input ic buf 0 len;
  close_in ic;
  Bytes.iteri
    (fun i b ->
      Flat_memory.write_byte
        bus
        ~addr:(Uint16.of_int (addr + i))
        ~data:(Uint8.of_int (Char.code b)))
    buf;
  len
;;

let () =
  let path =
    if Array.length Sys.argv > 1 then Sys.argv.(1) else "test/roms/zexdoc.com"
  in
  let bus = Flat_memory.create () in
  let len = load_com bus path ~addr:0x0100 in
  let regs = Registers.create () in
  let io = Null_io.create () in
  let cpu = Cpu.create ~bus ~io ~registers:regs in
  Cpu.For_tests.set_pc cpu (Uint16.of_int 0x0100);
  (* A sane stack in high RAM in case the program leans on the loader's SP. *)
  Registers.write_rr regs Registers.SP (Uint16.of_int 0xF000);
  Printf.eprintf "Loaded %s (%d bytes) at 0x0100\n%!" path len;
  (* BDOS output: stream live and flush on newline so we see per-test results
     while the run is still going. *)
  let emit_char c =
    print_char c;
    if Char.equal c '\n' then flush stdout
  in
  let bdos () =
    match Uint8.to_int (Registers.read_r regs Registers.C) with
    | 2 -> emit_char (Uint8.to_char (Registers.read_r regs Registers.E))
    | 9 ->
      let de = ref (Uint16.to_int (Registers.read_rr regs Registers.DE)) in
      let continue = ref true in
      while !continue do
        let c = Flat_memory.read_byte bus (Uint16.of_int !de) in
        if Char.equal (Uint8.to_char c) '$'
        then continue := false
        else (
          emit_char (Uint8.to_char c);
          de := (!de + 1) land 0xFFFF)
      done
    | _ -> ()
  in
  (* Simulate RET: pop the return address off SP and jump to it. *)
  let ret () =
    let sp = Uint16.to_int (Registers.read_rr regs Registers.SP) in
    let ret_addr = Flat_memory.read_word bus (Uint16.of_int sp) in
    Registers.write_rr regs Registers.SP (Uint16.of_int ((sp + 2) land 0xFFFF));
    Cpu.For_tests.set_pc cpu ret_addr
  in
  let instrs = ref 0 in
  let finished = ref false in
  while not !finished do
    let pc = Uint16.to_int (Cpu.For_tests.pc cpu) in
    if pc = 0x0000
    then finished := true (* warm boot: program done *)
    else if pc = 0x0005
    then (
      let c = Uint8.to_int (Registers.read_r regs Registers.C) in
      if c = 0 then finished := true else (bdos (); ret ()))
    else (
      ignore (Cpu.run_instruction cpu : int);
      incr instrs;
      if !instrs land 0x3FFFFFFF = 0
      then Printf.eprintf "... %d instructions\n%!" !instrs)
  done;
  flush stdout;
  Printf.eprintf "\nDone after %d instructions.\n%!" !instrs
;;
