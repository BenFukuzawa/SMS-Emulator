(* SingleStepTests harness.

   Runs the per-opcode JSON test vectors from https://github.com/SingleStepTests/z80
   (each file = 1000 cases for one opcode, with exact initial and final CPU
   state). For every case we load the initial state, execute exactly one
   instruction, and diff our final state against the expected one.

   Usage: sst_runner test/sst/27.json [max_failures_to_print] *)

open Uints

module Flat_memory = struct
  type t = { bytes : Bytes.t }

  let create () = { bytes = Bytes.make 0x10000 '\x00' }
  let accepts _ _ = true
  let read_byte t addr = Bytes.get t.bytes (Uint16.to_int addr) |> Uint8.of_char

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

module Null_io = struct
  type t = unit

  let create () = ()
  let read_port () ~port:_ = Uint8.of_int 0xFF
  let write_port () ~port:_ ~data:_ = ()
end

module Cpu = Z80.Make (Flat_memory) (Null_io)
module J = Yojson.Safe.Util

let g st k = J.member k st |> J.to_int

let load_state cpu bus regs st =
  let g = g st in
  Registers.write_rr regs Registers.AF (Uint16.of_int ((g "a" lsl 8) lor g "f"));
  Registers.write_rr regs Registers.BC (Uint16.of_int ((g "b" lsl 8) lor g "c"));
  Registers.write_rr regs Registers.DE (Uint16.of_int ((g "d" lsl 8) lor g "e"));
  Registers.write_rr regs Registers.HL (Uint16.of_int ((g "h" lsl 8) lor g "l"));
  Registers.write_rr regs Registers.SP (Uint16.of_int (g "sp"));
  Registers.write_rr regs Registers.IX (Uint16.of_int (g "ix"));
  Registers.write_rr regs Registers.IY (Uint16.of_int (g "iy"));
  Cpu.For_tests.set_pc cpu (Uint16.of_int (g "pc"));
  Cpu.For_tests.set_q cpu (Uint8.of_int (g "q"));
  Cpu.For_tests.set_interrupt_state
    cpu
    ~iff1:(g "iff1" = 1)
    ~iff2:(g "iff2" = 1)
    ~im:(g "im")
    ~i:(Uint8.of_int (g "i"))
    ~refresh:(Uint8.of_int (g "r"))
    ~halted:false;
  J.member "ram" st
  |> J.to_list
  |> List.iter (fun pair ->
    match J.to_list pair with
    | [ a; v ] ->
      Flat_memory.write_byte
        bus
        ~addr:(Uint16.of_int (J.to_int a))
        ~data:(Uint8.of_int (J.to_int v))
    | _ -> ())
;;

(* Fields we can read back and compare. (Shadow registers aren't settable
   through the current Registers API, so we skip them; the opcodes under test
   don't touch them.) *)
let diff_state cpu bus regs st =
  let expect k = g st k in
  let r8 name r = name, expect name, Uint8.to_int (Registers.read_r regs r) in
  let checks =
    [ r8 "a" Registers.A
    ; r8 "b" Registers.B
    ; r8 "c" Registers.C
    ; r8 "d" Registers.D
    ; r8 "e" Registers.E
    ; r8 "h" Registers.H
    ; r8 "l" Registers.L
    ; "f", expect "f", Uint16.to_int (Registers.read_rr regs Registers.AF) land 0xFF
    ; "pc", expect "pc", Uint16.to_int (Cpu.For_tests.pc cpu)
    ; "sp", expect "sp", Uint16.to_int (Registers.read_rr regs Registers.SP)
    ; "ix", expect "ix", Uint16.to_int (Registers.read_rr regs Registers.IX)
    ; "iy", expect "iy", Uint16.to_int (Registers.read_rr regs Registers.IY)
    ]
  in
  let reg_mismatches =
    List.filter_map
      (fun (name, exp, got) ->
        if exp = got then None else Some (name, exp, got))
      checks
  in
  let ram_mismatches =
    J.member "ram" st
    |> J.to_list
    |> List.filter_map (fun pair ->
      match J.to_list pair with
      | [ a; v ] ->
        let addr = J.to_int a in
        let exp = J.to_int v in
        let got = Uint8.to_int (Flat_memory.read_byte bus (Uint16.of_int addr)) in
        if exp = got
        then None
        else Some (Printf.sprintf "ram[%d]" addr, exp, got)
      | _ -> None)
  in
  reg_mismatches @ ram_mismatches
;;

let () =
  let path = Sys.argv.(1) in
  let max_print =
    if Array.length Sys.argv > 2 then int_of_string Sys.argv.(2) else 8
  in
  let tests = Yojson.Safe.from_file path |> J.to_list in
  let total = List.length tests in
  let passed = ref 0 in
  let printed = ref 0 in
  List.iter
    (fun test ->
      let name = J.member "name" test |> J.to_string in
      let initial = J.member "initial" test in
      let final = J.member "final" test in
      let bus = Flat_memory.create () in
      let regs = Registers.create () in
      let io = Null_io.create () in
      let cpu = Cpu.create ~bus ~io ~registers:regs in
      load_state cpu bus regs initial;
      let f_in = g initial "f" and a_in = g initial "a" in
      ignore (Cpu.run_instruction cpu : int);
      match diff_state cpu bus regs final with
      | [] -> incr passed
      | mismatches ->
        if !printed < max_print
        then (
          incr printed;
          Printf.printf
            "FAIL %s  in: a=%02X f=%02X\n"
            name
            a_in
            f_in;
          List.iter
            (fun (field, exp, got) ->
              Printf.printf
                "     %-8s expected %02X  got %02X  (xor %02X)\n"
                field
                exp
                got
                (exp lxor got))
            mismatches))
    tests;
  Printf.printf
    "\n%s: %d/%d passed (%d failed)\n"
    path
    !passed
    total
    (total - !passed)
;;
