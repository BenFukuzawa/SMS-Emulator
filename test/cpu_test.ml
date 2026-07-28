open Uints

(* Flat 64KB memory. No cartridge, no mapper, no mirroring -- every address
   is plain RAM. This is deliberately not the real SMS bus: it exists so the
   CPU can be exercised before the bus is finished, and it is also exactly
   what SingleStepTests wants later. *)
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

  (* Load raw bytes at an address, so tests can write machine code as a list
     of ints rather than building a ROM image. *)
  let load t ~addr program =
    List.iteri
      (fun i byte ->
        write_byte
          t
          ~addr:(Uint16.of_int (Uint16.to_int addr + i))
          ~data:(Uint8.of_int byte))
      program
  ;;
end

(* Nothing is plugged into the I/O space yet. Reads see open bus. *)
module Null_io = struct
  type t = unit

  let create () = ()

  let read_port () ~port =
    ignore port;
    Uint8.of_int 0xFF
  ;;

  let write_port () ~port ~data =
    ignore port;
    ignore data
  ;;
end

module Cpu = Z80.Make (Flat_memory) (Null_io)

let run_program ~name ~program ~steps =
  let bus = Flat_memory.create () in
  Flat_memory.load bus ~addr:Uint16.zero program;
  let registers = Registers.create () in
  let io = Null_io.create () in
  let cpu = Cpu.create ~bus ~registers ~io in
  Printf.printf "=== %s ===\n" name;
  Printf.printf "start: %s\n" (Registers.show registers);
  let total = ref 0 in
  for _ = 1 to steps do
    let cycles = Cpu.run_instruction cpu in
    total := !total + cycles;
    Printf.printf
      "  %-22s %s\n"
      (Cpu.last_inst cpu)
      (Registers.show registers)
  done;
  Printf.printf
    "end:   %s  (%d T-states)\n\n"
    (Registers.show registers)
    !total
;;

let () =
  (* LD A,$05 / LD B,$03 / ADD A,B / HALT -> A should be $08 *)
  run_program
    ~name:"8-bit addition"
    ~program:[ 0x3E; 0x05; 0x06; 0x03; 0x80; 0x76 ]
    ~steps:4;
  (* LD A,$FF / INC A -> A wraps to $00, Z and H set, C untouched *)
  run_program
    ~name:"INC wraparound"
    ~program:[ 0x3E; 0xFF; 0x3C; 0x76 ]
    ~steps:3;
  (* LD HL,$C000 / LD (HL),$42 / LD A,(HL) -> A = $42 via memory *)
  run_program
    ~name:"memory round trip"
    ~program:[ 0x21; 0x00; 0xC0; 0x36; 0x42; 0x7E; 0x76 ]
    ~steps:4;
  (* LD B,$03 / DJNZ -2 / HALT -> loops until B hits zero *)
  run_program
    ~name:"DJNZ loop"
    ~program:[ 0x06; 0x03; 0x10; 0xFE; 0x76 ]
    ~steps:5
;;
