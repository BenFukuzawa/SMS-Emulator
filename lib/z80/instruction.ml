open Uints

type _ arg =
  | Immediate8 : uint8 -> uint8 arg
  | Immediate16 : uint16 -> uint16 arg
  | Direct8 : uint8 -> uint8 arg
  | Direct16 : uint16 -> uint16 arg
  | R : Registers.r -> uint8 arg
  | RR : Registers.rr -> uint16 arg
  | RR_indirect : Registers.rr -> uint8 arg
  | IX_indirect : uint8 -> uint8 arg
  | IY_indirect : uint8 -> uint8 arg

type condition =
  | C (* Carry *)
  | NC (* No Carry *)
  | Z (* Zero *)
  | NZ (* Not zero *)
  | M (* Minus (negative) *)
  | P (* Plus (positive) *)
  | PE (* Parity Even / Overflow *)
  | PO (* Parity Odd. No overflow *)

type t =
  | ADC8 of uint8 arg * uint8 arg
  | ADC16 of uint16 arg * uint16 arg
  | ADD8 of uint8 arg * uint8 arg
  | ADD16 of uint16 arg * uint16 arg
  | AND8 of uint8 arg * uint8 arg
  | BIT of int * uint8 arg
  | CALL of condition * uint16
  | CCF
  | CP of uint8 arg
  | CPD
  | CPDR
  | CPI
  | CPIR
  | CPL
  | DAA
  | DEC of uint8 arg (* 8 bit registers only *)
  | DI
  | DJNZ
  | EI
  | EX of uint8 arg * uint8 arg
  | EXX
  | HALT
  | IM of int
  | IN of port * uint8 arg
  | INC8 of uint8 arg
  | INC16 of uint16 arg
  | IND
  | INDR
  | INI
  | INIR
  | JP of condition * uint16 arg
  | JR of condition * int8
    (* use int8 hre because int8 covers -128 to 127 while uint9 is 0 to 255 *)
  | LD8 of uint8 arg * uint8 arg
  | LD16 of uint16 arg * uint16 arg
  | LDD
  | LDDR
  | LDI
  | LDIR
  | NEG
  | NOP
  | OR of uint8 arg
  | OTDR
  | OTIR
  | OUT of uint8 arg * port
  | OUTD
  | OUTI
  | POP of uint16 arg
  | PUSH of uint16 arg
  | RES of int
  | RET of condition
  | RETI
  | RETN
  | RL of uint8
  (* | RLA *)
  | RLC of uint8
  | RLCA
  | RLD
  | RR of uint8
  | RRA
  | RRC of uint8
  | RRCA
  | RRD
  | RST of uint16
  | SBC8 of uint8 arg * uint8 arg
  | SBC16 of uint16 arg * uint16 arg
  | SCF
  | SET of int * uint8 arg
  | SLA of uint8 arg
  | SLL of uint8 arg
  | SRA of uint8 arg
  | SRL of uint8 arg
  | SUB of uint8 arg
  | XOR of uint8 arg

let show t =
  let show_arg : type a. a arg -> string = function
    | Immediate8 n -> Uint8.show n
    | Immediate16 n -> Uint16.show n
    | Direct8 n | Direct16 n -> Printf.sprintf "(%s)" (Uint16.show n)
    | R r -> Registers.show_r r
    | RR rr -> Registers.show_rr rr
    | RR_indirect rr -> Printf.sprintf "(%s)" (Registers.show_rr rr)
    | FF00_offset n -> Printf.sprintf "($FF00+%s)" (Uint8.show n)
    | FF00_C -> "($FF00+C)"
    | HL_inc -> "(HL+)"
    | HL_dec -> "(HL-)"
    | SP -> "SP"
    | SP_offset n ->
      if Int8.is_neg n
      then Printf.sprintf "SP-%s" Int8.(show @@ abs n)
      else Printf.sprintf "SP+%s" Int8.(show n)
  in
  let show_condition = function
    | None -> ""
    | NZ -> "NZ"
    | Z -> "Z"
    | NC -> "NC"
    | C -> "C"
  in
  match t with
  | LD8 (x, y) -> Printf.sprintf "LD %s, %s" (show_arg x) (show_arg y)
  | LD16 (x, y) -> Printf.sprintf "LD %s, %s" (show_arg x) (show_arg y)
  | ADD8 (x, y) -> Printf.sprintf "ADD %s, %s" (show_arg x) (show_arg y)
  | ADD16 (x, y) -> Printf.sprintf "ADD %s, %s" (show_arg x) (show_arg y)
  | ADDSP y -> Printf.sprintf "ADD SP, %s" (Int8.show y)
  | ADC (x, y) -> Printf.sprintf "ADC %s, %s" (show_arg x) (show_arg y)
  | SUB (x, y) -> Printf.sprintf "SUB %s, %s" (show_arg x) (show_arg y)
  | SBC (x, y) -> Printf.sprintf "SBC %s, %s" (show_arg x) (show_arg y)
  | AND (x, y) -> Printf.sprintf "AND %s, %s" (show_arg x) (show_arg y)
  | OR (x, y) -> Printf.sprintf "OR %s, %s" (show_arg x) (show_arg y)
  | XOR (x, y) -> Printf.sprintf "XOR %s, %s" (show_arg x) (show_arg y)
  | CP (x, y) -> Printf.sprintf "CP %s, %s" (show_arg x) (show_arg y)
  | INC x -> Printf.sprintf "INC %s" (show_arg x)
  | INC16 x -> Printf.sprintf "INC %s" (show_arg x)
  | DEC x -> Printf.sprintf "DEC %s" (show_arg x)
  | DEC16 x -> Printf.sprintf "DEC %s" (show_arg x)
  | SWAP x -> Printf.sprintf "SWAP %s" (show_arg x)
  | DAA -> Printf.sprintf "DAA"
  | CPL -> Printf.sprintf "CPL"
  | CCF -> Printf.sprintf "CCF"
  | SCF -> Printf.sprintf "SCF"
  | NOP -> Printf.sprintf "NOP"
  | HALT -> Printf.sprintf "HALT"
  | STOP -> Printf.sprintf "STOP"
  | DI -> Printf.sprintf "DI"
  | EI -> Printf.sprintf "EI"
  | RLCA -> Printf.sprintf "RLCA"
  | RLA -> Printf.sprintf "RLA"
  | RRCA -> Printf.sprintf "RRCA"
  | RRA -> Printf.sprintf "RRA"
  | RLC x -> Printf.sprintf "RLC %s" (show_arg x)
  | RL x -> Printf.sprintf "RL %s" (show_arg x)
  | RRC x -> Printf.sprintf "RRC %s" (show_arg x)
  | RR x -> Printf.sprintf "RR %s" (show_arg x)
  | SLA x -> Printf.sprintf "SLA %s" (show_arg x)
  | SRA x -> Printf.sprintf "SRA %s" (show_arg x)
  | SRL x -> Printf.sprintf "SRL %s" (show_arg x)
  | BIT (n, x) -> Printf.sprintf "BIT %d, %s" n (show_arg x)
  | SET (n, x) -> Printf.sprintf "SET %d, %s" n (show_arg x)
  | RES (n, x) -> Printf.sprintf "RES %d, %s" n (show_arg x)
  | PUSH rr -> Printf.sprintf "PUSH %s" (Registers.show_rr rr)
  | POP rr -> Printf.sprintf "POP %s" (Registers.show_rr rr)
  | JP (c, x) ->
    (match c with
     | None -> Printf.sprintf "JP %s" (show_arg x)
     | NZ | Z | NC | C ->
       Printf.sprintf "JP %s, %s" (show_condition c) (show_arg x))
  | JR (c, x) ->
    (match c with
     | None -> Printf.sprintf "JR %s" (Int8.show x)
     | NZ | Z | NC | C ->
       Printf.sprintf "JR %s, %s" (show_condition c) (Int8.show x))
  | CALL (c, x) ->
    (match c with
     | None -> Printf.sprintf "CALL %s" (Uint16.show x)
     | NZ | Z | NC | C ->
       Printf.sprintf "CALL %s, %s" (show_condition c) (Uint16.show x))
  | RST x -> Printf.sprintf "RST %s" (Uint16.show x)
  | RET c -> Printf.sprintf "RET %s" (show_condition c)
  | RETI -> Printf.sprintf "RETI"
;;
