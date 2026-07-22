open Uints

(* Different addressing modes for the zilog z80 cpu user manual *)
(* need to account for both 8 bit and 16 bit operations *)
type _ arg =
  | Immediate8 : uint8 -> uint8 arg
  | Immediate16 : uint16 -> uint16 arg
  | Direct8 : uint16 -> uint8 arg
  | Direct16 : uint16 -> uint16 arg
  | R : Registers.r -> uint8 arg
  | RR : Registers.rr -> uint16 arg
  | RR_indirect : Registers.rr -> uint8 arg
  | IX_indirect : uint8 -> uint8 arg
  | IY_indirect : uint8 -> uint8 arg

type port =
  | Port_n of uint8 (* (n) — immediate port number *)
  | Port_C (* (C) — port number held in register C *)

type condition =
  | C (* Carry *)
  | NC (* No Carry *)
  | Z (* Zero *)
  | NZ (* Not zero *)
  | M (* Minus (negative) *)
  | P (* Plus (positive) *)
  | PE (* Parity Even / Overflow *)
  | PO (* Parity Odd. No overflow *)
  | None

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

val show : t -> string
