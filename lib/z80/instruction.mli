open Uints

type _ arg =
  | Immediate8 : uint8 -> uint8 arg
  | Immediate16 : uint16 -> uint16 arg
  | Direct8 : uint16 -> uint8 arg
  | Direct16 : uint16 -> uint16 arg
  | R : Registers.r -> uint8 arg
  | RR : Registers.rr -> uint16 arg
  | RR_indirect : Registers.rr -> uint8 arg
  | IX_indirect : int8 -> uint8 arg
  | IY_indirect : int8 -> uint8 arg

type out_value =
  | Out_register of Registers.r
  | Out_zero

type port =
  | Port_n of uint8 (* (n) : immediate port number *)
  | Port_C (* (C) : port number held in register C *)

type condition =
  | C (* Carry *)
  | NC (* No Carry *)
  | Z (* Zero *)
  | NZ (* Not zero *)
  | M (* Minus (negative) *)
  | P (* Plus (positive) *)
  | PE (* Parity Even / Overflow *)
  | PO (* Parity Odd / No overflow *)
  | None (* Unconditional *)

type t =
  | ADD8 of uint8 arg
  | ADC8 of uint8 arg
  | SUB of uint8 arg
  | SBC8 of uint8 arg
  | AND of uint8 arg
  | OR of uint8 arg
  | XOR of uint8 arg
  | CP of uint8 arg
  | NEG
  | DAA
  | CPL
  | ADD16 of uint16 arg * uint16 arg
  | ADC16 of uint16 arg * uint16 arg
  | SBC16 of uint16 arg * uint16 arg
  | INC8 of uint8 arg
  | DEC8 of uint8 arg
  | INC16 of uint16 arg
  | DEC16 of uint16 arg
  | LD8 of uint8 arg * uint8 arg
  | LD16 of uint16 arg * uint16 arg
  (* Interrupt/refresh register loads. These are special-cased rather than
     routed through [arg] because I and R are not general-purpose operands,
     and LD A,I / LD A,R affect flags unlike every other load. *)
  | LD_A_I
  | LD_I_A
  | LD_A_R
  | LD_R_A
  (* --- Exchange --- Only four forms exist, and AF' (the shadow pair) is not
     expressible through [arg], so each gets its own constructor. *)
  | EX_DE_HL (* EX DE,HL *)
  | EX_AF_AF (* EX AF,AF' *)
  | EX_SP_indirect of Registers.rr (* EX (SP),HL / (SP),IX / (SP),IY *)
  | EXX
  | LDI
  | LDIR
  | LDD
  | LDDR
  | CPI
  | CPIR
  | CPD
  | CPDR
  (* --- Port I/O --- Operand order follows the mnemonics: IN r,(port) and
     OUT (port),r. The register side is always a plain 8-bit register. *)
  | IN of Registers.r option * port
  | OUT of port * out_value
  (* --- Block I/O --- *)
  | INI
  | INIR
  | IND
  | INDR
  | OUTI
  | OTIR
  | OUTD
  | OTDR
  | RLCA
  | RLA
  | RRCA
  | RRA
  | RLC of uint8 arg * uint8 arg option
  | RL of uint8 arg * uint8 arg option
  | RRC of uint8 arg * uint8 arg option
  | RR_rot of uint8 arg * uint8 arg option
  | SLA of uint8 arg * uint8 arg option
  | SRA of uint8 arg * uint8 arg option
  | SLL of uint8 arg * uint8 arg option
  | SRL of uint8 arg * uint8 arg option
  | RLD
  | RRD
  | BIT of int * uint8 arg
  | SET of int * uint8 arg
  | RES of int * uint8 arg
  | PUSH of Registers.rr
  | POP of Registers.rr
  | JP of condition * uint16 arg
  | JR of condition * int8
  | DJNZ of int8
  | CALL of condition * uint16
  | RET of condition
  | RETI
  | RETN
  | RST of uint16
  (* --- CPU control --- *)
  | NOP
  | HALT
  | DI
  | EI
  | IM of int
  | CCF
  | SCF

val show : t -> string
