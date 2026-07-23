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

type port =
  | Port_n of uint8
  | Port_C

type condition =
  | C
  | NC
  | Z
  | NZ
  | M
  | P
  | PE
  | PO
  | None

type t =
  (* 8-bit arithmetic and logic *)
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
  (* 16-bit arithmetic *)
  | ADD16 of uint16 arg * uint16 arg
  | ADC16 of uint16 arg * uint16 arg
  | SBC16 of uint16 arg * uint16 arg
  (* Increment / decrement *)
  | INC8 of uint8 arg
  | DEC8 of uint8 arg
  | INC16 of uint16 arg
  | DEC16 of uint16 arg
  (* Loads *)
  | LD8 of uint8 arg * uint8 arg
  | LD16 of uint16 arg * uint16 arg
  | LD_A_I
  | LD_I_A
  | LD_A_R
  | LD_R_A
  (* Exchange *)
  | EX_DE_HL
  | EX_AF_AF
  | EX_SP_indirect of Registers.rr
  | EXX
  (* Block transfer and search *)
  | LDI
  | LDIR
  | LDD
  | LDDR
  | CPI
  | CPIR
  | CPD
  | CPDR
  (* Port I/O *)
  | IN of Registers.r * port
  | OUT of port * Registers.r
  (* Block I/O *)
  | INI
  | INIR
  | IND
  | INDR
  | OUTI
  | OTIR
  | OUTD
  | OTDR
  (* Rotates and shifts *)
  | RLCA
  | RLA
  | RRCA
  | RRA
  | RLC of uint8 arg
  | RL of uint8 arg
  | RRC of uint8 arg
  | RR_rot of uint8 arg
  | SLA of uint8 arg
  | SRA of uint8 arg
  | SLL of uint8 arg
  | SRL of uint8 arg
  | RLD
  | RRD
  (* Bit manipulation *)
  | BIT of int * uint8 arg
  | SET of int * uint8 arg
  | RES of int * uint8 arg
  (* Stack *)
  | PUSH of Registers.rr
  | POP of Registers.rr
  (* Control flow *)
  | JP of condition * uint16 arg
  | JR of condition * int8
  | DJNZ of int8
  | CALL of condition * uint16
  | RET of condition
  | RETI
  | RETN
  | RST of uint16
  (* CPU control *)
  | NOP
  | HALT
  | DI
  | EI
  | IM of int
  | CCF
  | SCF

let show_displacement name d =
  if Int8.is_neg d
  then Printf.sprintf "(%s-%s)" name Int8.(show @@ abs d)
  else Printf.sprintf "(%s+%s)" name Int8.(show d)
;;

let show_condition = function
  | C -> "C"
  | NC -> "NC"
  | Z -> "Z"
  | NZ -> "NZ"
  | M -> "M"
  | P -> "P"
  | PE -> "PE"
  | PO -> "PO"
  | None -> ""
;;

(* Condition as it appears before an operand: "Z, " or "" when unconditional. *)
let show_condition_prefix = function
  | None -> ""
  | c -> show_condition c ^ ", "
;;

let show_port = function
  | Port_n n -> Printf.sprintf "(%s)" (Uint8.show n)
  | Port_C -> "(C)"
;;

let show t =
  let show_arg : type a. a arg -> string = function
    | Immediate8 n -> Uint8.show n
    | Immediate16 n -> Uint16.show n
    | Direct8 nn -> Printf.sprintf "(%s)" (Uint16.show nn)
    | Direct16 nn -> Printf.sprintf "(%s)" (Uint16.show nn)
    | R r -> Registers.show_r r
    | RR rr -> Registers.show_rr rr
    | RR_indirect rr -> Printf.sprintf "(%s)" (Registers.show_rr rr)
    | IX_indirect d -> show_displacement "IX" d
    | IY_indirect d -> show_displacement "IY" d
  in
  match t with
  (* --- 8-bit arithmetic and logic --- *)
  | ADD8 x -> Printf.sprintf "ADD A, %s" (show_arg x)
  | ADC8 x -> Printf.sprintf "ADC A, %s" (show_arg x)
  | SUB x -> Printf.sprintf "SUB %s" (show_arg x)
  | SBC8 x -> Printf.sprintf "SBC A, %s" (show_arg x)
  | AND x -> Printf.sprintf "AND %s" (show_arg x)
  | OR x -> Printf.sprintf "OR %s" (show_arg x)
  | XOR x -> Printf.sprintf "XOR %s" (show_arg x)
  | CP x -> Printf.sprintf "CP %s" (show_arg x)
  | NEG -> "NEG"
  | DAA -> "DAA"
  | CPL -> "CPL"
  (* --- 16-bit arithmetic --- *)
  | ADD16 (x, y) -> Printf.sprintf "ADD %s, %s" (show_arg x) (show_arg y)
  | ADC16 (x, y) -> Printf.sprintf "ADC %s, %s" (show_arg x) (show_arg y)
  | SBC16 (x, y) -> Printf.sprintf "SBC %s, %s" (show_arg x) (show_arg y)
  (* --- Increment / decrement --- *)
  | INC8 x -> Printf.sprintf "INC %s" (show_arg x)
  | DEC8 x -> Printf.sprintf "DEC %s" (show_arg x)
  | INC16 x -> Printf.sprintf "INC %s" (show_arg x)
  | DEC16 x -> Printf.sprintf "DEC %s" (show_arg x)
  (* --- Loads --- *)
  | LD8 (x, y) -> Printf.sprintf "LD %s, %s" (show_arg x) (show_arg y)
  | LD16 (x, y) -> Printf.sprintf "LD %s, %s" (show_arg x) (show_arg y)
  | LD_A_I -> "LD A, I"
  | LD_I_A -> "LD I, A"
  | LD_A_R -> "LD A, R"
  | LD_R_A -> "LD R, A"
  (* --- Exchange --- *)
  | EX_DE_HL -> "EX DE, HL"
  | EX_AF_AF -> "EX AF, AF'"
  | EX_SP_indirect rr -> Printf.sprintf "EX (SP), %s" (Registers.show_rr rr)
  | EXX -> "EXX"
  (* --- Block transfer and search --- *)
  | LDI -> "LDI"
  | LDIR -> "LDIR"
  | LDD -> "LDD"
  | LDDR -> "LDDR"
  | CPI -> "CPI"
  | CPIR -> "CPIR"
  | CPD -> "CPD"
  | CPDR -> "CPDR"
  (* --- Port I/O --- *)
  | IN (r, p) ->
    Printf.sprintf "IN %s, %s" (Registers.show_r r) (show_port p)
  | OUT (p, r) ->
    Printf.sprintf "OUT %s, %s" (show_port p) (Registers.show_r r)
  (* --- Block I/O --- *)
  | INI -> "INI"
  | INIR -> "INIR"
  | IND -> "IND"
  | INDR -> "INDR"
  | OUTI -> "OUTI"
  | OTIR -> "OTIR"
  | OUTD -> "OUTD"
  | OTDR -> "OTDR"
  (* --- Rotates and shifts --- *)
  | RLCA -> "RLCA"
  | RLA -> "RLA"
  | RRCA -> "RRCA"
  | RRA -> "RRA"
  | RLC x -> Printf.sprintf "RLC %s" (show_arg x)
  | RL x -> Printf.sprintf "RL %s" (show_arg x)
  | RRC x -> Printf.sprintf "RRC %s" (show_arg x)
  | RR_rot x -> Printf.sprintf "RR %s" (show_arg x)
  | SLA x -> Printf.sprintf "SLA %s" (show_arg x)
  | SRA x -> Printf.sprintf "SRA %s" (show_arg x)
  | SLL x -> Printf.sprintf "SLL %s" (show_arg x)
  | SRL x -> Printf.sprintf "SRL %s" (show_arg x)
  | RLD -> "RLD"
  | RRD -> "RRD"
  (* --- Bit manipulation --- *)
  | BIT (n, x) -> Printf.sprintf "BIT %d, %s" n (show_arg x)
  | SET (n, x) -> Printf.sprintf "SET %d, %s" n (show_arg x)
  | RES (n, x) -> Printf.sprintf "RES %d, %s" n (show_arg x)
  (* --- Stack --- *)
  | PUSH rr -> Printf.sprintf "PUSH %s" (Registers.show_rr rr)
  | POP rr -> Printf.sprintf "POP %s" (Registers.show_rr rr)
  (* --- Control flow --- *)
  | JP (c, x) ->
    Printf.sprintf "JP %s%s" (show_condition_prefix c) (show_arg x)
  | JR (c, e) ->
    Printf.sprintf "JR %s%s" (show_condition_prefix c) (Int8.show e)
  | DJNZ e -> Printf.sprintf "DJNZ %s" (Int8.show e)
  | CALL (c, nn) ->
    Printf.sprintf "CALL %s%s" (show_condition_prefix c) (Uint16.show nn)
  | RET None -> "RET"
  | RET c -> Printf.sprintf "RET %s" (show_condition c)
  | RETI -> "RETI"
  | RETN -> "RETN"
  | RST x -> Printf.sprintf "RST %s" (Uint16.show x)
  (* --- CPU control --- *)
  | NOP -> "NOP"
  | HALT -> "HALT"
  | DI -> "DI"
  | EI -> "EI"
  | IM n -> Printf.sprintf "IM %d" n
  | CCF -> "CCF"
  | SCF -> "SCF"
;;
