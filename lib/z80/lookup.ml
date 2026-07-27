open Uints
open Instruction

type index_prefix =
  | IX
  | IY
  | No_prefix

let cc = ([| NZ; Z; NC; C; PO; PE; P; M |] : Instruction.condition array)

let alu =
  [| (fun a -> ADD8 a)
   ; (fun a -> ADC8 a)
   ; (fun a -> SUB a)
   ; (fun a -> SBC8 a)
   ; (fun a -> AND a)
   ; (fun a -> XOR a)
   ; (fun a -> OR a)
   ; (fun a -> CP a)
  |]
;;

let rot
  (a : uint8 Instruction.arg)
  (destination : uint8 Instruction.arg option)
  : Instruction.t array
  =
  [| RLC (a, destination)
   ; RRC (a, destination)
   ; RL (a, destination)
   ; RR_rot (a, destination)
   ; SLA (a, destination)
   ; SRA (a, destination)
   ; SLL (a, destination)
   ; SRL (a, destination)
  |]
;;

let im = [| 0; 0; 1; 2; 0; 0; 1; 2 |]

let bli =
  Instruction.
    [| [| LDI; CPI; INI; OUTI |]
     ; [| LDD; CPD; IND; OUTD |]
     ; [| LDIR; CPIR; INIR; OTIR |]
     ; [| LDDR; CPDR; INDR; OTDR |]
    |]
;;

(* subtract 4 for index in the 2d array *)
let bli_lookup y z = bli.(y - 4).(z)

let r ~prefix ~touches_mem ~(next_byte : unit -> uint8) idx
  : uint8 Instruction.arg
  =
  let open Instruction in
  let displacement () = Int8.of_byte (next_byte ()) in
  match idx, prefix with
  | 0, _ -> R Registers.B
  | 1, _ -> R Registers.C
  | 2, _ -> R Registers.D
  | 3, _ -> R Registers.E
  | 4, IX when not touches_mem -> R Registers.IXH
  | 4, IY when not touches_mem -> R Registers.IYH
  | 4, _ -> R Registers.H (* No_prefix, or prefixed-but-(HL) *)
  | 5, IX when not touches_mem -> R Registers.IXL
  | 5, IY when not touches_mem -> R Registers.IYL
  | 5, _ -> R Registers.L
  | 6, No_prefix -> RR_indirect Registers.HL
  | 6, IX -> IX_indirect (displacement ())
  | 6, IY -> IY_indirect (displacement ())
  | 7, _ -> R Registers.A
  | _ -> invalid_arg (Printf.sprintf "Lookup.r: index %d out of range" idx)
;;

let rp ~prefix idx : Registers.rr =
  match idx, prefix with
  | 0, _ -> Registers.BC
  | 1, _ -> Registers.DE
  | 2, No_prefix -> Registers.HL
  | 2, IX -> Registers.IX
  | 2, IY -> Registers.IY
  | 3, _ -> Registers.SP
  | _ -> invalid_arg (Printf.sprintf "Lookup.rp: index %d out of range" idx)
;;

let rp2 ~prefix idx : Registers.rr =
  match idx, prefix with
  | 0, _ -> Registers.BC
  | 1, _ -> Registers.DE
  | 2, No_prefix -> Registers.HL
  | 2, IX -> Registers.IX
  | 2, IY -> Registers.IY
  | 3, _ -> Registers.AF
  | _ -> invalid_arg (Printf.sprintf "Lookup.rp2: index %d out of range" idx)
;;
