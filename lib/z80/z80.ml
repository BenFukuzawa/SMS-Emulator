(* make the cpu itself a functor *)
open Uints

module Make (Bus : Word_addressable_intf.S) = struct
  module Fetch_and_decode = Fetch_and_decode.Make (Bus)

  type t =
    { registers : Registers.t
    ; bus : Bus.t
    ; mutable pc : uint16
    ; mutable halted : bool
    ; mutable iff1 : bool
    ; mutable iff2 : bool
    ; mutable ei_pending : bool
    ; mutable irq_line : bool      (* VDP holds this; level-triggered *)
    ; mutable nmi_pending : bool   (* pause button; edge-triggered *)
    ; mutable im : int             (* 0, 1 or 2 — interruptor mode, set by IM n *)
    ; mutable i : uint8            (* vector base for IM 2 *)
    ; mutable refresh : uint8      (* R register *)
    ; mutable prev_inst : Instruction.t
    
    }

let create ~bus ~registers =
  { registers
  ; bus
  ; pc = Uint16.zero
  ; halted = false
  ; iff1 = false
  ; iff2 = false
  ; ei_pending = false
  ; irq_line = false (*the vdp sets this*)
  ; nmi_pending = false
  ; im = 0
  ; i = Uint8.zero
  ; refresh = Uint8.zero
  ; prev_inst = NOP
  }
;;

  type next_pc =
    | Next
    | Jump of uint16

  let execute (t : t) (inst_info : Inst_info.t) : int =
    let open Inst_info in

    let { len = _; tcycles; inst } = inst_info in
    let { taken; not_taken } = tcycles in
     
    let set_flags = Registers.set_flags t.registers in
    let read : type a. a Instruction.arg -> a =
      fun arg ->
      match arg with
      | Immediate8 n -> n
      | Immediate16 n -> n
      | Direct8 addr -> Bus.read_byte t.bus addr
      | Direct16 addr -> Bus.read_word t.bus addr
      | R r -> Registers.read_r t.registers r
      | RR_indirect rr ->
        let addr = Registers.read_rr t.registers rr in
        Bus.read_byte t.bus addr
      | IX_indirect offset ->
        let base = Registers.read_rr t.registers IX in
        let addr =
          Uint16.of_int (Uint16.to_int base + Int8.to_int offset)
        in
        Bus.read_byte t.bus addr
      | IY_indirect offset ->
        let base = Registers.read_rr t.registers IY in
        let addr =
          Uint16.of_int (Uint16.to_int base + Int8.to_int offset)

    in
    let write : type a. a Instruction.arg -> a -> unit =
      fun arg value ->
        match arg with 
        | Direct8 addr -> Bus.write_byte t.bus ~addr ~data:value
        | Direct16 addr -> Bus.write_word t.bus ~addr ~data:value
        | R r -> Registers.write_r t.registers r value
        | RR rr -> Registers.write_rr t.registers rr value
        | RR_indirect rr ->
          let addr = Registers.read_rr t.registers rr in
          Bus.write_byte t.bus ~addr ~data:value
        | IX_indirect offset ->
          let base = Registers.read_rr t.registers IX in
          let addr =
            Uint16.of_int (Uint16.to_int base + Int8.to_int offset)
          in
          Bus.write_byte t.bus ~addr ~data:value
        | IY_indirect offset ->
          let base = Registers.read_rr t.registers IY in
          let addr =
            Uint16.of_int (Uint16.to_int base + Int8.to_int offset) in
          Bus.write_byte t.bus ~addr ~data:value
        | Immediate8 _ | Immediate16 _ -> failwith "Cannot write to an immediate value"
    in
    let ( <-- ) x y = write x y in
    let check_condition t : Instruction.condition -> bool = function
      | None -> true
      | Z -> Registers.read_flag t.registers Zero
      | NZ -> not (Registers.read_flag t.registers Zero)
      | C -> Registers.read_flag t.registers Carry
      | NC -> not (Registers.read_flag t.registers Carry)
    in
    let next_pc =
      match inst with 
        | ADD8 ->
        | ADC8 ->
        | SUB ->
        | SBC8->
        | AND ->
        | OR ->
        | XOR ->
        | CP ->
        | NEG ->
        | DAA ->
        | CPL ->
        | ADD16 ->
        | ADC16 ->
        | SBC16 ->
        | INC8 ->
        | DEC8 ->
        | INC16 ->
        | DEC16 ->
        | LD8 ->
        | LD16 ->
        | LD_A_I ->
        | LD_I_A ->
        | LD_A_R ->
        | LD_R_A ->
        | EX_DE_HL ->
        | EX_AF_AF ->
        | EX_SP_indirect ->
        | EXX ->
        | LDI ->
        | LDIR ->
        | LDD ->
        | LDDR ->
        | CPI ->
        | CPIR ->
        | CPD ->
        | CPDR ->
        (* read from port *)
        | IN (dst, port) ->
          let p = match port with
            | Port_n n -> n
            | Port_C -> Registers.read_r t.registers Registers.C
          in
          let value = Bus.read_port t.bus ~port:p in
  ...
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
        | SET of int * uint8 arg * uint8 arg option
        | RES of int * uint8 arg * uint8 arg option
        | PUSH of Registers.rr
        | POP of Registers.rr
        | DJNZ of int8
        | JP of condition option * uint16 arg
        | JP_indirect of Registers.rr
        | JR of condition option * int8
        | CALL of condition option * uint16
        | RET of condition option
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
      
    match next_pc with
    | Next -> not_branched_mcycles
    | Jump addr ->
      t.pc <- addr;
      branched_mcycles
  ;;

  let set_irq_line t line = t.irq_line <- line
  let request_nmi t = t.nmi_pending <- true

  let run_instruction t =
    let was_ei_pending = t.ei_pending in
    t.ei_pending <- false;
    if t.nmi_pending
    then accept_nmi t
    else if t.irq_line && t.iff1 && not was_ei_pending
    then accept_irq t
    else if t.halted
    then (
      bump_refresh t ~opcode:0x00;
      4)
    else (
      let inst_info = Fetch_and_decode.f t.bus ~pc:t.pc in
      t.pc <- Uint16.(t.pc + inst_info.len);
      execute t inst_info)
  ;;

  let show t =
    Printf.sprintf
      "%s SP:%s PC:%s"
      (Registers.show t.registers)
      (t.sp |> Uint16.show)
      (t.pc |> Uint16.show)
  ;;

  module For_tests = struct
    let execute = execute
    let prev_inst t = t.prev_inst
  end
end
