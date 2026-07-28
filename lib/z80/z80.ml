(* Z80 execution core for the Sega Master System.

   Flag layout (matches Registers.flag): bit 7 S, 6 Z, 5 Y, 4 H, 3 X, 2 P/V,
   1 N, 0 C

   X and Y are the two undocumented bits. Almost every arithmetic and logic
   operation copies them out of bits 3 and 5 of the result; the exceptions
   (CP, SCF/CCF, the block instructions, BIT n,(HL)) are noted at each site.
   ZEXALL and SingleStepTests both check them, so they are set everywhere
   rather than left alone. *)

open Uints

module Make (Mem_bus : Word_addressable_intf.S) (Io_bus : Port_io_intf.S) =
struct
  module Fetch_and_decode = Fetch_and_decode.Make (Mem_bus)

  type t =
    { registers : Registers.t
    ; bus : Mem_bus.t
    ; io : Io_bus.t
    ; mutable pc : uint16
    ; mutable i : uint8 (* interrupt vector base *)
    ; mutable refresh : uint8 (* R, bit 7 is not incremented *)
    ; mutable iff1 : bool
    ; mutable iff2 : bool
    ; mutable im : int (* 0, 1 or 2 *)
    ; mutable halted : bool
    ; mutable ei_pending : bool (* EI defers acceptance by one instr *)
    ; mutable irq_line : bool (* level triggered, held by the VDP *)
    ; mutable nmi_pending : bool (* edge triggered, pause button *)
    ; mutable prev_inst : Instruction.t (* debugging only *)
    }

  let create ~bus ~io ~registers =
    { registers
    ; bus
    ; io
    ; pc = Uint16.zero
    ; i = Uint8.zero
    ; refresh = Uint8.zero
    ; iff1 = false
    ; iff2 = false
    ; im = 0
    ; halted = false
    ; ei_pending = false
    ; irq_line = false
    ; nmi_pending = false
    ; prev_inst = NOP
    }
  ;;

  (* ------------------------------------------------------------------ *)
  (* Small helpers *)
  (* ------------------------------------------------------------------ *)

  let set_flags t = Registers.set_flags t.registers
  let flag t f = Registers.read_flag t.registers f
  let read_a t = Registers.read_r t.registers Registers.A
  let write_a t v = Registers.write_r t.registers Registers.A v
  let read_rr t rr = Registers.read_rr t.registers rr
  let write_rr t rr v = Registers.write_rr t.registers rr v
  let sp t = read_rr t Registers.SP
  let set_sp t v = write_rr t Registers.SP v

  let parity_even n =
    let n = n land 0xFF in
    let n = n lxor (n lsr 4) in
    let n = n lxor (n lsr 2) in
    let n = n lxor (n lsr 1) in
    n land 1 = 0
  ;;

  let indexed t rr d =
    let base = read_rr t rr |> Uint16.to_int in
    Uint16.of_int ((base + Int8.to_int d) land 0xFFFF)
  ;;

  (* Address an instruction operand denotes, when it denotes memory. Needed
     because the undocumented DDCB forms read, modify and write the same
     address twice. *)
  let addr_of : type a. t -> a Instruction.arg -> uint16 option =
    fun t arg ->
    match arg with
    | Direct8 addr -> Some addr
    | Direct16 addr -> Some addr
    | RR_indirect rr -> Some (read_rr t rr)
    | IX_indirect d -> Some (indexed t Registers.IX d)
    | IY_indirect d -> Some (indexed t Registers.IY d)
    | Immediate8 _ | Immediate16 _ | R _ | RR _ -> None
  ;;

  let read : type a. t -> a Instruction.arg -> a =
    fun t arg ->
    match arg with
    | Immediate8 n -> n
    | Immediate16 n -> n
    | Direct8 addr -> Mem_bus.read_byte t.bus addr
    | Direct16 addr -> Mem_bus.read_word t.bus addr
    | R r -> Registers.read_r t.registers r
    | RR rr -> read_rr t rr
    | RR_indirect rr -> Mem_bus.read_byte t.bus (read_rr t rr)
    | IX_indirect d -> Mem_bus.read_byte t.bus (indexed t Registers.IX d)
    | IY_indirect d -> Mem_bus.read_byte t.bus (indexed t Registers.IY d)
  ;;

  let write : type a. t -> a Instruction.arg -> a -> unit =
    fun t arg v ->
    match arg with
    | R r -> Registers.write_r t.registers r v
    | RR rr -> write_rr t rr v
    | Direct8 addr -> Mem_bus.write_byte t.bus ~addr ~data:v
    | Direct16 addr -> Mem_bus.write_word t.bus ~addr ~data:v
    | RR_indirect rr -> Mem_bus.write_byte t.bus ~addr:(read_rr t rr) ~data:v
    | IX_indirect d ->
      Mem_bus.write_byte t.bus ~addr:(indexed t Registers.IX d) ~data:v
    | IY_indirect d ->
      Mem_bus.write_byte t.bus ~addr:(indexed t Registers.IY d) ~data:v
    | Immediate8 _ | Immediate16 _ ->
      failwith "Z80.write: immediate is not an lvalue"
  ;;

  let push t value =
    let sp' = Uint16.sub (sp t) (Uint16.of_int 2) in
    set_sp t sp';
    Mem_bus.write_word t.bus ~addr:sp' ~data:value
  ;;

  let pop t =
    let sp' = sp t in
    let value = Mem_bus.read_word t.bus sp' in
    set_sp t (Uint16.add sp' (Uint16.of_int 2));
    value
  ;;

  (* Only the low 8 bits select a port. B does reach the device on A8-A15,
     but nothing on the SMS decodes those lines. *)
  let port_of t (p : Instruction.port) =
    match p with
    | Port_n n -> n
    | Port_C -> Registers.read_r t.registers Registers.C
  ;;

  let check_condition t : Instruction.condition option -> bool = function
    | None -> true
    | Some C -> flag t Registers.Carry
    | Some NC -> not (flag t Registers.Carry)
    | Some Z -> flag t Registers.Zero
    | Some NZ -> not (flag t Registers.Zero)
    | Some M -> flag t Registers.Sign
    | Some P -> not (flag t Registers.Sign)
    | Some PE -> flag t Registers.Parity_overflow
    | Some PO -> not (flag t Registers.Parity_overflow)
  ;;

  (* ------------------------------------------------------------------ *)
  (* ALU primitives *)
  (* ------------------------------------------------------------------ *)

  let add8 t ~carry a b =
    let a = Uint8.to_int a
    and b = Uint8.to_int b in
    let c = if carry then 1 else 0 in
    let sum = a + b + c in
    let res = sum land 0xFF in
    set_flags
      t
      ~s:(res land 0x80 <> 0)
      ~z:(res = 0)
      ~y:(res land 0x20 <> 0)
      ~h:((a land 0xF) + (b land 0xF) + c > 0xF)
      ~x:(res land 0x08 <> 0)
      ~p:(a lxor res land (b lxor res) land 0x80 <> 0)
      ~n:false
      ~c:(sum > 0xFF)
      ();
    Uint8.of_int res
  ;;

  let sub8 t ~carry a b =
    let a = Uint8.to_int a
    and b = Uint8.to_int b in
    let c = if carry then 1 else 0 in
    let diff = a - b - c in
    let res = diff land 0xFF in
    set_flags
      t
      ~s:(res land 0x80 <> 0)
      ~z:(res = 0)
      ~y:(res land 0x20 <> 0)
      ~h:((a land 0xF) - (b land 0xF) - c < 0)
      ~x:(res land 0x08 <> 0)
      ~p:(a lxor b land (a lxor res) land 0x80 <> 0)
      ~n:true
      ~c:(diff < 0)
      ();
    Uint8.of_int res
  ;;

  (* CP is SUB with the result thrown away, except that X and Y come from
     the *operand* rather than from the result. *)
  let cp8 t a b =
    let a' = Uint8.to_int a
    and b' = Uint8.to_int b in
    let diff = a' - b' in
    let res = diff land 0xFF in
    set_flags
      t
      ~s:(res land 0x80 <> 0)
      ~z:(res = 0)
      ~y:(b' land 0x20 <> 0)
      ~h:((a' land 0xF) - (b' land 0xF) < 0)
      ~x:(b' land 0x08 <> 0)
      ~p:(a' lxor b' land (a' lxor res) land 0x80 <> 0)
      ~n:true
      ~c:(diff < 0)
      ()
  ;;

  let logic_flags t ~half res =
    let r = Uint8.to_int res in
    set_flags
      t
      ~s:(r land 0x80 <> 0)
      ~z:(r = 0)
      ~y:(r land 0x20 <> 0)
      ~h:half
      ~x:(r land 0x08 <> 0)
      ~p:(parity_even r)
      ~n:false
      ~c:false
      ()
  ;;

  (* INC and DEC leave C alone, which is why they cannot reuse add8. *)
  let inc8 t v =
    let v' = Uint8.to_int v in
    let res = (v' + 1) land 0xFF in
    set_flags
      t
      ~s:(res land 0x80 <> 0)
      ~z:(res = 0)
      ~y:(res land 0x20 <> 0)
      ~h:(v' land 0xF = 0xF)
      ~x:(res land 0x08 <> 0)
      ~p:(v' = 0x7F)
      ~n:false
      ();
    Uint8.of_int res
  ;;

  let dec8 t v =
    let v' = Uint8.to_int v in
    let res = (v' - 1) land 0xFF in
    set_flags
      t
      ~s:(res land 0x80 <> 0)
      ~z:(res = 0)
      ~y:(res land 0x20 <> 0)
      ~h:(v' land 0xF = 0)
      ~x:(res land 0x08 <> 0)
      ~p:(v' = 0x80)
      ~n:true
      ();
    Uint8.of_int res
  ;;

  (* ADD HL,rr touches only H, N, C, X and Y. S, Z and P/V survive. *)
  let add16 t a b =
    let a' = Uint16.to_int a
    and b' = Uint16.to_int b in
    let sum = a' + b' in
    let res = sum land 0xFFFF in
    set_flags
      t
      ~y:(res land 0x2000 <> 0)
      ~h:((a' land 0x0FFF) + (b' land 0x0FFF) > 0x0FFF)
      ~x:(res land 0x0800 <> 0)
      ~n:false
      ~c:(sum > 0xFFFF)
      ();
    Uint16.of_int res
  ;;

  let adc16 t a b =
    let a' = Uint16.to_int a
    and b' = Uint16.to_int b in
    let c = if flag t Registers.Carry then 1 else 0 in
    let sum = a' + b' + c in
    let res = sum land 0xFFFF in
    set_flags
      t
      ~s:(res land 0x8000 <> 0)
      ~z:(res = 0)
      ~y:(res land 0x2000 <> 0)
      ~h:((a' land 0x0FFF) + (b' land 0x0FFF) + c > 0x0FFF)
      ~x:(res land 0x0800 <> 0)
      ~p:(a' lxor res land (b' lxor res) land 0x8000 <> 0)
      ~n:false
      ~c:(sum > 0xFFFF)
      ();
    Uint16.of_int res
  ;;

  let sbc16 t a b =
    let a' = Uint16.to_int a
    and b' = Uint16.to_int b in
    let c = if flag t Registers.Carry then 1 else 0 in
    let diff = a' - b' - c in
    let res = diff land 0xFFFF in
    set_flags
      t
      ~s:(res land 0x8000 <> 0)
      ~z:(res = 0)
      ~y:(res land 0x2000 <> 0)
      ~h:((a' land 0x0FFF) - (b' land 0x0FFF) - c < 0)
      ~x:(res land 0x0800 <> 0)
      ~p:(a' lxor b' land (a' lxor res) land 0x8000 <> 0)
      ~n:true
      ~c:(diff < 0)
      ();
    Uint16.of_int res
  ;;

  (* ------------------------------------------------------------------ *)
  (* Rotates and shifts *)
  (* ------------------------------------------------------------------ *)

  (* Each returns (result, carry_out). Flag setting is done by the caller,
     because the accumulator forms (RLCA, RLA, RRCA, RRA) set a different
     subset of flags than the CB-prefixed forms. *)

  let rlc v =
    let v = Uint8.to_int v in
    let c = v land 0x80 <> 0 in
    Uint8.of_int ((v lsl 1) lor (if c then 1 else 0) land 0xFF), c
  ;;

  let rrc v =
    let v = Uint8.to_int v in
    let c = v land 0x01 <> 0 in
    Uint8.of_int ((v lsr 1) lor (if c then 0x80 else 0) land 0xFF), c
  ;;

  let rl t v =
    let v = Uint8.to_int v in
    let old_c = if flag t Registers.Carry then 1 else 0 in
    let c = v land 0x80 <> 0 in
    Uint8.of_int ((v lsl 1) lor old_c land 0xFF), c
  ;;

  let rr t v =
    let v = Uint8.to_int v in
    let old_c = if flag t Registers.Carry then 0x80 else 0 in
    let c = v land 0x01 <> 0 in
    Uint8.of_int ((v lsr 1) lor old_c land 0xFF), c
  ;;

  let sla v =
    let v = Uint8.to_int v in
    Uint8.of_int ((v lsl 1) land 0xFF), v land 0x80 <> 0
  ;;

  let sra v =
    let v = Uint8.to_int v in
    Uint8.of_int ((v lsr 1) lor (v land 0x80) land 0xFF), v land 1 <> 0
  ;;

  (* SLL is undocumented: shift left, feeding a 1 into bit 0. *)
  let sll v =
    let v = Uint8.to_int v in
    Uint8.of_int ((v lsl 1) lor 1 land 0xFF), v land 0x80 <> 0
  ;;

  let srl v =
    let v = Uint8.to_int v in
    Uint8.of_int ((v lsr 1) land 0xFF), v land 1 <> 0
  ;;

  let rot_flags t res c =
    let r = Uint8.to_int res in
    set_flags
      t
      ~s:(r land 0x80 <> 0)
      ~z:(r = 0)
      ~y:(r land 0x20 <> 0)
      ~h:false
      ~x:(r land 0x08 <> 0)
      ~p:(parity_even r)
      ~n:false
      ~c
      ()
  ;;

  (* The accumulator rotates preserve S, Z and P/V. *)
  let acc_rot_flags t res c =
    let r = Uint8.to_int res in
    set_flags
      t
      ~y:(r land 0x20 <> 0)
      ~h:false
      ~x:(r land 0x08 <> 0)
      ~n:false
      ~c
      ()
  ;;

  (* ------------------------------------------------------------------ *)
  (* Block instructions *)
  (* ------------------------------------------------------------------ *)

  let ld_block t ~delta =
    let hl = read_rr t Registers.HL in
    let de = read_rr t Registers.DE in
    let value = Mem_bus.read_byte t.bus hl in
    Mem_bus.write_byte t.bus ~addr:de ~data:value;
    write_rr t Registers.HL (Uint16.of_int (Uint16.to_int hl + delta));
    write_rr t Registers.DE (Uint16.of_int (Uint16.to_int de + delta));
    let bc = Uint16.sub (read_rr t Registers.BC) Uint16.one in
    write_rr t Registers.BC bc;
    (* X and Y come from A + the transferred byte, not from a result. *)
    let n = (Uint8.to_int (read_a t) + Uint8.to_int value) land 0xFF in
    set_flags
      t
      ~y:(n land 0x02 <> 0)
      ~h:false
      ~x:(n land 0x08 <> 0)
      ~p:(Uint16.to_int bc <> 0)
      ~n:false
      ();
    Uint16.to_int bc <> 0
  ;;

  let cp_block t ~delta =
    let hl = read_rr t Registers.HL in
    let a = Uint8.to_int (read_a t) in
    let value = Uint8.to_int (Mem_bus.read_byte t.bus hl) in
    let diff = (a - value) land 0xFF in
    let half = (a land 0xF) - (value land 0xF) < 0 in
    write_rr t Registers.HL (Uint16.of_int (Uint16.to_int hl + delta));
    let bc = Uint16.sub (read_rr t Registers.BC) Uint16.one in
    write_rr t Registers.BC bc;
    let n = (diff - if half then 1 else 0) land 0xFF in
    set_flags
      t
      ~s:(diff land 0x80 <> 0)
      ~z:(diff = 0)
      ~y:(n land 0x02 <> 0)
      ~h:half
      ~x:(n land 0x08 <> 0)
      ~p:(Uint16.to_int bc <> 0)
      ~n:true
      ();
    Uint16.to_int bc <> 0 && diff <> 0
  ;;

  let in_block t ~delta =
    let port = Registers.read_r t.registers Registers.C in
    let hl = read_rr t Registers.HL in
    let value = Io_bus.read_port t.io ~port in
    Mem_bus.write_byte t.bus ~addr:hl ~data:value;
    let b = dec8 t (Registers.read_r t.registers Registers.B) in
    Registers.write_r t.registers Registers.B b;
    write_rr t Registers.HL (Uint16.of_int (Uint16.to_int hl + delta));
    let c = Uint8.to_int port in
    let k = Uint8.to_int value + ((c + delta) land 0xFF) in
    set_flags
      t
      ~h:(k > 0xFF)
      ~p:(parity_even (k land 0x07 lxor Uint8.to_int b))
      ~n:(Uint8.to_int value land 0x80 <> 0)
      ~c:(k > 0xFF)
      ();
    Uint8.to_int b <> 0
  ;;

  let out_block t ~delta =
    let hl = read_rr t Registers.HL in
    let value = Mem_bus.read_byte t.bus hl in
    let b = dec8 t (Registers.read_r t.registers Registers.B) in
    Registers.write_r t.registers Registers.B b;
    let port = Registers.read_r t.registers Registers.C in
    Io_bus.write_port t.io ~port ~data:value;
    write_rr t Registers.HL (Uint16.of_int (Uint16.to_int hl + delta));
    let l = Uint16.to_int (read_rr t Registers.HL) land 0xFF in
    let k = Uint8.to_int value + l in
    set_flags
      t
      ~h:(k > 0xFF)
      ~p:(parity_even (k land 0x07 lxor Uint8.to_int b))
      ~n:(Uint8.to_int value land 0x80 <> 0)
      ~c:(k > 0xFF)
      ();
    Uint8.to_int b <> 0
  ;;

  (* ------------------------------------------------------------------ *)
  (* Execute *)
  (* ------------------------------------------------------------------ *)

  type control =
    | Next (* advance PC by the instruction length *)
    | Jump of uint16
    | Repeat (* leave PC alone so the instruction re-runs *)

  let execute (t : t) (inst_info : Inst_info.t) : int =
    let open Inst_info in
    let { len; tcycles; inst } = inst_info in
    let taken = ref true in
    let rot_with t op arg dest =
      (* [dest] is the undocumented DDCB/FDCB second operand: the result is
         written both to memory and to the named register. *)
      let value = read t arg in
      let res, c =
        match op with
        | `Rlc -> rlc value
        | `Rrc -> rrc value
        | `Rl -> rl t value
        | `Rr -> rr t value
        | `Sla -> sla value
        | `Sra -> sra value
        | `Sll -> sll value
        | `Srl -> srl value
      in
      write t arg res;
      Option.iter (fun d -> write t d res) dest;
      rot_flags t res c
    in
    let control =
      match inst with
      (* --- 8-bit load --- *)
      | LD8 (dst, src) ->
        write t dst (read t src);
        Next
      | LD16 (dst, src) ->
        write t dst (read t src);
        Next
      | LD_A_I ->
        let v = t.i in
        write_a t v;
        set_flags
          t
          ~s:(Uint8.to_int v land 0x80 <> 0)
          ~z:(Uint8.to_int v = 0)
          ~y:(Uint8.to_int v land 0x20 <> 0)
          ~h:false
          ~x:(Uint8.to_int v land 0x08 <> 0)
          ~p:t.iff2
          ~n:false
          ();
        Next
      | LD_A_R ->
        let v = t.refresh in
        write_a t v;
        set_flags
          t
          ~s:(Uint8.to_int v land 0x80 <> 0)
          ~z:(Uint8.to_int v = 0)
          ~y:(Uint8.to_int v land 0x20 <> 0)
          ~h:false
          ~x:(Uint8.to_int v land 0x08 <> 0)
          ~p:t.iff2
          ~n:false
          ();
        Next
      | LD_I_A ->
        t.i <- read_a t;
        Next
      | LD_R_A ->
        t.refresh <- read_a t;
        Next
      (* --- 8-bit arithmetic and logic --- *)
      | ADD8 x ->
        write_a t (add8 t ~carry:false (read_a t) (read t x));
        Next
      | ADC8 x ->
        write_a
          t
          (add8 t ~carry:(flag t Registers.Carry) (read_a t) (read t x));
        Next
      | SUB x ->
        write_a t (sub8 t ~carry:false (read_a t) (read t x));
        Next
      | SBC8 x ->
        write_a
          t
          (sub8 t ~carry:(flag t Registers.Carry) (read_a t) (read t x));
        Next
      | AND x ->
        let res = Uint8.logand (read_a t) (read t x) in
        write_a t res;
        logic_flags t ~half:true res;
        Next
      | OR x ->
        let res = Uint8.logor (read_a t) (read t x) in
        write_a t res;
        logic_flags t ~half:false res;
        Next
      | XOR x ->
        let res = Uint8.logxor (read_a t) (read t x) in
        write_a t res;
        logic_flags t ~half:false res;
        Next
      | CP x ->
        cp8 t (read_a t) (read t x);
        Next
      | NEG ->
        write_a t (sub8 t ~carry:false Uint8.zero (read_a t));
        Next
      | CPL ->
        let res = Uint8.logxor (read_a t) (Uint8.of_int 0xFF) in
        write_a t res;
        let r = Uint8.to_int res in
        set_flags
          t
          ~y:(r land 0x20 <> 0)
          ~h:true
          ~x:(r land 0x08 <> 0)
          ~n:true
          ();
        Next
      | DAA ->
        let a = Uint8.to_int (read_a t) in
        let n = flag t Registers.Subtraction in
        let h = flag t Registers.Half_carry in
        let c = flag t Registers.Carry in
        let adjust =
          (if h || ((not n) && a land 0xF > 9) then 0x06 else 0)
          lor if c || a > 0x99 then 0x60 else 0
        in
        let res = (if n then a - adjust else a + adjust) land 0xFF in
        write_a t (Uint8.of_int res);
        set_flags
          t
          ~s:(res land 0x80 <> 0)
          ~z:(res = 0)
          ~y:(res land 0x20 <> 0)
          ~h:(if n then h && a land 0xF < 6 else a land 0xF > 9)
          ~x:(res land 0x08 <> 0)
          ~p:(parity_even res)
          ~c:(c || a > 0x99)
          ();
        Next
      (* --- 16-bit arithmetic --- *)
      | ADD16 (dst, src) ->
        write t dst (add16 t (read t dst) (read t src));
        Next
      | ADC16 (dst, src) ->
        write t dst (adc16 t (read t dst) (read t src));
        Next
      | SBC16 (dst, src) ->
        write t dst (sbc16 t (read t dst) (read t src));
        Next
      | INC8 x ->
        write t x (inc8 t (read t x));
        Next
      | DEC8 x ->
        write t x (dec8 t (read t x));
        Next
      | INC16 x ->
        write t x (Uint16.succ (read t x));
        Next
      | DEC16 x ->
        write t x (Uint16.pred (read t x));
        Next
      (* --- Exchange --- *)
      | EX_DE_HL ->
        let de = read_rr t Registers.DE in
        write_rr t Registers.DE (read_rr t Registers.HL);
        write_rr t Registers.HL de;
        Next
      | EX_AF_AF ->
        Registers.ex_af t.registers;
        Next
      | EXX ->
        Registers.exx t.registers;
        Next
      | EX_SP_indirect rr ->
        let top = Mem_bus.read_word t.bus (sp t) in
        Mem_bus.write_word t.bus ~addr:(sp t) ~data:(read_rr t rr);
        write_rr t rr top;
        Next
      (* --- Block transfer and search --- *)
      | LDI ->
        ignore (ld_block t ~delta:1 : bool);
        Next
      | LDD ->
        ignore (ld_block t ~delta:(-1) : bool);
        Next
      | LDIR ->
        taken := ld_block t ~delta:1;
        if !taken then Repeat else Next
      | LDDR ->
        taken := ld_block t ~delta:(-1);
        if !taken then Repeat else Next
      | CPI ->
        ignore (cp_block t ~delta:1 : bool);
        Next
      | CPD ->
        ignore (cp_block t ~delta:(-1) : bool);
        Next
      | CPIR ->
        taken := cp_block t ~delta:1;
        if !taken then Repeat else Next
      | CPDR ->
        taken := cp_block t ~delta:(-1);
        if !taken then Repeat else Next
      (* --- Port I/O --- *)
      | IN (dst, port) ->
        let p = port_of t port in
        let value = Io_bus.read_port t.io ~port:p in
        (match dst, port with
         | Some r, Port_n _ ->
           (* IN A,(n) does not touch the flags. *)
           Registers.write_r t.registers r value
         | _ ->
           (* IN r,(C) and the undocumented IN (C) do set them. *)
           (match dst with
            | Some r -> Registers.write_r t.registers r value
            | None -> ());
           let v = Uint8.to_int value in
           set_flags
             t
             ~s:(v land 0x80 <> 0)
             ~z:(v = 0)
             ~y:(v land 0x20 <> 0)
             ~h:false
             ~x:(v land 0x08 <> 0)
             ~p:(parity_even v)
             ~n:false
             ());
        Next
      | OUT (port, value) ->
        let p = port_of t port in
        let data =
          match value with
          | Out_register r -> Registers.read_r t.registers r
          | Out_zero -> Uint8.zero
        in
        Io_bus.write_port t.io ~port:p ~data;
        Next
      | INI ->
        ignore (in_block t ~delta:1 : bool);
        Next
      | IND ->
        ignore (in_block t ~delta:(-1) : bool);
        Next
      | INIR ->
        taken := in_block t ~delta:1;
        if !taken then Repeat else Next
      | INDR ->
        taken := in_block t ~delta:(-1);
        if !taken then Repeat else Next
      | OUTI ->
        ignore (out_block t ~delta:1 : bool);
        Next
      | OUTD ->
        ignore (out_block t ~delta:(-1) : bool);
        Next
      | OTIR ->
        taken := out_block t ~delta:1;
        if !taken then Repeat else Next
      | OTDR ->
        taken := out_block t ~delta:(-1);
        if !taken then Repeat else Next
      (* --- Accumulator rotates --- *)
      | RLCA ->
        let res, c = rlc (read_a t) in
        write_a t res;
        acc_rot_flags t res c;
        Next
      | RRCA ->
        let res, c = rrc (read_a t) in
        write_a t res;
        acc_rot_flags t res c;
        Next
      | RLA ->
        let res, c = rl t (read_a t) in
        write_a t res;
        acc_rot_flags t res c;
        Next
      | RRA ->
        let res, c = rr t (read_a t) in
        write_a t res;
        acc_rot_flags t res c;
        Next
      (* --- CB rotates and shifts --- *)
      | RLC (x, d) ->
        rot_with t `Rlc x d;
        Next
      | RRC (x, d) ->
        rot_with t `Rrc x d;
        Next
      | RL (x, d) ->
        rot_with t `Rl x d;
        Next
      | RR_rot (x, d) ->
        rot_with t `Rr x d;
        Next
      | SLA (x, d) ->
        rot_with t `Sla x d;
        Next
      | SRA (x, d) ->
        rot_with t `Sra x d;
        Next
      | SLL (x, d) ->
        rot_with t `Sll x d;
        Next
      | SRL (x, d) ->
        rot_with t `Srl x d;
        Next
      | RLD ->
        let hl = read_rr t Registers.HL in
        let m = Uint8.to_int (Mem_bus.read_byte t.bus hl) in
        let a = Uint8.to_int (read_a t) in
        let m' = (m lsl 4) lor (a land 0x0F) land 0xFF in
        let a' = a land 0xF0 lor (m lsr 4) in
        Mem_bus.write_byte t.bus ~addr:hl ~data:(Uint8.of_int m');
        write_a t (Uint8.of_int a');
        set_flags
          t
          ~s:(a' land 0x80 <> 0)
          ~z:(a' = 0)
          ~y:(a' land 0x20 <> 0)
          ~h:false
          ~x:(a' land 0x08 <> 0)
          ~p:(parity_even a')
          ~n:false
          ();
        Next
      | RRD ->
        let hl = read_rr t Registers.HL in
        let m = Uint8.to_int (Mem_bus.read_byte t.bus hl) in
        let a = Uint8.to_int (read_a t) in
        let m' = ((a land 0x0F) lsl 4) lor (m lsr 4) in
        let a' = a land 0xF0 lor (m land 0x0F) in
        Mem_bus.write_byte t.bus ~addr:hl ~data:(Uint8.of_int m');
        write_a t (Uint8.of_int a');
        set_flags
          t
          ~s:(a' land 0x80 <> 0)
          ~z:(a' = 0)
          ~y:(a' land 0x20 <> 0)
          ~h:false
          ~x:(a' land 0x08 <> 0)
          ~p:(parity_even a')
          ~n:false
          ();
        Next
      (* --- Bit manipulation --- *)
      | BIT (n, x) ->
        let v = Uint8.to_int (read t x) in
        let is_set = v land (1 lsl n) <> 0 in
        (* For BIT n,r the undocumented bits come from the operand. For BIT
           n,(HL) the real chip supplies them from the internal WZ register;
           the address high byte is the usual stand-in. *)
        let xy =
          match addr_of t x with
          | Some addr -> Uint16.to_int addr lsr 8
          | None -> v
        in
        set_flags
          t
          ~s:(n = 7 && is_set)
          ~z:(not is_set)
          ~y:(xy land 0x20 <> 0)
          ~h:true
          ~x:(xy land 0x08 <> 0)
          ~p:(not is_set)
          ~n:false
          ();
        Next
      | SET (n, x, d) ->
        let res = Uint8.logor (read t x) (Uint8.of_int (1 lsl n)) in
        write t x res;
        Option.iter (fun dst -> write t dst res) d;
        Next
      | RES (n, x, d) ->
        let mask = Uint8.of_int (lnot (1 lsl n) land 0xFF) in
        let res = Uint8.logand (read t x) mask in
        write t x res;
        Option.iter (fun dst -> write t dst res) d;
        Next
      (* --- Stack --- *)
      | PUSH rr ->
        push t (read_rr t rr);
        Next
      | POP rr ->
        write_rr t rr (pop t);
        Next
      (* --- Control flow --- *)
      | JP (cond, target) ->
        if check_condition t cond
        then Jump (read t target)
        else (
          taken := false;
          Next)
      | JP_indirect rr ->
        (* JP (HL) loads PC from the register pair; it does not dereference,
           despite the mnemonic. *)
        Jump (read_rr t rr)
      | JR (cond, offset) ->
        if check_condition t cond
        then (
          let base = Uint16.to_int t.pc + Uint16.to_int len in
          Jump (Uint16.of_int ((base + Int8.to_int offset) land 0xFFFF)))
        else (
          taken := false;
          Next)
      | DJNZ offset ->
        let b = Uint8.pred (Registers.read_r t.registers Registers.B) in
        Registers.write_r t.registers Registers.B b;
        if Uint8.to_int b <> 0
        then (
          let base = Uint16.to_int t.pc + Uint16.to_int len in
          Jump (Uint16.of_int ((base + Int8.to_int offset) land 0xFFFF)))
        else (
          taken := false;
          Next)
      | CALL (cond, target) ->
        if check_condition t cond
        then (
          push t (Uint16.add t.pc len);
          Jump target)
        else (
          taken := false;
          Next)
      | RET cond ->
        if check_condition t cond
        then Jump (pop t)
        else (
          taken := false;
          Next)
      | RETI -> Jump (pop t)
      | RETN ->
        t.iff1 <- t.iff2;
        Jump (pop t)
      | RST target ->
        push t (Uint16.add t.pc len);
        Jump target
      (* --- CPU control --- *)
      | NOP -> Next
      | HALT ->
        t.halted <- true;
        Next
      | DI ->
        t.iff1 <- false;
        t.iff2 <- false;
        Next
      | EI ->
        t.iff1 <- true;
        t.iff2 <- true;
        t.ei_pending <- true;
        Next
      | IM n ->
        t.im <- n;
        Next
      | SCF ->
        let a = Uint8.to_int (read_a t) in
        set_flags
          t
          ~y:(a land 0x20 <> 0)
          ~h:false
          ~x:(a land 0x08 <> 0)
          ~n:false
          ~c:true
          ();
        Next
      | CCF ->
        let a = Uint8.to_int (read_a t) in
        let c = flag t Registers.Carry in
        set_flags
          t
          ~y:(a land 0x20 <> 0)
          ~h:c
          ~x:(a land 0x08 <> 0)
          ~n:false
          ~c:(not c)
          ();
        Next
    in
    t.prev_inst <- inst;
    (match control with
     | Next -> t.pc <- Uint16.add t.pc len
     | Jump addr -> t.pc <- addr
     | Repeat -> ());
    if !taken then tcycles.taken else tcycles.not_taken
  ;;

  (* ------------------------------------------------------------------ *)
  (* Interrupts and the top-level step *)
  (* ------------------------------------------------------------------ *)

  let set_irq_line t v = t.irq_line <- v
  let request_nmi t = t.nmi_pending <- true

  let accept_nmi t =
    t.nmi_pending <- false;
    t.halted <- false;
    t.iff2 <- t.iff1;
    t.iff1 <- false;
    push t t.pc;
    t.pc <- Uint16.of_int 0x66;
    11
  ;;

  let accept_irq t =
    t.halted <- false;
    t.iff1 <- false;
    t.iff2 <- false;
    match t.im with
    | 2 ->
      (* The device supplies the low byte. Nothing on the SMS drives the data
         bus during the acknowledge cycle, so it floats to $FF. *)
      let vector = Uint16.of_int ((Uint8.to_int t.i lsl 8) lor 0xFF) in
      push t t.pc;
      t.pc <- Mem_bus.read_word t.bus vector;
      19
    | _ ->
      (* IM 0 on the SMS sees $FF on the bus, which is RST 38h, so it behaves
         identically to IM 1. *)
      push t t.pc;
      t.pc <- Uint16.of_int 0x38;
      13
  ;;

  let bump_refresh t ~opcode =
    (* R advances once per M1 cycle. Prefixed opcodes have two. Bit 7 is not
       part of the counter. *)
    let steps =
      match opcode with 0xDD | 0xFD | 0xED | 0xCB -> 2 | _ -> 1
    in
    let r = Uint8.to_int t.refresh in
    t.refresh <- Uint8.of_int (r land 0x80 lor ((r + steps) land 0x7F))
  ;;

  let run_instruction t =
    let was_ei_pending = t.ei_pending in
    t.ei_pending <- false;
    if t.nmi_pending
    then accept_nmi t
    else if t.irq_line && t.iff1 && not was_ei_pending
    then accept_irq t
    else if t.halted
    then (
      (* HALT executes NOPs until something interrupts it. *)
      bump_refresh t ~opcode:0x00;
      4)
    else (
      let opcode = Mem_bus.read_byte t.bus t.pc |> Uint8.to_int in
      bump_refresh t ~opcode;
      let inst_info = Fetch_and_decode.f t.bus ~pc:t.pc in
      execute t inst_info)
  ;;

  let last_inst t = Instruction.show t.prev_inst

  let show t =
    Printf.sprintf
      "%s PC:%s IFF1:%b IM:%d %s"
      (Registers.show t.registers)
      (Uint16.show t.pc)
      t.iff1
      t.im
      (Instruction.show t.prev_inst)
  ;;

  module For_tests = struct
    let execute = execute
    let prev_inst t = t.prev_inst
    let pc t = t.pc
    let set_pc t pc = t.pc <- pc
    let registers t = t.registers
    let interrupt_state t = t.iff1, t.iff2, t.im, t.i, t.refresh, t.halted

    let set_interrupt_state t ~iff1 ~iff2 ~im ~i ~refresh ~halted =
      t.iff1 <- iff1;
      t.iff2 <- iff2;
      t.im <- im;
      t.i <- i;
      t.refresh <- refresh;
      t.halted <- halted
    ;;
  end
end
