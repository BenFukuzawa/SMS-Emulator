open Uints

module Make
    (Mem_bus : Word_addressable_intf.S)
    (Io_bus : Port_io_intf.S) : sig
  type t

  val create : bus:Mem_bus.t -> io:Io_bus.t -> registers:Registers.t -> t

  (** Fetches, decodes and executes one instruction, or services a pending
      interrupt. Returns T-states consumed -- the unit the VDP and PSG are
      clocked against. *)
  val run_instruction : t -> int

  (** Level-triggered maskable interrupt, held asserted by the VDP until the
      ROM reads the status port. *)
  val set_irq_line : t -> bool -> unit

  (** Edge-triggered non-maskable interrupt, raised by the pause button. *)
  val request_nmi : t -> unit

  val show : t -> string
  val last_inst : t -> string

  module For_tests : sig
    val execute : t -> Inst_info.t -> int
    val prev_inst : t -> Instruction.t
    val pc : t -> uint16
    val set_pc : t -> uint16 -> unit
    val registers : t -> Registers.t
    val q : t -> uint8
    val set_q : t -> uint8 -> unit
    val interrupt_state : t -> bool * bool * int * uint8 * uint8 * bool

    val set_interrupt_state
      :  t
      -> iff1:bool
      -> iff2:bool
      -> im:int
      -> i:uint8
      -> refresh:uint8
      -> halted:bool
      -> unit
  end
end
