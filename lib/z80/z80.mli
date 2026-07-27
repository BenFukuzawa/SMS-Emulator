open Uints

module Make (Bus : Word_addressable_intf.S) : sig
  type t

  val create
    :  bus:Bus.t
    -> registers:Registers.t
    -> sp:uint16
    -> pc:uint16
    -> halted:bool
    -> ime:bool
    -> t

  (** Executes a single instruction. ** Returns machine cycle (mcycle) count
      consumed during the execution. *)
  val run_instruction : t -> int

  val show : t -> string

  module For_tests : sig
    val execute : t -> Inst_info.t -> int
    val prev_inst : t -> Instruction.t
  end

  val set_irq_line : t -> bool -> unit (* VDP: "I'm tapping" / "I stopped" *)
  val request_nmi : t -> unit (* pause button: "I tapped" *)
end
