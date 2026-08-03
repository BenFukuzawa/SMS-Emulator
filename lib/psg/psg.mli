(** SN76489 programmable sound generator: the Master System's sound.

    Three square-wave tone channels and one noise channel, four bits of
    volume each. Write-only from the CPU's side -- there is no register a
    program can read back -- so every byte a ROM sends arrives through
    [write] and nothing ever goes the other way.

    The chip shares the Z80's crystal and divides it by 16, so it is clocked
    the same way the VDP is: hand [step] the T-states each instruction cost.
    Samples accumulate as it runs and the host drains them with [take]. *)

type t

(** [sample_rate] is what [take] will produce, in Hz. Default 44100. *)
val create : ?sample_rate:int -> unit -> t

(** Any write to ports $40-$7F. Bit 7 picks between a latch/data byte, which
    selects a channel and carries four bits, and a bare data byte, which
    carries six more into whatever was latched last. *)
include Psg_intf.S with type t := t

(** Advance by the T-states an instruction consumed. Generates samples. *)
val step : t -> cycles:int -> unit

val sample_rate : t -> int

(** How many samples are waiting. Roughly [sample_rate / 60] per frame. *)
val pending : t -> int

(** Take everything generated since the last call, in [-1.0, 1.0], and empty
    the queue. *)
val take : t -> float array

(** Throw away what is queued. For a host that has fallen behind and would
    rather skip than play stale audio. *)
val drop : t -> unit

module For_tests : sig
  val tone_register : t -> int -> int
  val volume : t -> int -> int
  val noise_register : t -> int
  val lfsr : t -> int

  (** The channel a bare data byte would go to, and whether it would be
      treated as a volume. *)
  val latched : t -> int * bool

  val noise_period : t -> int
  val volume_table : int array
end
