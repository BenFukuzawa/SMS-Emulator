open Uints

(** Sega Master System Video Display Processor.

    The program cannot address video RAM. It reaches the chip through two I/O
    ports only: $BE for data and $BF for commands and status. Those ports are
    this interface. *)

type t

val create : unit -> t

(** Port $BF, written. Commands are two bytes; the latch between them is live
    state, so these arrive one byte at a time. *)
val write_control : t -> uint8 -> unit

(** Port $BF, read. Destructive: clears the interrupt flags and the control
    latch. This is how a program acknowledges a VDP interrupt. *)
val read_status : t -> uint8

(** Port $BE, written. Goes to VRAM or CRAM depending on the last command,
    and advances the address counter. *)
val write_data : t -> uint8 -> unit

(** Port $BE, read. Returns the byte fetched by the *previous* access: the
    chip reads ahead rather than stalling the CPU. *)
val read_data : t -> uint8

(** Advance the chip by the T-states an instruction consumed. The VDP is
    free-running; this is the only thing that moves it. *)
val step : t -> cycles:int -> unit

(** The maskable interrupt line, level triggered. Feed it to the Z80 after
    every instruction: it goes high the moment a flag and its enable bit are
    both set, and drops when the program reads the status port. *)
val irq : t -> bool

(** Port $7E. The current scanline, with the mid-frame backwards jump that
    squeezes 262 lines into a byte. *)
val v_counter : t -> uint8

(** Port $7F. Not modelled; reads back zero. *)
val h_counter : t -> uint8

(** Completed frames. The host watches this to know when to put a picture up. *)
val frame_count : t -> int

(** Direct access to state that is otherwise reachable only through the port
    protocol -- which is exactly the thing under test. *)
module For_tests : sig
  val vram_byte : t -> int -> int
  val set_vram_byte : t -> addr:int -> data:int -> unit
  val cram_entry : t -> int -> int
  val register : t -> int -> int
  val address : t -> int
  val code : t -> int
  val latch_pending : t -> bool
  val line_pending : t -> bool
  val vblank_flag : t -> bool
  val overflow : t -> bool
  val collision : t -> bool
  val line : t -> int
  val line_cycles : t -> int
  val line_counter : t -> int

  (** The background line buffers, as filled by the last rendered line.
      [bg_index] is the colour within the line's palette, so 0 is the value
      a sprite is allowed to show through. *)
  val bg_index : t -> int -> int

  val bg_palette : t -> int -> int
  val bg_priority : t -> int -> bool
  val vscroll_latch : t -> int
  val render_line : t -> line:int -> unit

  (** The register decoding, private to the chip and not yet observable in
      any other way. *)
  module Regs : sig
    val vscroll_lock : t -> bool
    val hscroll_lock : t -> bool
    val hide_left_column : t -> bool
    val line_irq_enabled : t -> bool
    val shift_sprites : t -> bool
    val mode4 : t -> bool
    val display_enabled : t -> bool
    val frame_irq_enabled : t -> bool
    val tall_sprites : t -> bool
    val zoom_sprites : t -> bool
    val active_lines : t -> int
    val name_table_base : t -> int
    val sprite_attr_base : t -> int
    val sprite_pattern_base : t -> int
    val backdrop_colour : t -> int
  end
  val set_flags : t -> vblank:bool -> overflow:bool -> collision:bool -> unit
  val set_line_pending : t -> bool -> unit
end
