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

(** The active display: three bytes per pixel, red first, top row first. The
    buffer is sized for the tallest mode and written as each line is drawn,
    so reading it mid-frame gives a torn picture -- wait on [frame_count].

    Not a copy. The host is expected to blit it, not keep it. *)
val framebuffer : t -> Bytes.t

(** Width and height of the live part of [framebuffer], in pixels. Height
    follows the display mode, so it changes when a program writes R0 or R1. *)
val frame_size : t -> int * int

(** Recolouring: imposing a colour on one of the 32 CRAM entries, for as long
    as it is set. Not something the hardware could do and not visible to the
    program, which goes on writing whatever palette it likes -- the
    substitution happens between CRAM and the screen.

    What is stored is a transform of the colour the program writes and not a
    colour to put in its place, because the program keeps writing: a game
    reloads its palette every level and walks the whole thing down to black
    to fade out. A colour poked into CRAM is gone within a frame.

    An entry is shared by everything drawn through it, which is the hardware
    and not this: sprites all read the upper 16 entries, so recolouring one
    character recolours anything else drawn from the same entries -- the
    backdrop included, since R7 names one of them too. *)

(** Take the hue and saturation of [rgb], leaving the entry the lightness the
    program gives it. Shading ramps keep their steps and fades go on working,
    both being lightness.

    White, black and grey are not reachable this way: they are the absence of
    a hue rather than a position among them, so asking for white gets the
    grey ramp underneath the colour rather than white. Use [set_replace] for
    those. *)
val set_tint : t -> entry:int -> rgb:int -> unit

(** Land the entry on [rgb] exactly, and go on scaling with the program from
    there -- so this does reach white and black, and still fades, the
    lightness being carried as a factor rather than a value.

    The cost is at the extremes. Asking a whole ramp for white clamps every
    step at the top, and the shading collapses to a silhouette; that is what
    a white sprite is, but it is worth knowing before asking for one. *)
val set_replace : t -> entry:int -> rgb:int -> unit

(** Put the entry back to what the program says it is. *)
val clear_recolour : t -> entry:int -> unit

(** Whether a recolour is in force on an entry. *)
val recolour : t -> entry:int -> bool

(** What an entry reaches the screen as, packed [0xRRGGBB]: the colour the
    program wrote, after any recolour. This is the palette the renderer
    actually reads, so a viewer showing colours should show these. *)
val palette_rgb : t -> entry:int -> int

(** The hue of a packed [0xRRGGBB] in degrees, or [None] for a grey, which is
    off the colour circle rather than at some point on it. *)
val hue_of_rgb : int -> int option

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

  (** The sprite colour at a pixel, 0 where no sprite covers it. *)
  val sprite_index : t -> int -> int

  (** Sprite and background resolved into one CRAM entry, 0-31. *)
  val composite : t -> int -> int

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
    val display_enabled : t -> bool
    val frame_irq_enabled : t -> bool
    val tall_sprites : t -> bool
    val zoom_sprites : t -> bool
    val active_lines : t -> int
    val name_table_base : t -> int
    val sprite_attr_base : t -> int
    val sprite_pattern_base : t -> int
  end
  val set_flags : t -> vblank:bool -> overflow:bool -> collision:bool -> unit
  val set_line_pending : t -> bool -> unit
end
