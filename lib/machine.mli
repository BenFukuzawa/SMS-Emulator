(** A Master System.

    This is the board: it owns the chips, connects them to each other, and
    keeps them in step. It contains no emulation of its own -- every
    register, cycle count and pixel belongs to one of the chips it holds.

    Nothing here knows what a window, a file or a keypress is. That is the
    host program's half, and the two meet at [run_frame]: this module decides
    what a frame *is*, the host decides when to ask for one and what to do
    with the picture that comes back. *)

type t

(** Insert a cartridge and power on. [rom] is the raw file, mapper and all. *)
val create : rom:bytes -> t

(** Run the console until the VDP finishes the frame it is on. Typically
    around 60,000 T-states, but the exact number depends on where the last
    instruction left off. *)
val run_frame : t -> unit

(** One instruction, or one interrupt if one is pending, with the VDP
    advanced by exactly the T-states it cost. Returns that count. This is the
    unit [run_frame] is built from, exposed so a debugger can advance the
    machine by less than a frame. The picture is torn until the frame
    finishes -- see [framebuffer]. *)
val step : t -> int

(** Run until the VDP moves to the next scanline. The useful granularity for
    watching a raster effect: mid-frame register writes are what make the
    status bar hold still while the level scrolls under it. *)
val step_scanline : t -> unit

(** The finished picture: three bytes per pixel, red first, top row first.
    Not a copy -- blit it, do not keep it. Call after [run_frame], never
    during, or the top and bottom of the image will come from different
    frames. *)
val framebuffer : t -> Bytes.t

(** Width and height of the live part of [framebuffer]. Changes when a
    program selects a taller display mode, so read it every frame rather than
    caching it. *)
val frame_size : t -> int * int

(** Frames completed since power on. *)
val frame_count : t -> int

(** The two controller ports, and a pad's inputs. Both are the joypad's own
    types, re-exported so a frontend can drive the machine without naming
    that module. Pause is not among the buttons -- see [pause].

    Mapping keys to buttons is deliberately not here: this module knows
    nothing about keyboards. [Joypad.button_of_char] is where that lives. *)
type player = Joypad.player =
  | One
  | Two

type button = Joypad.button =
  | Up
  | Down
  | Left
  | Right
  | Button1
  | Button2

val press : t -> player -> button -> unit
val release : t -> player -> button -> unit

(** Let go of everything on both pads. Worth calling on focus loss, so a key
    held while the window goes away cannot leave a button stuck down. *)
val release_all : t -> unit

(** The Pause button, which sits on the console rather than the pad and is
    wired to the CPU's non-maskable interrupt. Edge triggered: one call is
    one press. *)
val pause : t -> unit

module For_tests : sig
  (** Read as the CPU would: through the mapper and the RAM mirror. For
      watching what a ROM left behind, which is otherwise invisible from
      outside the machine. *)
  val read_byte : t -> int -> int
end

(** What a debugger needs to see, and nothing it could change with.

    Every function here reads. [Mem.read_byte] has no side effects -- the
    mapper snoops writes, not reads -- so walking memory from outside cannot
    perturb the run being described. That property is what makes a live
    inspection view safe to point at a running game.

    Kept apart from [For_tests] because the two carry different promises. A
    test peephole may be narrowed the moment its test goes away; this is a
    surface a frontend is built on. *)
module For_debug : sig
  (** Read as the CPU would, through the mapper and the RAM mirror. *)
  val read_byte : t -> int -> int

  (** The video chip, for its own [For_tests] accessors: VRAM, CRAM, the
      register file and the scanline counter. *)
  val vdp : t -> Vdp.t

  val registers : t -> Registers.t
  val pc : t -> int

  (** [iff1, iff2, im, i, refresh, halted]. *)
  val interrupt_state : t -> bool * bool * int * int * int * bool

  (** The Sega mapper's three 16 KB page registers: which ROM bank is
      currently visible at $0000, $4000 and $8000. A big game repages these
      constantly, which is how it fits in a 48 KB window. *)
  val mapper_pages : t -> int * int * int
end
