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

(** The sound generated while the last [run_frame] ran, in [-1.0, 1.0], and
    the rate it is meant to be played back at. Around 735 samples a frame at
    44.1 kHz. Taking them empties the queue, so call it once per frame. *)
val audio : t -> float array

val audio_rate : t -> int
val audio_pending : t -> int

(** Discard queued sound. For a host that has fallen behind and would rather
    skip than play audio that is already late. *)
val drop_audio : t -> unit

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
