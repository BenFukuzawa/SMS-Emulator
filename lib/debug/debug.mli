(** The machine, described rather than run.

    Everything here derives something a person can look at from state a
    running [Machine] already holds: a disassembly around the program
    counter, the tile patterns in video RAM, the tilemap the background is
    fetched from, the sprite table. It performs no emulation and changes
    nothing -- the whole module is a read of [Machine.For_debug].

    It also knows nothing about a screen. Buffers come back as raw RGB and
    listings as ordinary values; where they are drawn is the frontend's
    business. That split is what lets the whole thing be tested without a
    browser. *)

(** {1 Disassembly} *)

type line =
  { addr : int
  ; bytes : int list (** the instruction's own bytes, for the listing *)
  ; text : string
  ; is_data : bool
  (** True when the bytes at [addr] are not a valid instruction and [text] is
      a [DB $xx] placeholder. Linear disassembly walks off the end of
      routines into data constantly, so this is a normal occurrence rather
      than an error. *)
  }

(** [disassemble m ~at ~count] decodes [count] instructions starting at [at],
    each one beginning where the last ended.

    Two things this cannot do, both inherent to a variable-length instruction
    set. It cannot run backwards, so [at] is a start and not a centre --
    there is no way to know where the instruction before an address began
    without having watched the program get there. And data embedded in a code
    stream desynchronises the following few lines until the decoding happens
    to resync. *)
val disassemble : Machine.t -> at:int -> count:int -> line list

(** {1 Colour} *)

(** CRAM entry 0-31 as it reaches the screen, packed [0xRRGGBB]. The VDP
    stores two bits per channel, so this is one of only 64 possible colours
    unless [Vdp.set_recolour] has been used on the entry. *)
val cram_rgb : Machine.t -> int -> int

(** All 32 entries, background palette first. *)
val cram : Machine.t -> int array

(** {1 Patterns}

    Video RAM holds 512 eight-by-eight patterns. Their colour is stored
    across four bitplanes rather than packed per pixel, so a pattern is
    unreadable without decoding -- which is the whole reason to show them.

    The buffers below are reused between calls, exactly as [Vdp.framebuffer]
    is. Blit them, do not keep them. *)

(** [tile_sheet m ~palette] renders every pattern in video RAM as a 32-by-16
    grid of tiles, 256x128 pixels, three bytes per pixel. Patterns carry no
    palette of their own -- the tilemap decides that per cell -- so [palette]
    picks which half of CRAM to read them through: 0 for the background
    palette, 1 for the sprite one. *)
val tile_sheet : Machine.t -> palette:int -> Bytes.t

(** Always 256 by 128. *)
val tile_sheet_size : int * int

(** {1 Tilemap}

    The background is not a picture but a grid of references into the
    patterns above, each with its own palette, flip and priority bits. The
    grid is larger than the screen; which part of it you see is [viewport]. *)

(** The whole name table rendered, three bytes per pixel. Reused buffer. *)
val tilemap : Machine.t -> Bytes.t

(** Width and height of [tilemap] in pixels. The height follows the display
    mode -- a 192-line mode wraps the map at 28 rows, taller modes at 32 --
    so read it rather than caching it. *)
val tilemap_size : Machine.t -> int * int

(** Where the visible screen currently sits on the tilemap, as
    [x, y, width, height] in tilemap pixels. Wraps: a viewport near the right
    edge continues at the left, so a frontend drawing this as a rectangle
    needs to draw it twice. *)
val viewport : Machine.t -> int * int * int * int

(** {1 Sprites} *)

type sprite =
  { index : int
  ; y : int (** as stored; the chip draws it one line lower *)
  ; x : int
  ; tile : int
  }

(** The sprite attribute table, up to the terminator.

    A Y coordinate of $D0 means "this sprite and every one after it is not
    drawn", so the list stops there and [terminated] says whether it did.
    Without that, the table's tail is leftover data and reading it as sprites
    is misleading. *)
val sprites : Machine.t -> sprite list * bool

(** {1 Chip state} *)

type vdp_state =
  { registers : int array (** all 11, as written *)
  ; line : int
  ; display_enabled : bool
  ; hide_left_column : bool
  ; tall_sprites : bool
  ; zoom_sprites : bool
  ; shift_sprites : bool
  ; name_table_base : int
  ; sprite_attr_base : int
  ; sprite_pattern_base : int
  }

val vdp_state : Machine.t -> vdp_state
