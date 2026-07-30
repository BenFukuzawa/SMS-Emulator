open Uints

(* The VDP owns its own 16KB of video RAM, which is wired directly to the
   chip and never appears in the Z80 address space. Everything a program can
   see goes through two I/O ports: $BE (data) and $BF (control when written,
   status when read). This module is the state behind that keyhole.

   Registers, CRAM and the address counter are plain ints rather than
   uint8/uint16: the VDP is almost entirely bit manipulation, and the
   conversions cost more clarity than the type safety buys. Uints appear at
   the port boundary only. *)

type t =
  { vram : Bytes.t
  ; cram : int array (* 32 entries, --BBGGRR *)
  ; registers : int array (* R0..R10, write-only from the program *)
  ; mutable address : int (* 14-bit, auto-increments on every data access *)
  ; mutable code : int (* 0=vram read, 1=vram write, 2=register, 3=cram *)
  ; mutable latch : int option (* first byte of a control pair, if pending *)
  ; mutable read_buffer : int (* data reads run one byte behind; see below *)
  ; mutable vblank_flag : bool (* status bit 7 *)
  ; mutable overflow : bool (* status bit 6: 9+ sprites on a line *)
  ; mutable collision : bool (* status bit 5 *)
  ; mutable line_pending : bool (* line interrupt fired, not yet acked *)
  ; mutable line : int (* scanline being drawn, 0 .. 261 *)
  ; mutable line_cycles : int (* T-states elapsed within that line *)
  ; mutable line_counter : int (* counts down from R10 between interrupts *)
  ; mutable frames : int (* completed frames; the host diffs this *)
  ; mutable vscroll : int (* R9, sampled once per frame and held *)
  ; (* One scanline of background, as the chip sees it rather than as a
       screen sees it. Sprites are composited against these, and the test
       they lose is "priority set and the background pixel is not colour 0"
       -- which needs the pattern index, not a colour. Allocated once and
       overwritten every line. *)
    bg_index : int array (* 256, colour 0-15 within the line's palette *)
  ; bg_palette : int array (* 256, 0 = CRAM 0-15, 1 = CRAM 16-31 *)
  ; bg_priority : bool array (* 256, name table bit p *)
  ; sprite_index : int array (* 256, 0 = no sprite pixel here *)
  }

(* North American hardware only: 262 scanlines of 228 T-states each, so 59736
   T-states per frame, which against the 3.579545 MHz Z80 clock is 59.92 Hz.
   A PAL machine keeps the 228-cycle line and has 313 of them; nothing else
   about the chip changes, so should it ever be wanted it is this constant
   and the V counter table below. *)
let cycles_per_line = 228
let lines_per_frame = 262

(* --- register decoding -------------------------------------------------

   The registers are a bag of bits with no structure of their own. Every
   question the rest of the chip wants to ask gets a name here, so that no
   later code indexes t.registers directly and no bit number is written out
   twice.

   The SMS1 mask bits -- R2 bit 0, R5 bit 0, R6 bits 1-0, and the whole of R3
   and R4 -- are deliberately ignored. On the SMS1 they AND against the VRAM
   address bus and can mirror a table onto itself; on the SMS2 the gate
   always passes. Every commercial game sets them to 1, so the difference is
   unobservable outside a test written against an SMS1. *)

let r t n = t.registers.(n)

(* R0 *)
let vscroll_lock t =
  r t 0 land 0x80 <> 0 (* cols 24-31 pinned to vscroll 0 *)
;;

let hscroll_lock t =
  r t 0 land 0x40 <> 0 (* lines 0-15 pinned to hscroll 0 *)
;;

let hide_left_column t = r t 0 land 0x20 <> 0
let line_irq_enabled t = r t 0 land 0x10 <> 0 (* IE1 *)
let shift_sprites t = r t 0 land 0x08 <> 0 (* EC: sprites move left 8px *)
let mode4 t = r t 0 land 0x04 <> 0

(* R1 *)
let display_enabled t = r t 1 land 0x40 <> 0 (* BLK *)
let frame_irq_enabled t = r t 1 land 0x20 <> 0 (* IE0 *)
let tall_sprites t = r t 1 land 0x02 <> 0 (* 8x16 rather than 8x8 *)
let zoom_sprites t = r t 1 land 0x01 <> 0 (* every sprite pixel doubled *)

(* Display height. The mode bits keep the names they had on the TMS9918a,
   where M1 selected Text, M2 Multicolor and M3 Graphics II; in Mode 4 they
   are reused to pick the number of active lines. An extra-height mode needs
   M3 set as well as its own select bit, and needs the other height's select
   bit clear. With M3 clear the chip is in the 192-line mode whatever M1 and
   M2 say.

   Checked against the M1/M2/M3/M4 combination table in MacDonald's
   msvdp-20021112.txt, SMS2 column, which has to be read through his own
   mode-bit names: what he calls M2 is R0 bit 1 and what he calls M3 is R1
   bit 3, the opposite of the TI and SMS Power naming used here. Translated
   to physical bits, his table confirms all six rows this reaches, including
   the all-bits-set row, which he gives as plain Mode 4 -- 192 lines.

   Two rows are deliberately not modelled. R0 bit 1 clear with R1 bit 4 set
   is "invalid text mode" on the SMS2; this returns 192 for it instead.
   Nothing writes that combination. *)
let active_lines t =
  let m3 = r t 0 land 0x02 <> 0 in
  let m1 = r t 1 land 0x10 <> 0 in
  let m2 = r t 1 land 0x08 <> 0 in
  if not m3
  then 192
  else if m1 && not m2
  then 224
  else if m2 && not m1
  then 240
  else 192
;;

(* Table bases. The 192-line name table is 32x28 entries and lands on an $800
   boundary; the taller modes use a 32x32 table on a $1000 boundary with $700
   added, which is why the two cases share no arithmetic. *)
let name_table_base t =
  if active_lines t = 192
  then (r t 2 land 0x0E) lsl 10 (* bits 3-1 * $800; $FF -> $3800 *)
  else (((r t 2 lsr 2) land 0x03) lsl 12) lor 0x700 (* $FF -> $3700 *)
;;

let sprite_attr_base t = (r t 5 land 0x7E) lsl 7 (* $FF -> $3F00 *)

let sprite_pattern_base t =
  (r t 6 land 0x04) lsl 11 (* bit 2: $0000 or $2000 *)
;;

(* The backdrop is picked from the *sprite* half of CRAM, not the background
   half -- the one place a background-ish colour comes from entries 16-31. *)
let backdrop_colour t = 16 + (r t 7 land 0x0F)

(* Power-on values, from the Mark III software reference manual. *)
let initial_registers =
  [| 0x36 (* R0: mode *)
   ; 0xA0 (* R1: mode *)
   ; 0xFF (* R2: name table at $3800 *)
   ; 0xFF (* R3 *)
   ; 0xFF (* R4 *)
   ; 0xFF (* R5: sprite attribute table at $3F00 *)
   ; 0xFB (* R6: sprite patterns in the first 8K *)
   ; 0x00 (* R7: border colour 0 *)
   ; 0x00 (* R8: horizontal scroll *)
   ; 0x00 (* R9: vertical scroll *)
   ; 0xFF (* R10: line interrupt off *)
  |]
;;

let create () =
  { vram = Bytes.make 0x4000 '\000'
  ; cram = Array.make 0x20 0
  ; (* copied, so two VDPs never share the array *)
    registers = Array.copy initial_registers
  ; address = 0
  ; code = 0
  ; latch = None
  ; read_buffer = 0
  ; vblank_flag = false
  ; overflow = false
  ; collision = false
  ; line_pending = false
  ; line = 0
  ; line_cycles = 0
  ; line_counter = initial_registers.(10)
  ; frames = 0
  ; vscroll = initial_registers.(9)
  ; bg_index = Array.make 256 0
  ; bg_palette = Array.make 256 0
  ; bg_priority = Array.make 256 false
  ; sprite_index = Array.make 256 0
  }
;;

(* Data reads run one byte behind the address counter. The chip can only
   reach VRAM in the slots left over between display fetches, so rather than
   stalling the Z80 it reads ahead into a buffer. Setting up a read has to
   prime that buffer, otherwise every VRAM read a program makes comes back
   shifted by one byte. *)
let prefetch t =
  t.read_buffer <- Char.code (Bytes.get t.vram t.address);
  t.address <- (t.address + 1) land 0x3FFF
;;

(* Port $BF, written. Commands are two bytes, but they arrive one at a time
   and the half-finished state is real hardware state: a program can be
   interrupted between the two, and an interrupt handler that reads the
   status port will destroy the pair. That is why the manual says to wrap
   control writes in DI/EI, and why this cannot be an API that takes both
   bytes at once.

   Second byte, bits 7-6: 0 = set up VRAM read 1 = set up VRAM write 2 =
   write register 3 = set up CRAM write *)
let write_control t byte =
  let byte = Uint8.to_int byte in
  match t.latch with
  | None -> t.latch <- Some byte
  | Some low ->
    t.latch <- None;
    t.code <- byte lsr 6;
    t.address <- ((byte land 0x3F) lsl 8) lor low;
    (match t.code with
     | 0 -> prefetch t
     | 2 ->
       (* The data is the *first* byte of the pair; the register number is
          the low nibble of the second. Nothing goes to the data port. Only
          R0..R10 exist -- higher numbers land nowhere. *)
       let n = byte land 0x0F in
       if n < Array.length t.registers then t.registers.(n) <- low
     | _ -> ())
;;

(* Port $BF, read. Not a getter: reading is how the program acknowledges an
   interrupt, so it clears every flag and drops the IRQ line. It also clears
   the control latch, which is the desync the manual warns about.

   The low five bits are not driven by the chip and conventionally read back
   as ones. *)
let read_status t =
  let v =
    (if t.vblank_flag then 0x80 else 0)
    lor (if t.overflow then 0x40 else 0)
    lor (if t.collision then 0x20 else 0)
    lor 0x1F
  in
  t.vblank_flag <- false;
  t.overflow <- false;
  t.collision <- false;
  t.line_pending <- false;
  t.latch <- None;
  Uint8.of_int v
;;

(* Port $BE, written. Only code 3 targets CRAM; codes 0, 1 and 2 all land in
   VRAM. The address counter is 14 bits wide and increments either way -- it
   is only the CRAM *index* that is the low five bits of it, so writing 33
   bytes of palette wraps onto entry 0 while the counter reads 33.

   The written byte also lands in the read buffer, which is the chip passing
   it through the same latch on its way out. *)
let write_data t byte =
  let byte = Uint8.to_int byte in
  t.latch <- None;
  if t.code = 3
  then t.cram.(t.address land 0x1F) <- byte
  else Bytes.set t.vram t.address (Char.chr byte);
  t.read_buffer <- byte;
  t.address <- (t.address + 1) land 0x3FFF
;;

(* Port $BE, read. Hand back the buffer and refill it -- exactly the same
   step the read setup performs, one access earlier. CRAM is genuinely
   write-only, so reads come from VRAM whatever the latched code says. *)
let read_data t =
  t.latch <- None;
  let v = t.read_buffer in
  prefetch t;
  Uint8.of_int v
;;

(* --- the scanline engine -----------------------------------------------

   The VDP is free-running: it walks 262 lines regardless of what the Z80 is
   doing, and the Z80 finds out where it is only by reading the counters or
   taking an interrupt. `step` is called with the T-states each instruction
   consumed, so the two stay in step to within one instruction. *)

(* Port $7E. The line number, but only 256 of them fit in a byte, so the
   counter jumps backwards partway down the frame and repeats a stretch of
   values. Games poll this to find the line they want to change something on,
   so the jump has to be in the right place: the segments below are
   contiguous runs of returned values, and their lengths sum to 262.

   Tables are MacDonald's, msvdp-20021112.txt, "V counter values".

   The 240-line row is his too, and it does not sum to 262: that mode does
   not work on an NTSC machine, and per the same document has "no border,
   blanking, or retrace period", so its frame is 263 lines and the picture
   rolls. This engine runs a fixed 262-line frame, so t.line never reaches
   the last entry and $06 is never returned. The row is written as documented
   rather than as this engine can reach it. *)
let v_counter_segments t =
  match active_lines t with
  | 192 -> [ 0x00, 0xDA; 0xD5, 0xFF ] (* 219 + 43 = 262 *)
  | 224 -> [ 0x00, 0xEA; 0xE5, 0xFF ] (* 235 + 27 = 262 *)
  | _ -> [ 0x00, 0xFF; 0x00, 0x06 ]
;;

(* 256 + 7 = 263; see above *)

(* where is the line rn *)
let v_counter t =
  let rec walk n = function
    | [] -> 0xFF (* unreachable: the segments cover the whole frame *)
    | (lo, hi) :: rest ->
      let len = hi - lo + 1 in
      if n < len then lo + n else walk (n - len) rest
  in
  walk t.line (v_counter_segments t) |> Uint8.of_int
;;

(* Port $7F. A latched pixel counter, used by the light gun and by a few
   games doing mid-line effects. Nothing models it yet; returning zero is a
   known gap rather than a guess. *)
let h_counter _t = Uint8.of_int 0x00
let frame_count t = t.frames

(* --- the background renderer -------------------------------------------

   One scanline of tilemap, straight into the line buffers. Everything here
   is from MacDonald's msvdp-20021112.txt unless marked otherwise.

   This walks screen pixels rather than tile columns, refetching the name
   table entry for all 256 of them. The chip fetches 33 entries per line and
   shifts out eight pixels each; doing it per pixel costs eight times the
   VRAM reads and buys arithmetic with no special case at the column
   boundaries, which is where fine horizontal scrolling goes wrong. If a
   profile ever says this matters, the fix is to cache the decoded entry
   while (row, col) is unchanged. *)

let vram t addr = Char.code (Bytes.unsafe_get t.vram (addr land 0x3FFF))

(* "Each pattern uses 32 bytes. The first four bytes are bitplanes 0 through
   3 for line 0, the next four bytes are bitplanes 0 through 3 for line 1,
   etc., up to line 7." So the four bytes of a row are not four pixels: each
   contributes one bit to all eight, and a pixel's colour is read across
   them. *)
let pattern_pixel t ~addr ~x =
  let bit = 7 - x in
  let plane n = (vram t (addr + n) lsr bit) land 1 in
  plane 0 lor (plane 1 lsl 1) lor (plane 2 lsl 2) lor (plane 3 lsl 3)
;;

(* Register 7 names an entry in the sprite half of CRAM, so it is written
   into the buffers as palette 1 and the register's low nibble. *)
let fill_with_backdrop t ~from ~until =
  let index = r t 7 land 0x0F in
  for x = from to until do
    t.bg_index.(x) <- index;
    t.bg_palette.(x) <- 1;
    t.bg_priority.(x) <- false
  done
;;

let render_background t =
  let line = t.line in
  let base = name_table_base t in
  begin
    (* "In 192-line mode the vertical scroll register wraps past 223"; the
       taller modes wrap past 255. That is the tilemap being 28 rows tall
       rather than 32. *)
    let map_height = if active_lines t = 192 then 224 else 256 in
    (* "If bit #6 of VDP register $00 is set, horizontal scrolling will be
       fixed at zero for scanlines zero through 15." *)
    let hscroll = if hscroll_lock t && line < 16 then 0 else r t 8 in
    for x = 0 to 255 do
      (* "If bit 7 of register $00 is set, the vertical scroll value will be
         fixed to zero when columns 24 to 31 are rendered." Columns 24-31 are
         screen pixels 192-255. *)
      let vscroll = if vscroll_lock t && x >= 192 then 0 else t.vscroll in
      (* R8 shifts the picture right, so the source moves left. MacDonald
         describes this as a starting column of 32 minus the register's top
         five bits plus a three-bit fine offset; subtracting the whole
         register from the pixel and wrapping at 256 is the same thing with
         the column and the fine part not pulled apart. *)
      let src_x = (x - hscroll) land 0xFF in
      let src_y = (line + vscroll) mod map_height in
      let entry = base + ((((src_y lsr 3) * 32) + (src_x lsr 3)) * 2) in
      (* ---pcvhnnnnnnnnn, little endian. *)
      let lo = vram t entry
      and hi = vram t (entry + 1) in
      let tile = ((hi land 0x01) lsl 8) lor lo in
      let hflip = hi land 0x02 <> 0
      and vflip = hi land 0x04 <> 0 in
      let row = src_y land 7
      and col = src_x land 7 in
      let row = if vflip then 7 - row else row
      and col = if hflip then 7 - col else col in
      t.bg_index.(x)
        <- pattern_pixel t ~addr:((tile * 32) + (row * 4)) ~x:col;
      t.bg_palette.(x) <- (hi lsr 3) land 1;
      t.bg_priority.(x) <- hi land 0x10 <> 0
    done
  end
;;

(* --- the sprite renderer -----------------------------------------------

   "Each sprite is defined in the sprite attribute table (SAT), a 256-byte
   table located in VRAM", holding 64 sprites laid out as

     00: yyyyyyyyyyyyyyyy   y = Y coordinate + 1
     ...
     80: xnxnxnxnxnxnxnxn   x = X coordinate, n = pattern index

   so sprite i has its Y at base+i and its X and pattern index at
   base+$80+2i and base+$80+2i+1. The $40-$7F gap is unused and some games
   store their own data there.

   That colour 0 is transparent is NOT stated anywhere in MacDonald's
   document. It is assumed here, on the strength of the collision and
   priority rules both being written in terms of "opaque" pixels, which
   presupposes that some sprite pixels are not. *)

let sprite_height t =
  (if tall_sprites t then 16 else 8) * if zoom_sprites t then 2 else 1
;;

let draw_sprite t ~sprite ~row =
  let sat = sprite_attr_base t in
  let x0 = vram t (sat + 0x80 + (sprite * 2)) in
  let pattern = vram t (sat + 0x80 + (sprite * 2) + 1) in
  (* "D3 - (EC) 1 = Shift sprites left by 8 pixels" *)
  let x0 = if shift_sprites t then x0 - 8 else x0 in
  (* "When bit 0 of register #1 is set, sprite pixels are zoomed to double
     their size." The SMS1 only zooms four of the eight sprites on a line
     horizontally; the SMS2 modelled here zooms all of them. *)
  let zoom = zoom_sprites t in
  let row = if zoom then row / 2 else row in
  (* "When bit 1 of register #1 is set, bit 0 of the pattern index is
     ignored... the same pattern index plus one is used for the bottom
     half." Rows 8-15 give a row offset past 32 bytes, so the address walks
     into the next pattern on its own. *)
  let pattern = if tall_sprites t then pattern land 0xFE else pattern in
  let addr = sprite_pattern_base t + (pattern * 32) + (row * 4) in
  let width = if zoom then 2 else 1 in
  for px = 0 to 7 do
    let colour = pattern_pixel t ~addr ~x:px in
    if colour <> 0
    then
      for d = 0 to width - 1 do
        let x = x0 + (px * width) + d in
        if x >= 0 && x < 256
        then
          (* "An opaque pixel from a lower-entry sprite is displayed over any
             opaque pixel from a higher-entry sprite", and sprites are drawn
             in order, so an occupied slot means the earlier sprite wins --
             and that the two have collided. *)
          if t.sprite_index.(x) <> 0
          then t.collision <- true
          else t.sprite_index.(x) <- colour
      done
  done
;;

let render_sprites t =
  let line = t.line in
  let sat = sprite_attr_base t in
  let height = sprite_height t in
  (* "If the Y coordinate is set to $D0, then the sprite in question and all
     remaining sprites of the 64 available will not be drawn." No effect in
     the taller modes. *)
  let terminates = active_lines t = 192 in
  let drawn = ref 0 in
  let sprite = ref 0 in
  let stop = ref false in
  while (not !stop) && !sprite < 64 do
    let y = vram t (sat + !sprite) in
    if terminates && y = 0xD0
    then stop := true
    else (
      (* "The Y coordinate is treated as being plus one, so a value of zero
         would place a sprite on scanline 1 and not scanline zero." Compared
         as plain integers, so a Y near $FF puts the sprite past the bottom
         of the screen rather than wrapping to the top. *)
      let row = line - (y + 1) in
      if row >= 0 && row < height
      then
        if !drawn = 8
        then
          (* "If all eight buffer entries have been used and there are more
             sprites that fall on the same line, bit 6 of the status flags is
             set" -- "regardless of the sprite X coordinate or pattern
             data", so this is decided before anything is fetched. *)
          (t.overflow <- true;
           stop := true)
        else (
          incr drawn;
          draw_sprite t ~sprite:!sprite ~row);
      incr sprite)
  done
;;

let render_line t =
  Array.fill t.sprite_index 0 256 0;
  (* With the display off nothing is fetched at all and the whole line,
     active area included, is the overscan colour. *)
  if not (display_enabled t)
  then fill_with_backdrop t ~from:0 ~until:255
  else (
    render_background t;
    render_sprites t;
    (* "1 = Mask column 0 with overscan color from register #7." The mask is
       the last thing the chip does, so it covers sprites too. *)
    if hide_left_column t
    then (
      fill_with_backdrop t ~from:0 ~until:7;
      Array.fill t.sprite_index 0 8 0))
;;

(* The finished pixel, as an index into CRAM's 32 entries.

   "The resulting sprite pixel is printed over any low priority background
   tile. Or, for high priority background tiles, only where there is a
   transparent pixel." Sprite colours "are always taken from the second
   group of 16 colors in the color RAM". *)
let composite t x =
  let sprite = t.sprite_index.(x) in
  if sprite <> 0 && not (t.bg_priority.(x) && t.bg_index.(x) <> 0)
  then 16 + sprite
  else (t.bg_palette.(x) * 16) + t.bg_index.(x)
;;

(* Everything that happens between one line and the next, in the order the
   chip does it. *)
let end_of_line t =
  let active = active_lines t in
  if t.line < active then render_line t;
  (* The line counter runs across the active display and one line past it,
     and is reloaded from R10 on every other line. That continuous reload
     during blanking is what makes R10 = n fire on line n of the next frame,
     rather than n lines after wherever the last interrupt happened to leave
     the counter. *)
  if t.line <= active
  then
    if t.line_counter = 0
    then (
      t.line_counter <- r t 10;
      t.line_pending <- true)
    else t.line_counter <- t.line_counter - 1
  else t.line_counter <- r t 10;
  (* Finishing the last active line is the moment vblank begins. *)
  if t.line = active - 1 then t.vblank_flag <- true;
  t.line <- t.line + 1;
  if t.line >= lines_per_frame
  then (
    t.line <- 0;
    t.frames <- t.frames + 1;
    (* MacDonald: "The vertical scroll value cannot be changed during the
       active display period, any changes made will be stored in a temporary
       location and used only when the active display period ends."

       Sampling at the frame boundary rather than at the end of the active
       display is equivalent for anything visible. A write during the active
       display is deferred either way; a write during blanking reaches the
       chip immediately on hardware, but nothing renders between there and
       line 0, so the only value that can matter is whatever R9 holds when
       the display restarts -- which is what is captured here.

       Horizontal scroll is the opposite and is sampled per line, by the
       renderer. *)
    t.vscroll <- r t 9)
;;

let rec step t ~cycles =
  t.line_cycles <- t.line_cycles + cycles;
  if t.line_cycles >= cycles_per_line
  then (
    t.line_cycles <- t.line_cycles - cycles_per_line;
    end_of_line t;
    (* moves it to new line *)
    (* A long instruction can span more than one line boundary. *)
    step t ~cycles:0)
;;

(* The interrupt line, recomputed on demand rather than cached. A program
   that enables an interrupt whose flag is already pending must see the
   request immediately, and it will: nothing here remembers a decision made
   before the register write. Reading the status port clears both flags and
   so drops the line, which is the acknowledgement the Z80 never sends. *)
let irq t =
  (t.vblank_flag && frame_irq_enabled t)
  || (t.line_pending && line_irq_enabled t)
;;

(* Direct access, bypassing the ports. Tests use this to arrange state and to
   check where bytes actually landed; nothing in the emulator should. *)
module For_tests = struct
  let vram_byte t addr = Bytes.get t.vram addr |> Char.code
  let set_vram_byte t ~addr ~data = Bytes.set t.vram addr (Char.chr data)
  let cram_entry t n = t.cram.(n)
  let register t n = t.registers.(n)
  let address t = t.address
  let code t = t.code
  let latch_pending t = Option.is_some t.latch
  let line_pending t = t.line_pending
  let vblank_flag t = t.vblank_flag
  let overflow t = t.overflow
  let collision t = t.collision
  let line t = t.line
  let line_cycles t = t.line_cycles
  let line_counter t = t.line_counter
  let bg_index t x = t.bg_index.(x)
  let bg_palette t x = t.bg_palette.(x)
  let bg_priority t x = t.bg_priority.(x)
  let sprite_index t x = t.sprite_index.(x)
  let composite t x = composite t x
  let vscroll_latch t = t.vscroll

  (* Render one line on demand. The engine only ever renders the line it is
     on; a test wants to pick one. *)
  let render_line t ~line =
    t.line <- line;
    render_line t
  ;;

  (* The register decoding, which is otherwise private to the chip and only
     observable through pixels that do not exist yet. *)
  module Regs = struct
    let vscroll_lock = vscroll_lock
    let hscroll_lock = hscroll_lock
    let hide_left_column = hide_left_column
    let line_irq_enabled = line_irq_enabled
    let shift_sprites = shift_sprites
    let mode4 = mode4
    let display_enabled = display_enabled
    let frame_irq_enabled = frame_irq_enabled
    let tall_sprites = tall_sprites
    let zoom_sprites = zoom_sprites
    let active_lines = active_lines
    let name_table_base = name_table_base
    let sprite_attr_base = sprite_attr_base
    let sprite_pattern_base = sprite_pattern_base
    let backdrop_colour = backdrop_colour
  end

  let set_flags t ~vblank ~overflow ~collision =
    t.vblank_flag <- vblank;
    t.overflow <- overflow;
    t.collision <- collision
  ;;

  let set_line_pending t v = t.line_pending <- v
end
