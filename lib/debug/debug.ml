open Uints

(* --- reading the machine without touching it ----------------------------

   The decoder is a functor over a bus, so pointing it at a machine is a
   matter of supplying one. This adapter is deliberately crippled: reads go
   through [Machine.For_debug.read_byte], which is the CPU's own path through
   the mapper and the RAM mirror, and writes go nowhere.

   That is not belt and braces. It is the property that makes a disassembly
   panel safe to point at a running game: the type checker now guarantees
   this module cannot alter the machine it is describing, however the decoder
   is changed later. *)
module Snoop = struct
  type t = Machine.t

  let read_byte m addr =
    Uint8.of_int (Machine.For_debug.read_byte m (Uint16.to_int addr))
  ;;

  let read_word m addr =
    let lo = Uint8.to_int (read_byte m addr) in
    let hi = Uint8.to_int (read_byte m Uint16.(succ addr)) in
    Uint16.of_int ((hi lsl 8) lor lo)
  ;;

  let accepts _ _ = true
  let write_byte _ ~addr:_ ~data:_ = ()
  let write_word _ ~addr:_ ~data:_ = ()
end

module Decode = Fetch_and_decode.Make (Snoop)

(* --- disassembly -------------------------------------------------------- *)

type line =
  { addr : int
  ; bytes : int list
  ; text : string
  ; is_data : bool
  }

(* [Instruction.show] keeps these to itself. They are small enough to
   restate, and restating them is the point: the listing wants different
   choices from the ones a log line wants. *)
let show_condition : Instruction.condition -> string = function
  | C -> "C"
  | NC -> "NC"
  | Z -> "Z"
  | NZ -> "NZ"
  | M -> "M"
  | P -> "P"
  | PE -> "PE"
  | PO -> "PO"
;;

let cond_prefix = function None -> "" | Some c -> show_condition c ^ ", "

let show_out_value : Instruction.out_value -> string = function
  | Out_register r -> Registers.show_r r
  | Out_zero -> "0"
;;

(* A relative jump stores a displacement from the *next* instruction, which
   is the one thing a reader cannot do in their head while following control
   flow. [Instruction.show] prints the raw number ("JR Z, 12") because that
   is what a log line wants; a listing wants the address it lands on. Ports
   get the same treatment: $BE means something to anyone reading SMS code,
   190 does not. Everything else is already right, so it falls through. *)
let render ~addr ~len (inst : Instruction.t) =
  let target rel = (addr + len + rel) land 0xFFFF in
  match inst with
  | JR (c, e) ->
    Printf.sprintf "JR %s$%04X" (cond_prefix c) (target (Int8.to_int e))
  | DJNZ e -> Printf.sprintf "DJNZ $%04X" (target (Int8.to_int e))
  | IN (Some r, Port_n n) ->
    Printf.sprintf "IN %s, ($%02X)" (Registers.show_r r) (Uint8.to_int n)
  | IN (None, Port_n n) -> Printf.sprintf "IN ($%02X)" (Uint8.to_int n)
  | OUT (Port_n n, v) ->
    Printf.sprintf "OUT ($%02X), %s" (Uint8.to_int n) (show_out_value v)
  | other -> Instruction.show other
;;

let byte m addr = Machine.For_debug.read_byte m (addr land 0xFFFF)

let decode_one m addr =
  match Decode.f m ~pc:(Uint16.of_int addr) with
  | info ->
    let len = max 1 (Uint16.to_int info.Inst_info.len) in
    let bytes = List.init len (fun i -> byte m (addr + i)) in
    ( { addr
      ; bytes
      ; text = render ~addr ~len info.Inst_info.inst
      ; is_data = false
      }
    , len )
  (* The decoder is written for a program counter, where every byte it
     reaches really is an instruction, and it says so with [assert false] and
     [invalid_arg] on the cases that cannot arise there. Walking forward from
     an address reaches data all the time -- a jump table, a string, a tile
     -- so those are ordinary outcomes here, not failures. Swallowing them
     and emitting one byte keeps the listing walking. *)
  | exception (Assert_failure _ | Invalid_argument _ | Failure _) ->
    let b = byte m addr in
    ( { addr
      ; bytes = [ b ]
      ; text = Printf.sprintf "DB $%02X" b
      ; is_data = true
      }
    , 1 )
;;

let disassemble m ~at ~count =
  let rec go addr n acc =
    if n <= 0
    then List.rev acc
    else (
      let line, len = decode_one m addr in
      go ((addr + len) land 0xFFFF) (n - 1) (line :: acc))
  in
  go (at land 0xFFFF) count []
;;

(* --- colour -------------------------------------------------------------

   Two bits per channel, spread evenly over 0-255. This is [cram_rgb] in
   vdp.ml; it is restated rather than shared because the VDP keeps its
   expanded palette private, and a viewer that disagreed with the renderer
   about what a colour looks like would be worse than useless. If the two
   ever drift, debug_test.ml catches it by comparing a rendered frame against
   this. *)
let expand2 v = v * 85

let rgb_of_entry e =
  let r = expand2 (e land 3)
  and g = expand2 ((e lsr 2) land 3)
  and b = expand2 ((e lsr 4) land 3) in
  (r lsl 16) lor (g lsl 8) lor b
;;

let cram_rgb m n =
  rgb_of_entry
    (Vdp.For_tests.cram_entry (Machine.For_debug.vdp m) (n land 31))
;;

let cram m =
  let vdp = Machine.For_debug.vdp m in
  Array.init 32 (fun i -> Vdp.For_tests.cram_entry vdp i)
;;

(* --- patterns -----------------------------------------------------------

   "Each pattern uses 32 bytes. The first four bytes are bitplanes 0 through
   3 for line 0", so the four bytes of a row are not four pixels: each
   contributes one bit to all eight. This is the same decode as
   [pattern_pixel] in vdp.ml, hoisted a row at a time because a viewer draws
   whole rows and the per-pixel form would re-read the same four bytes eight
   times. *)

let tile_count = 512
let sheet_w = 256
let sheet_h = 128
let tile_sheet_size = sheet_w, sheet_h

(* Reused between calls, as [Vdp.framebuffer] is. A viewer blits these every
   frame; allocating a fresh 96 KB each time would put the garbage collector
   in the middle of the draw loop for no gain. *)
let sheet_buf = Bytes.make (sheet_w * sheet_h * 3) '\000'
let map_buf = Bytes.make (256 * 256 * 3) '\000'

let put buf ~at ~rgb =
  Bytes.unsafe_set buf at (Char.unsafe_chr ((rgb lsr 16) land 0xFF));
  Bytes.unsafe_set buf (at + 1) (Char.unsafe_chr ((rgb lsr 8) land 0xFF));
  Bytes.unsafe_set buf (at + 2) (Char.unsafe_chr (rgb land 0xFF))
;;

(* One eight-pixel row of a pattern, written into [buf] at [at]. [pal] is the
   CRAM half, already multiplied up. *)
let blit_row vdp buf ~at ~addr ~pal ~flip =
  let plane n = Vdp.For_tests.vram_byte vdp ((addr + n) land 0x3FFF) in
  let p0 = plane 0
  and p1 = plane 1
  and p2 = plane 2
  and p3 = plane 3 in
  for x = 0 to 7 do
    let bit = if flip then x else 7 - x in
    let c =
      (p0 lsr bit)
      land 1
      lor (((p1 lsr bit) land 1) lsl 1)
      lor (((p2 lsr bit) land 1) lsl 2)
      lor (((p3 lsr bit) land 1) lsl 3)
    in
    let e = Vdp.For_tests.cram_entry vdp (pal + c) in
    put buf ~at:(at + (x * 3)) ~rgb:(rgb_of_entry e)
  done
;;

let tile_sheet m ~palette =
  let vdp = Machine.For_debug.vdp m in
  let pal = if palette = 0 then 0 else 16 in
  for tile = 0 to tile_count - 1 do
    let tx = tile mod 32 * 8
    and ty = tile / 32 * 8 in
    for row = 0 to 7 do
      let at = (((ty + row) * sheet_w) + tx) * 3 in
      blit_row
        vdp
        sheet_buf
        ~at
        ~addr:((tile * 32) + (row * 4))
        ~pal
        ~flip:false
    done
  done;
  sheet_buf
;;

(* --- tilemap ------------------------------------------------------------

   "---pcvhnnnnnnnnn, little endian": nine bits of pattern index, then flip,
   palette and priority. In 192-line mode the map is 28 rows tall and wraps
   past 224; the taller modes use all 32. *)

let map_rows m =
  let vdp = Machine.For_debug.vdp m in
  if Vdp.For_tests.Regs.active_lines vdp = 192 then 28 else 32
;;

let tilemap_size m = 256, map_rows m * 8

let tilemap m =
  let vdp = Machine.For_debug.vdp m in
  let base = Vdp.For_tests.Regs.name_table_base vdp in
  let rows = map_rows m in
  for cy = 0 to rows - 1 do
    for cx = 0 to 31 do
      let entry = (base + (((cy * 32) + cx) * 2)) land 0x3FFF in
      let lo = Vdp.For_tests.vram_byte vdp entry
      and hi = Vdp.For_tests.vram_byte vdp ((entry + 1) land 0x3FFF) in
      let tile = ((hi land 0x01) lsl 8) lor lo in
      let hflip = hi land 0x02 <> 0
      and vflip = hi land 0x04 <> 0 in
      let pal = (hi lsr 3) land 1 * 16 in
      for row = 0 to 7 do
        let src = if vflip then 7 - row else row in
        let at = ((((cy * 8) + row) * 256) + (cx * 8)) * 3 in
        blit_row
          vdp
          map_buf
          ~at
          ~addr:((tile * 32) + (src * 4))
          ~pal
          ~flip:hflip
      done
    done
  done;
  map_buf
;;

(* R8 shifts the picture right, so the source window moves left -- the same
   sign convention as [render_background]. *)
let viewport m =
  let vdp = Machine.For_debug.vdp m in
  let hscroll = Vdp.For_tests.register vdp 8 in
  (* R9 is latched once per frame, and it is the latch the renderer reads.
     Taking the register instead would put the rectangle somewhere the
     picture never was whenever a game writes R9 mid-frame -- which is
     exactly what a status bar that holds still does. *)
  let vscroll = Vdp.For_tests.vscroll_latch vdp in
  let _, map_h = tilemap_size m in
  let x = (256 - hscroll) land 0xFF in
  let y = vscroll mod map_h in
  x, y, 256, Vdp.For_tests.Regs.active_lines vdp
;;

(* --- sprites ------------------------------------------------------------

   "00: yyyyyyyy ... 80: xnxnxn": sprite i has its Y at base+i, and its X and
   pattern index at base+$80+2i. A Y of $D0 ends the table. *)

type sprite =
  { index : int
  ; y : int
  ; x : int
  ; tile : int
  }

let sprites m =
  let vdp = Machine.For_debug.vdp m in
  let sat = Vdp.For_tests.Regs.sprite_attr_base vdp in
  let v a = Vdp.For_tests.vram_byte vdp (a land 0x3FFF) in
  let rec go i acc =
    if i > 63
    then List.rev acc, false
    else (
      let y = v (sat + i) in
      if y = 0xD0
      then List.rev acc, true
      else
        go
          (i + 1)
          ({ index = i
           ; y
           ; x = v (sat + 0x80 + (i * 2))
           ; tile = v (sat + 0x80 + (i * 2) + 1)
           }
           :: acc))
  in
  go 0 []
;;

(* --- chip state --------------------------------------------------------- *)

type vdp_state =
  { registers : int array
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

let vdp_state m =
  let vdp = Machine.For_debug.vdp m in
  let open Vdp.For_tests in
  { registers = Array.init 11 (fun i -> register vdp i)
  ; line = line vdp
  ; display_enabled = Regs.display_enabled vdp
  ; hide_left_column = Regs.hide_left_column vdp
  ; tall_sprites = Regs.tall_sprites vdp
  ; zoom_sprites = Regs.zoom_sprites vdp
  ; shift_sprites = Regs.shift_sprites vdp
  ; name_table_base = Regs.name_table_base vdp
  ; sprite_attr_base = Regs.sprite_attr_base vdp
  ; sprite_pattern_base = Regs.sprite_pattern_base vdp
  }
;;
