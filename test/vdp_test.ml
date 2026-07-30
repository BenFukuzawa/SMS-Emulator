open Uints

(* Milestone 1: the port protocol only. Nothing here knows what a scanline or
   a pixel is. Every case below fails on a plausible first draft of the VDP,
   which is the point of writing them now rather than after the renderer
   exists. *)

let failures = ref 0

let check ~name ~expect ~actual =
  if expect = actual
  then Printf.printf "  PASS  %s\n" name
  else (
    incr failures;
    Printf.printf
      "  FAIL  %s: expected $%02X, got $%02X\n"
      name
      expect
      actual)
;;

let check_bool ~name ~expect ~actual =
  if Bool.equal expect actual
  then Printf.printf "  PASS  %s\n" name
  else (
    incr failures;
    Printf.printf "  FAIL  %s: expected %b, got %b\n" name expect actual)
;;

let group name = Printf.printf "=== %s ===\n" name

(* --- the program's-eye view of the two ports ------------------------- *)

let ctrl t byte = Vdp.write_control t (Uint8.of_int byte)
let data t byte = Vdp.write_data t (Uint8.of_int byte)
let read_data t = Vdp.read_data t |> Uint8.to_int
let read_status t = Vdp.read_status t |> Uint8.to_int

(* A command is a pair of bytes written to $BF, low half first. *)
let command t ~low ~high =
  ctrl t low;
  ctrl t high
;;

let set_read_address t addr =
  command t ~low:(addr land 0xFF) ~high:(addr lsr 8)
;;

let set_write_address t addr =
  command t ~low:(addr land 0xFF) ~high:(0x40 lor (addr lsr 8))
;;

let set_reg t n v = command t ~low:v ~high:(0x80 lor n)

(* One scanline is 228 T-states; a frame is 262 of them. Nothing below cares
   how the cycles arrive, only how many. *)
let cycles_per_line = 228
let lines_per_frame = 262

let step_lines t n =
  for _ = 1 to n do
    Vdp.step t ~cycles:cycles_per_line
  done
;;

(* Run out the frame the chip powers on in, so the line counter is holding
   R10 rather than its power-on $FF, and drop any flags that raised. *)
let settle t =
  step_lines t lines_per_frame;
  ignore (read_status t : int)
;;

(* --- registers -------------------------------------------------------- *)

let test_register_write () =
  group "register writes";
  let t = Vdp.create () in
  (* The data is the first byte of the pair, not the second. *)
  command t ~low:0x36 ~high:0x80;
  check
    ~name:"R0 takes the first byte"
    ~expect:0x36
    ~actual:(Vdp.For_tests.register t 0);
  (* Only the low nibble of the second byte selects the register; bits 5-4
     are not part of the number. *)
  command t ~low:0xA0 ~high:0xB1;
  check
    ~name:"register number is the low nibble"
    ~expect:0xA0
    ~actual:(Vdp.For_tests.register t 1);
  (* R11..R15 do not exist. This must not raise. *)
  command t ~low:0x99 ~high:0x8F;
  check
    ~name:"nonexistent register is harmless"
    ~expect:0xA0
    ~actual:(Vdp.For_tests.register t 1)
;;

(* --- VRAM writes ------------------------------------------------------ *)

let test_write_and_increment () =
  group "VRAM writes";
  let t = Vdp.create () in
  set_write_address t 0x0000;
  data t 0xAA;
  data t 0xBB;
  check ~name:"first byte" ~expect:0xAA ~actual:(Vdp.For_tests.vram_byte t 0);
  check
    ~name:"address auto-incremented"
    ~expect:0xBB
    ~actual:(Vdp.For_tests.vram_byte t 1);
  check
    ~name:"counter advanced twice"
    ~expect:2
    ~actual:(Vdp.For_tests.address t)
;;

let test_address_wraps () =
  group "address wrap";
  let t = Vdp.create () in
  (* The counter is 14 bits. Past $3FFF it comes back to zero rather than
     running off the end of VRAM. *)
  set_write_address t 0x3FFF;
  data t 0x11;
  data t 0x22;
  check
    ~name:"last byte of VRAM"
    ~expect:0x11
    ~actual:(Vdp.For_tests.vram_byte t 0x3FFF);
  check
    ~name:"wrapped to zero"
    ~expect:0x22
    ~actual:(Vdp.For_tests.vram_byte t 0x0000);
  check
    ~name:"counter wrapped too"
    ~expect:1
    ~actual:(Vdp.For_tests.address t)
;;

(* --- the prefetch ----------------------------------------------------- *)

let test_read_prefetch () =
  group "read prefetch";
  let t = Vdp.create () in
  Vdp.For_tests.set_vram_byte t ~addr:0x0100 ~data:0x5A;
  Vdp.For_tests.set_vram_byte t ~addr:0x0101 ~data:0x6B;
  set_read_address t 0x0100;
  (* Setting up a read primes the buffer, so the first read returns the byte
     the program asked for. Without that priming everything is off by one. *)
  check
    ~name:"first read is the requested byte"
    ~expect:0x5A
    ~actual:(read_data t);
  check ~name:"reads stream forwards" ~expect:0x6B ~actual:(read_data t)
;;

let test_write_feeds_read_buffer () =
  group "write feeds the read buffer";
  let t = Vdp.create () in
  set_write_address t 0x0200;
  data t 0x77;
  (* No new setup: the read hands back the byte just written, not VRAM. *)
  check
    ~name:"buffer holds the written byte"
    ~expect:0x77
    ~actual:(read_data t)
;;

(* --- CRAM ------------------------------------------------------------- *)

let test_cram_index_masks () =
  group "CRAM";
  let t = Vdp.create () in
  command t ~low:0x00 ~high:0xC0;
  for i = 0 to 32 do
    data t i
  done;
  (* The palette is 32 entries, so byte 32 lands back on entry 0 -- but the
     address counter itself is 14-bit and does not wrap with it. *)
  check
    ~name:"entry 1 written"
    ~expect:1
    ~actual:(Vdp.For_tests.cram_entry t 1);
  check
    ~name:"33rd byte wrapped onto entry 0"
    ~expect:32
    ~actual:(Vdp.For_tests.cram_entry t 0);
  check
    ~name:"counter did not wrap at 32"
    ~expect:33
    ~actual:(Vdp.For_tests.address t)
;;

(* --- the latch -------------------------------------------------------- *)

(* A half-written command is real state. Anything touching the data port or
   the status port destroys it -- this is the interrupt desync the manual
   warns about, and the reason control writes are wrapped in DI/EI. *)

let test_data_access_clears_latch () =
  group "data access clears the latch";
  let t = Vdp.create () in
  ctrl t 0x12;
  check_bool
    ~name:"pair is half written"
    ~expect:true
    ~actual:(Vdp.For_tests.latch_pending t);
  data t 0x00;
  check_bool
    ~name:"data access dropped it"
    ~expect:false
    ~actual:(Vdp.For_tests.latch_pending t);
  (* $40 must now start a fresh pair. If the latch had survived, this would
     have completed the old one and set the address to $0012. *)
  command t ~low:0x40 ~high:0x41;
  check
    ~name:"fresh pair, address $0140"
    ~expect:0x0140
    ~actual:(Vdp.For_tests.address t);
  check ~name:"write mode" ~expect:1 ~actual:(Vdp.For_tests.code t)
;;

let test_status_read_clears_latch () =
  group "status read clears the latch";
  let t = Vdp.create () in
  ctrl t 0x12;
  ignore (read_status t : int);
  check_bool
    ~name:"status read dropped it"
    ~expect:false
    ~actual:(Vdp.For_tests.latch_pending t);
  command t ~low:0x40 ~high:0x41;
  check
    ~name:"fresh pair, address $0140"
    ~expect:0x0140
    ~actual:(Vdp.For_tests.address t)
;;

(* --- status ----------------------------------------------------------- *)

let test_status_is_destructive () =
  group "status read";
  let t = Vdp.create () in
  Vdp.For_tests.set_flags t ~vblank:true ~overflow:true ~collision:true;
  Vdp.For_tests.set_line_pending t true;
  (* Low five bits are not driven and read back as ones. *)
  check ~name:"all flags set" ~expect:0xFF ~actual:(read_status t);
  check ~name:"reading cleared them" ~expect:0x1F ~actual:(read_status t);
  check_bool
    ~name:"line interrupt acknowledged too"
    ~expect:false
    ~actual:(Vdp.For_tests.line_pending t)
;;

(* --- register decoding ------------------------------------------------

   The bases come from the SMS Power VDP Registers page; the display-height
   combinations from the M1/M2/M3/M4 table in MacDonald's msvdp-20021112.txt,
   SMS2 column, translated out of his mode-bit naming into physical bits. *)

let test_table_bases () =
  group "table base addresses";
  let t = Vdp.create () in
  let open Vdp.For_tests.Regs in
  (* R2 bits 3-1 are multiplied by $800. *)
  check ~name:"R2 = $FF -> $3800" ~expect:0x3800 ~actual:(name_table_base t);
  set_reg t 2 0xF1;
  check ~name:"R2 = $F1 -> $0000" ~expect:0x0000 ~actual:(name_table_base t);
  set_reg t 2 0x0C;
  check ~name:"R2 = $0C -> $3000" ~expect:0x3000 ~actual:(name_table_base t);
  (* The sprite attribute table sits on a $100 boundary. *)
  check
    ~name:"R5 = $FF -> $3F00"
    ~expect:0x3F00
    ~actual:(sprite_attr_base t);
  set_reg t 5 0x82;
  check
    ~name:"R5 = $82 -> $0100"
    ~expect:0x0100
    ~actual:(sprite_attr_base t);
  (* Only R6 bit 2 matters: sprite patterns live at $0000 or $2000. *)
  check
    ~name:"R6 = $FB -> $0000"
    ~expect:0x0000
    ~actual:(sprite_pattern_base t);
  set_reg t 6 0xFF;
  check
    ~name:"R6 = $FF -> $2000"
    ~expect:0x2000
    ~actual:(sprite_pattern_base t);
  (* The backdrop indexes the sprite half of CRAM, so entries 16-31. *)
  set_reg t 7 0x05;
  check ~name:"R7 = $05 -> CRAM 21" ~expect:21 ~actual:(backdrop_colour t)
;;

let test_display_height () =
  group "display height";
  let t = Vdp.create () in
  let open Vdp.For_tests.Regs in
  (* (R0 bit 1, R1 bit 4, R1 bit 3) -> active lines. R0 $36 and $34 differ
     only in bit 1; R1 $A0 is the power-on value with both height bits
     clear. *)
  let case ~name ~r0 ~r1 ~expect =
    set_reg t 0 r0;
    set_reg t 1 r1;
    check ~name ~expect ~actual:(active_lines t)
  in
  case ~name:"0,0,0 -> 192" ~r0:0x34 ~r1:0xA0 ~expect:192;
  case ~name:"1,0,0 -> 192" ~r0:0x36 ~r1:0xA0 ~expect:192;
  case ~name:"1,1,0 -> 224" ~r0:0x36 ~r1:0xB0 ~expect:224;
  case ~name:"0,0,1 -> 192" ~r0:0x34 ~r1:0xA8 ~expect:192;
  case ~name:"1,0,1 -> 240" ~r0:0x36 ~r1:0xA8 ~expect:240;
  (* MacDonald's table gives the all-bits-set row as plain Mode 4. *)
  case ~name:"1,1,1 -> 192" ~r0:0x36 ~r1:0xB8 ~expect:192;
  (* The taller modes move the name table to a $1000 boundary plus $700. *)
  set_reg t 0 0x36;
  set_reg t 1 0xB0;
  set_reg t 2 0xFF;
  check
    ~name:"224-line, R2 = $FF -> $3700"
    ~expect:0x3700
    ~actual:(name_table_base t)
;;

(* --- frame timing ------------------------------------------------------ *)

let test_frame_length () =
  group "frame length";
  let t = Vdp.create () in
  step_lines t 261;
  check ~name:"261 lines is not a frame" ~expect:0 ~actual:(Vdp.frame_count t);
  step_lines t 1;
  check ~name:"262 lines is" ~expect:1 ~actual:(Vdp.frame_count t);
  check ~name:"back to line 0" ~expect:0 ~actual:(Vdp.For_tests.line t)
;;

let test_cycles_carry () =
  group "ragged cycle counts";
  let t = Vdp.create () in
  (* 262 * 228 = 59736 T-states, delivered in threes so that no chunk lands
     on a line boundary. If the leftover were dropped at each boundary
     rather than carried, the frame would come up short. *)
  for _ = 1 to 59736 / 3 do
    Vdp.step t ~cycles:3
  done;
  check ~name:"exactly one frame" ~expect:1 ~actual:(Vdp.frame_count t);
  check ~name:"no cycles left over" ~expect:0
    ~actual:(Vdp.For_tests.line_cycles t);
  (* And a single oversized chunk must cross several lines at once. *)
  let t = Vdp.create () in
  Vdp.step t ~cycles:(cycles_per_line * 5);
  check ~name:"one big chunk crosses 5 lines" ~expect:5
    ~actual:(Vdp.For_tests.line t)
;;

let test_vblank_line () =
  group "vblank";
  let t = Vdp.create () in
  step_lines t 191;
  check_bool
    ~name:"still active at line 191"
    ~expect:false
    ~actual:(Vdp.For_tests.vblank_flag t);
  step_lines t 1;
  check_bool
    ~name:"set once line 191 has finished"
    ~expect:true
    ~actual:(Vdp.For_tests.vblank_flag t)
;;

(* --- the line counter --------------------------------------------------

   MacDonald: "The VDP has a counter that is loaded with the contents of
   register $0A on every line outside of the active display period excluding
   the line after the last line of the active display period. It is
   decremented on every line within the active display period including the
   line after the last line." For 192-line NTSC: decremented on lines 0-191
   and 192, reloaded on lines 193-261. *)

let test_line_interrupt_lands_on_r10 () =
  group "line counter";
  let t = Vdp.create () in
  set_reg t 10 100;
  settle t;
  check
    ~name:"counter reloaded during blanking"
    ~expect:100
    ~actual:(Vdp.For_tests.line_counter t);
  step_lines t 100;
  check_bool
    ~name:"quiet through line 99"
    ~expect:false
    ~actual:(Vdp.For_tests.line_pending t);
  step_lines t 1;
  check_bool
    ~name:"fires as line 100 finishes"
    ~expect:true
    ~actual:(Vdp.For_tests.line_pending t);
  check
    ~name:"and reloads from R10"
    ~expect:100
    ~actual:(Vdp.For_tests.line_counter t)
;;

(* The "including the line after the last line" clause, which is invisible
   for any R10 below 192: only a counter long enough to still be running when
   the active display ends can tell line 192 apart from line 193. With R10 =
   192 the counter reaches zero at the end of line 191 and must underflow one
   line later. If line 192 reloaded instead of decrementing, this interrupt
   would never arrive at all. *)
let test_line_counter_runs_one_line_past_the_display () =
  group "line counter, one line past the display";
  let t = Vdp.create () in
  set_reg t 10 192;
  settle t;
  step_lines t 192;
  check_bool
    ~name:"quiet through the whole active display"
    ~expect:false
    ~actual:(Vdp.For_tests.line_pending t);
  step_lines t 1;
  check_bool
    ~name:"fires on line 192, the line after it"
    ~expect:true
    ~actual:(Vdp.For_tests.line_pending t)
;;

let test_line_interrupt_every_line () =
  group "line counter, R10 = 0";
  let t = Vdp.create () in
  set_reg t 10 0;
  settle t;
  step_lines t 1;
  check_bool
    ~name:"R10 = 0 fires on every line"
    ~expect:true
    ~actual:(Vdp.For_tests.line_pending t)
;;

let test_r10_write_is_not_a_reload () =
  group "R10 write does not reload";
  (* "Writing to register $0A will not immediately change the contents of the
     counter, this only occurs when the counter is reloaded." If the write
     did reload, a counter of 5 would have fired by line 6. *)
  let t = Vdp.create () in
  set_reg t 10 100;
  settle t;
  set_reg t 10 5;
  step_lines t 10;
  check_bool
    ~name:"still counting down from 100"
    ~expect:false
    ~actual:(Vdp.For_tests.line_pending t);
  check ~name:"counter is 90" ~expect:90 ~actual:(Vdp.For_tests.line_counter t)
;;

(* --- the interrupt line ------------------------------------------------ *)

let test_irq_gating () =
  group "interrupt line";
  let t = Vdp.create () in
  (* Power-on R1 is $A0, which has IE0 set. *)
  step_lines t 192;
  check_bool ~name:"vblank raises the line" ~expect:true ~actual:(Vdp.irq t);
  (* Clearing the enable drops it without touching the flag ... *)
  set_reg t 1 0x80;
  check_bool ~name:"IE0 clear drops it" ~expect:false ~actual:(Vdp.irq t);
  check_bool
    ~name:"flag itself survives"
    ~expect:true
    ~actual:(Vdp.For_tests.vblank_flag t);
  (* ... and setting it again raises it immediately, which is the case a
     cached irq decision would miss. *)
  set_reg t 1 0xA0;
  check_bool ~name:"re-enabling raises it" ~expect:true ~actual:(Vdp.irq t);
  ignore (read_status t : int);
  check_bool ~name:"status read drops it" ~expect:false ~actual:(Vdp.irq t)
;;

let test_line_irq_gating () =
  group "line interrupt line";
  let t = Vdp.create () in
  set_reg t 10 10;
  settle t;
  step_lines t 11;
  check_bool
    ~name:"pending after line 10"
    ~expect:true
    ~actual:(Vdp.For_tests.line_pending t);
  check_bool ~name:"IE1 is set at power on" ~expect:true ~actual:(Vdp.irq t);
  (* R0 $36 with bit 4 cleared. *)
  set_reg t 0 0x26;
  check_bool ~name:"IE1 clear drops it" ~expect:false ~actual:(Vdp.irq t)
;;

(* --- the V counter -----------------------------------------------------

   MacDonald's table, NTSC: 192-line is "00-DA, D5-FF" and 224-line is
   "00-EA, E5-FF". The comma is a backwards jump, and these cases pin it to
   the exact line it happens on. *)

let v t = Vdp.v_counter t |> Uint8.to_int

let test_v_counter_192 () =
  group "V counter, 192-line";
  let t = Vdp.create () in
  check ~name:"line 0" ~expect:0x00 ~actual:(v t);
  step_lines t 218;
  check ~name:"line 218 is the last before the jump" ~expect:0xDA ~actual:(v t);
  step_lines t 1;
  check ~name:"line 219 jumps back to $D5" ~expect:0xD5 ~actual:(v t);
  step_lines t 42;
  check ~name:"line 261 is the last of the frame" ~expect:0xFF ~actual:(v t);
  step_lines t 1;
  check ~name:"and the next frame restarts at $00" ~expect:0x00 ~actual:(v t)
;;

let test_v_counter_224 () =
  group "V counter, 224-line";
  let t = Vdp.create () in
  set_reg t 0 0x36;
  set_reg t 1 0xB0;
  step_lines t 234;
  check ~name:"line 234" ~expect:0xEA ~actual:(v t);
  step_lines t 1;
  check ~name:"line 235 jumps back to $E5" ~expect:0xE5 ~actual:(v t);
  step_lines t 26;
  check ~name:"line 261" ~expect:0xFF ~actual:(v t)
;;

(* Every line of a frame must map to some segment; the fallback in v_counter
   is only unreachable while the tables sum to at least 262. *)
let test_v_counter_covers_the_frame () =
  group "V counter coverage";
  let t = Vdp.create () in
  let last = ref (-1) in
  let gaps = ref 0 in
  for _ = 1 to lines_per_frame do
    let value = v t in
    (* Values step by one except across the single documented jump. *)
    if !last >= 0 && value <> (!last + 1) land 0xFF then incr gaps;
    last := value;
    step_lines t 1
  done;
  check ~name:"exactly one jump per frame" ~expect:1 ~actual:!gaps
;;

(* --- background rendering ----------------------------------------------

   Scenes are built directly in VRAM and one line is rendered on demand, so
   every assertion below is on a colour index the chip computed rather than
   on a picture. The layouts are MacDonald's: a name table entry is
   "---pcvhnnnnnnnnn", and "each pattern uses 32 bytes. The first four bytes
   are bitplanes 0 through 3 for line 0". *)

let name_table = 0x3800

(* Three tiles that are easy to tell apart in an assertion. *)
let ramp_x = Array.init 8 (fun _ -> Array.init 8 (fun x -> x)) (* 0-7 across *)
let ramp_y = Array.init 8 (fun y -> Array.init 8 (fun _ -> y)) (* 0-7 down *)
let high_x = Array.init 8 (fun _ -> Array.init 8 (fun x -> 8 + x)) (* 8-15 *)

(* rows.(y).(x) is the colour index of pixel x on line y. Split it back into
   the four bitplanes the chip stores. *)
let put_tile t ~index rows =
  Array.iteri
    (fun y row ->
      for plane = 0 to 3 do
        let byte = ref 0 in
        Array.iteri
          (fun x colour ->
            if (colour lsr plane) land 1 = 1
            then byte := !byte lor (1 lsl (7 - x)))
          row;
        Vdp.For_tests.set_vram_byte
          t
          ~addr:((index * 32) + (y * 4) + plane)
          ~data:!byte
      done)
    rows
;;

let put_entry
  t
  ~row
  ~col
  ~tile
  ?(hflip = false)
  ?(vflip = false)
  ?(palette = 0)
  ?(priority = false)
  ()
  =
  let addr = name_table + (((row * 32) + col) * 2) in
  let hi =
    ((tile lsr 8) land 1)
    lor (if hflip then 0x02 else 0)
    lor (if vflip then 0x04 else 0)
    lor (palette lsl 3)
    lor if priority then 0x10 else 0
  in
  Vdp.For_tests.set_vram_byte t ~addr ~data:(tile land 0xFF);
  Vdp.For_tests.set_vram_byte t ~addr:(addr + 1) ~data:hi
;;

let fill_row t ~row ~tile =
  for col = 0 to 31 do
    put_entry t ~row ~col ~tile ()
  done
;;

let fill_column t ~col ~tile =
  for row = 0 to 27 do
    put_entry t ~row ~col ~tile ()
  done
;;

(* R0 $06: mode 4, M3 set, no scroll locks, left column shown. R1 $E0:
   display enabled, 192 lines. The power-on values have the left column
   masked and the display off, which would swallow most of these tests. *)
let scene () =
  let t = Vdp.create () in
  set_reg t 0 0x06;
  set_reg t 1 0xE0;
  set_reg t 2 0xFF;
  put_tile t ~index:1 ramp_x;
  put_tile t ~index:2 ramp_y;
  put_tile t ~index:3 high_x;
  t
;;

let render t ~line = Vdp.For_tests.render_line t ~line
let px t x = Vdp.For_tests.bg_index t x

let test_bitplane_decode () =
  group "tile bitplanes";
  let t = scene () in
  put_entry t ~row:0 ~col:0 ~tile:1 ();
  put_entry t ~row:0 ~col:1 ~tile:3 ();
  render t ~line:0;
  check ~name:"pixel 0 of a ramp" ~expect:0 ~actual:(px t 0);
  check ~name:"pixel 5 of a ramp" ~expect:5 ~actual:(px t 5);
  check ~name:"pixel 7 of a ramp" ~expect:7 ~actual:(px t 7);
  (* Colours 8-15 only appear if bitplane 3 is being read at all. *)
  check ~name:"bitplane 3 reaches colour 8" ~expect:8 ~actual:(px t 8);
  check ~name:"and colour 15" ~expect:15 ~actual:(px t 15);
  (* An unset entry is tile 0, which is all zeroes. *)
  check ~name:"empty tile is colour 0" ~expect:0 ~actual:(px t 20)
;;

(* "n = Pattern index, any one of 512 patterns in VRAM can be selected." The
   ninth bit lives in the entry's high byte, so a tile above 255 is the only
   thing that exercises it. Tile 44 -- what 300 becomes if that bit is
   dropped -- is deliberately left blank. *)
let test_tile_index_is_nine_bits () =
  group "nine-bit tile index";
  let t = scene () in
  put_tile t ~index:300 ramp_y;
  put_entry t ~row:0 ~col:0 ~tile:300 ();
  render t ~line:5;
  check ~name:"tile 300, row 5" ~expect:5 ~actual:(px t 0);
  render t ~line:2;
  check ~name:"tile 300, row 2" ~expect:2 ~actual:(px t 0)
;;

let test_flips () =
  group "tile flips";
  let t = scene () in
  put_entry t ~row:0 ~col:0 ~tile:1 ~hflip:true ();
  render t ~line:0;
  check ~name:"hflip reverses the row" ~expect:7 ~actual:(px t 0);
  check ~name:"hflip, other end" ~expect:0 ~actual:(px t 7);
  (* ramp_y differs per row, so vflip is visible where hflip would not be. *)
  let t = scene () in
  put_entry t ~row:0 ~col:0 ~tile:2 ();
  render t ~line:0;
  check ~name:"line 0 without vflip" ~expect:0 ~actual:(px t 0);
  put_entry t ~row:0 ~col:0 ~tile:2 ~vflip:true ();
  render t ~line:0;
  check ~name:"vflip reads row 7 instead" ~expect:7 ~actual:(px t 0)
;;

let test_palette_and_priority () =
  group "palette and priority bits";
  let t = scene () in
  (* c and p are adjacent bits, so setting both on one entry cannot tell
     them apart -- each tile below carries exactly one of them. *)
  put_entry t ~row:0 ~col:0 ~tile:1 ~palette:1 ~priority:false ();
  put_entry t ~row:0 ~col:1 ~tile:1 ~palette:0 ~priority:true ();
  put_entry t ~row:0 ~col:2 ~tile:1 ~palette:0 ~priority:false ();
  render t ~line:0;
  check ~name:"palette bit alone" ~expect:1
    ~actual:(Vdp.For_tests.bg_palette t 0);
  check_bool
    ~name:"and it is not the priority bit"
    ~expect:false
    ~actual:(Vdp.For_tests.bg_priority t 0);
  check_bool
    ~name:"priority bit alone"
    ~expect:true
    ~actual:(Vdp.For_tests.bg_priority t 8);
  check ~name:"and it is not the palette bit" ~expect:0
    ~actual:(Vdp.For_tests.bg_palette t 8);
  check ~name:"neither set" ~expect:0
    ~actual:(Vdp.For_tests.bg_palette t 16);
  check_bool
    ~name:"neither set, priority"
    ~expect:false
    ~actual:(Vdp.For_tests.bg_priority t 16)
;;

(* --- scrolling ---------------------------------------------------------

   "Register $08 can be divided into two parts, the upper five bits are the
   starting column, and the lower three bits are the fine scroll value. The
   starting column value gives the first column in the name table to use,
   calculated by subtracting it from the value 32." *)

let test_hscroll_whole_columns () =
  group "horizontal scroll, whole columns";
  let t = scene () in
  fill_column t ~col:0 ~tile:1;
  fill_column t ~col:31 ~tile:3;
  set_reg t 8 8;
  render t ~line:0;
  (* Column 31 has been pulled round to the left edge ... *)
  check ~name:"x 0 is column 31" ~expect:8 ~actual:(px t 0);
  check ~name:"x 7 is column 31" ~expect:15 ~actual:(px t 7);
  (* ... and column 0 now starts eight pixels in. *)
  check ~name:"x 8 is column 0" ~expect:0 ~actual:(px t 8);
  check ~name:"x 15 is column 0" ~expect:7 ~actual:(px t 15)
;;

let test_hscroll_fine () =
  group "horizontal scroll, fine";
  let t = scene () in
  fill_column t ~col:0 ~tile:1;
  fill_column t ~col:31 ~tile:3;
  (* Three is the case a column-at-a-time renderer gets wrong: the boundary
     between two source columns falls inside a screen column. *)
  set_reg t 8 3;
  render t ~line:0;
  check ~name:"x 0 is column 31, pixel 5" ~expect:13 ~actual:(px t 0);
  check ~name:"x 2 is column 31, pixel 7" ~expect:15 ~actual:(px t 2);
  check ~name:"x 3 is column 0, pixel 0" ~expect:0 ~actual:(px t 3);
  check ~name:"x 10 is column 0, pixel 7" ~expect:7 ~actual:(px t 10)
;;

(* "The vertical scroll value cannot be changed during the active display
   period, any changes made will be stored in a temporary location and used
   only when the active display period ends." *)

let test_vscroll_is_latched () =
  group "vertical scroll is latched";
  let t = scene () in
  fill_row t ~row:0 ~tile:2;
  set_reg t 9 3;
  render t ~line:0;
  check ~name:"write alone changes nothing" ~expect:0 ~actual:(px t 0);
  check ~name:"latch still holds 0" ~expect:0
    ~actual:(Vdp.For_tests.vscroll_latch t);
  step_lines t lines_per_frame;
  check ~name:"a frame boundary picks it up" ~expect:3
    ~actual:(Vdp.For_tests.vscroll_latch t);
  render t ~line:0;
  check ~name:"and line 0 now shows tile row 3" ~expect:3 ~actual:(px t 0)
;;

let test_vscroll_wraps_at_224 () =
  group "vertical scroll wraps past 223";
  let t = scene () in
  fill_row t ~row:0 ~tile:1 (* colours run across *);
  fill_row t ~row:27 ~tile:3 (* colours 8-15 *);
  set_reg t 9 216;
  step_lines t lines_per_frame;
  (* 7 + 216 = 223, the last row of a 28-row tilemap. *)
  render t ~line:7;
  check ~name:"line 7 is map row 27" ~expect:11 ~actual:(px t 3);
  (* 8 + 216 = 224, which wraps to 0 rather than running off the table. *)
  render t ~line:8;
  check ~name:"line 8 wraps to map row 0" ~expect:3 ~actual:(px t 3)
;;

(* --- the register 0 lock bits ------------------------------------------ *)

let test_hscroll_lock () =
  group "horizontal scroll lock";
  let t = scene () in
  fill_column t ~col:0 ~tile:1;
  fill_column t ~col:31 ~tile:3;
  set_reg t 0 0x46 (* bit 6 *);
  set_reg t 8 8;
  (* "horizontal scrolling will be fixed at zero for scanlines zero
     through 15" *)
  render t ~line:15;
  check ~name:"line 15 ignores R8" ~expect:3 ~actual:(px t 3);
  render t ~line:16;
  check ~name:"line 16 does not" ~expect:11 ~actual:(px t 3)
;;

let test_vscroll_lock () =
  group "vertical scroll lock";
  let t = scene () in
  fill_row t ~row:0 ~tile:1;
  fill_row t ~row:1 ~tile:3;
  set_reg t 0 0x86 (* bit 7 *);
  set_reg t 9 8;
  step_lines t lines_per_frame;
  render t ~line:0;
  (* "the vertical scroll value will be fixed to zero when columns 24 to 31
     are rendered" -- columns 24-31 are pixels 192-255. *)
  check ~name:"column 0 scrolls to map row 1" ~expect:11 ~actual:(px t 3);
  check ~name:"column 23 still scrolls" ~expect:11 ~actual:(px t 187);
  check ~name:"column 24 is pinned to row 0" ~expect:3 ~actual:(px t 195);
  check ~name:"column 31 too" ~expect:3 ~actual:(px t 251)
;;

let test_hide_left_column () =
  group "mask column 0";
  let t = scene () in
  fill_row t ~row:0 ~tile:1;
  set_reg t 0 0x26 (* bit 5 *);
  set_reg t 7 0x05;
  render t ~line:0;
  (* "1 = Mask column 0 with overscan color from register #7", and register
     7 indexes the sprite half of CRAM. *)
  check ~name:"x 0 is the overscan colour" ~expect:5 ~actual:(px t 0);
  check ~name:"from the sprite palette" ~expect:1
    ~actual:(Vdp.For_tests.bg_palette t 0);
  check ~name:"x 7 too" ~expect:5 ~actual:(px t 7);
  check ~name:"x 8 is the tilemap again" ~expect:0 ~actual:(px t 8);
  check ~name:"x 11" ~expect:3 ~actual:(px t 11)
;;

let test_display_disabled () =
  group "display disabled";
  let t = scene () in
  fill_row t ~row:0 ~tile:1;
  set_reg t 1 0xA0 (* bit 6 clear *);
  set_reg t 7 0x07;
  render t ~line:0;
  check ~name:"left edge is backdrop" ~expect:7 ~actual:(px t 0);
  check ~name:"middle is backdrop" ~expect:7 ~actual:(px t 128);
  check ~name:"right edge is backdrop" ~expect:7 ~actual:(px t 255);
  check_bool
    ~name:"nothing has priority"
    ~expect:false
    ~actual:(Vdp.For_tests.bg_priority t 128)
;;

let () =
  test_register_write ();
  test_write_and_increment ();
  test_address_wraps ();
  test_read_prefetch ();
  test_write_feeds_read_buffer ();
  test_cram_index_masks ();
  test_data_access_clears_latch ();
  test_status_read_clears_latch ();
  test_status_is_destructive ();
  test_table_bases ();
  test_display_height ();
  test_frame_length ();
  test_cycles_carry ();
  test_vblank_line ();
  test_line_interrupt_lands_on_r10 ();
  test_line_counter_runs_one_line_past_the_display ();
  test_line_interrupt_every_line ();
  test_r10_write_is_not_a_reload ();
  test_irq_gating ();
  test_line_irq_gating ();
  test_v_counter_192 ();
  test_v_counter_224 ();
  test_v_counter_covers_the_frame ();
  test_bitplane_decode ();
  test_tile_index_is_nine_bits ();
  test_flips ();
  test_palette_and_priority ();
  test_hscroll_whole_columns ();
  test_hscroll_fine ();
  test_vscroll_is_latched ();
  test_vscroll_wraps_at_224 ();
  test_hscroll_lock ();
  test_vscroll_lock ();
  test_hide_left_column ();
  test_display_disabled ();
  print_newline ();
  if !failures = 0
  then print_endline "all VDP port tests passed"
  else Printf.printf "%d failure(s)\n" !failures;
  exit (if !failures = 0 then 0 else 1)
;;
