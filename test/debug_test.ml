(* The debug view's derivations, without a browser.

   Everything [Debug] produces is a description of a machine, and a
   description that quietly stops matching what it describes is worse than
   none at all. So these tests are mostly agreements: the disassembly agrees
   with the bytes the ROM was assembled from, [step] agrees with [run_frame],
   and the tile decoder agrees with the VDP's own renderer about what a
   pattern looks like. *)

let assemble program = List.concat_map snd program

let rom_of segments =
  let b = Bytes.make 0x8000 '\000' in
  List.iter
    (fun (addr, program) ->
      List.iteri
        (fun i v -> Bytes.set b (addr + i) (Char.chr v))
        (assemble program))
    segments;
  b
;;

let failures = ref 0

let check name ~expect ~got =
  if String.equal expect got
  then Printf.printf "  PASS  %s\n" name
  else (
    incr failures;
    Printf.printf
      "  FAIL  %s\n        expected %S\n        got      %S\n"
      name
      expect
      got)
;;

let check_int name ~expect ~got =
  check name ~expect:(string_of_int expect) ~got:(string_of_int got)
;;

let check_bool name ~expect ~got =
  check name ~expect:(string_of_bool expect) ~got:(string_of_bool got)
;;

(* --- 1. the listing matches the source ---------------------------------- *)

(* Chosen for the cases the plain [Instruction.show] gets wrong for a
   listing: a forward relative jump, a backward one, and a port. *)
let listing_program =
  [ "ld hl,$c100", [ 0x21; 0x00; 0xC1 ]
  ; "ld b,$20", [ 0x06; 0x20 ]
  ; "ld a,(hl)", [ 0x7E ]
  ; "cp $d0", [ 0xFE; 0xD0 ]
  ; "jr z,+8", [ 0x28; 0x08 ]
  ; "inc hl", [ 0x23 ] (* $000B + 2 - 8 = $0005, back to the LD A,(HL). *)
  ; "djnz -8", [ 0x10; 0xF8 ]
  ; "out ($be),a", [ 0xD3; 0xBE ]
  ; "ret", [ 0xC9 ]
  ]
;;

let test_listing () =
  print_endline "=== disassembly agrees with the assembled bytes ===";
  let m = Machine.create ~rom:(rom_of [ 0x0000, listing_program ]) in
  let lines = Debug.disassemble m ~at:0x0000 ~count:9 in
  let texts = List.map (fun (l : Debug.line) -> l.text) lines in
  let expect =
    [ "LD HL, $C100"
    ; "LD B, $20"
    ; "LD A, (HL)"
    ; "CP $D0"
      (* The jump sits at $0008 and is two bytes long, so +8 lands on $0012.
         This is the whole reason [Debug] renders its own operands: the raw
         displacement tells a reader nothing about where control goes, and
         computing it by hand is exactly the error this test caught while it
         was being written. *)
    ; "JR Z, $0012"
    ; "INC HL"
    ; "DJNZ $0005" (* $BE, not 190. *)
    ; "OUT ($BE), A"
    ; "RET"
    ]
  in
  List.iter2
    (fun e g -> check (Printf.sprintf "%-14s" e) ~expect:e ~got:g)
    expect
    texts;
  (* Addresses advance by each instruction's own length. *)
  let addrs = List.map (fun (l : Debug.line) -> l.addr) lines in
  check
    "addresses"
    ~expect:"0 3 5 6 8 10 11 13 15"
    ~got:(String.concat " " (List.map string_of_int addrs))
;;

(* --- 2. undecodable bytes do not raise ---------------------------------- *)

(* $DD is an index prefix; on its own at the end of memory it leads the
   decoder somewhere it was never meant to go. A listing has to survive that
   -- walking into data is the normal case, not the exceptional one. *)
let test_data_bytes () =
  print_endline "\n=== bytes that are not instructions ===";
  let rom = rom_of [ 0x0000, [ "db $ed,$ff", [ 0xED; 0xFF ] ] ] in
  let m = Machine.create ~rom in
  let lines = Debug.disassemble m ~at:0x0000 ~count:3 in
  check_bool
    "walk of 3 returned 3 lines"
    ~expect:true
    ~got:(List.length lines = 3);
  let first = List.hd lines in
  check_bool
    "an undecodable lead byte is flagged as data"
    ~expect:true
    ~got:first.Debug.is_data;
  check "and is rendered as a byte" ~expect:"DB $ED" ~got:first.Debug.text;
  check_int
    "and advances exactly one byte"
    ~expect:0x0001
    ~got:(List.nth lines 1).Debug.addr
;;

(* --- 3. step agrees with run_frame -------------------------------------- *)

(* [run_frame] is now a loop over [step], so a frame's worth of steps must
   land in the same place a frame does. If these ever disagree, stepping in
   the debugger is not showing the machine the game runs on. *)
let spin = [ "jr $", [ 0x18; 0xFE ] ]

let test_step () =
  print_endline "\n=== step and run_frame are the same machine ===";
  let a = Machine.create ~rom:(rom_of [ 0x0000, spin ]) in
  let b = Machine.create ~rom:(rom_of [ 0x0000, spin ]) in
  Machine.run_frame a;
  let steps = ref 0 in
  while Machine.frame_count b = 0 && !steps < 200_000 do
    ignore (Machine.step b : int);
    incr steps
  done;
  check_int
    "same frame count"
    ~expect:(Machine.frame_count a)
    ~got:(Machine.frame_count b);
  check_int
    "same PC"
    ~expect:(Machine.For_debug.pc a)
    ~got:(Machine.For_debug.pc b);
  (* A scanline is a smaller unit than a frame and a larger one than an
     instruction; it should land inside the frame, not past it. *)
  let c = Machine.create ~rom:(rom_of [ 0x0000, spin ]) in
  Machine.step_scanline c;
  check_bool
    "one scanline stays within the frame"
    ~expect:true
    ~got:(Machine.frame_count c = 0)
;;

(* --- 4. the tile decoder agrees with the renderer ------------------------ *)

(* Writes one pattern and one palette through the ports, then compares what
   [Debug.tile_sheet] makes of that pattern against what the VDP's own
   renderer puts in the framebuffer for it. The two decoders are separate
   code; this is what stops them drifting. *)
let pattern_program =
  (* Palette entries 0-3 of the background palette, then pattern 0: eight
     rows whose four bitplanes give colour index 0,1,2,3,0,1,2,3 across. *)
  [ "di", [ 0xF3 ]
  ; "ld a,$00 ; CRAM 0", [ 0x3E; 0x00 ]
  ; "out ($bf),a", [ 0xD3; 0xBF ]
  ; "ld a,$c0 ; CRAM write", [ 0x3E; 0xC0 ]
  ; "out ($bf),a", [ 0xD3; 0xBF ]
  ; "ld a,$00 ; black", [ 0x3E; 0x00 ]
  ; "out ($be),a", [ 0xD3; 0xBE ]
  ; "ld a,$03 ; red", [ 0x3E; 0x03 ]
  ; "out ($be),a", [ 0xD3; 0xBE ]
  ; "ld a,$0c ; green", [ 0x3E; 0x0C ]
  ; "out ($be),a", [ 0xD3; 0xBE ]
  ; "ld a,$30 ; blue", [ 0x3E; 0x30 ]
  ; "out ($be),a", [ 0xD3; 0xBE ] (* VRAM address 0, write *)
  ; "ld a,$00", [ 0x3E; 0x00 ]
  ; "out ($bf),a", [ 0xD3; 0xBF ]
  ; "ld a,$40", [ 0x3E; 0x40 ]
  ; "out ($bf),a", [ 0xD3; 0xBF ]
    (* Eight identical rows: plane0 = 01010101, plane1 = 00110011. That is
       index 0,1,2,3 repeating across the eight pixels. *)
  ; "ld b,$08", [ 0x06; 0x08 ]
  ; "ld a,$55 ; plane 0", [ 0x3E; 0x55 ]
  ; "out ($be),a", [ 0xD3; 0xBE ]
  ; "ld a,$33 ; plane 1", [ 0x3E; 0x33 ]
  ; "out ($be),a", [ 0xD3; 0xBE ]
  ; "xor a ; plane 2", [ 0xAF ]
  ; "out ($be),a", [ 0xD3; 0xBE ]
  ; "out ($be),a ; plane 3", [ 0xD3; 0xBE ]
    (* The loop body is 15 bytes, so the branch back is -15. *)
  ; "djnz -15", [ 0x10; 0xF1 ]
  ; "jr $", [ 0x18; 0xFE ]
  ]
;;

let test_pattern () =
  print_endline "\n=== the tile decoder agrees with the VDP ===";
  let m = Machine.create ~rom:(rom_of [ 0x0000, pattern_program ]) in
  Machine.run_frame m;
  let sheet = Debug.tile_sheet m ~palette:0 in
  let w, _ = Debug.tile_sheet_size in
  let px x y =
    let at = ((y * w) + x) * 3 in
    Printf.sprintf
      "%d,%d,%d"
      (Char.code (Bytes.get sheet at))
      (Char.code (Bytes.get sheet (at + 1)))
      (Char.code (Bytes.get sheet (at + 2)))
  in
  (* $03 = --BBGGRR with RR = 3, so full red; $0C is full green; $30 full
     blue. Index 0 stayed black. *)
  check "tile 0 pixel 0 is palette 0 (black)" ~expect:"0,0,0" ~got:(px 0 0);
  check "tile 0 pixel 1 is palette 1 (red)" ~expect:"255,0,0" ~got:(px 1 0);
  check "tile 0 pixel 2 is palette 2 (green)" ~expect:"0,255,0" ~got:(px 2 0);
  check "tile 0 pixel 3 is palette 3 (blue)" ~expect:"0,0,255" ~got:(px 3 0);
  check
    "and the pattern repeats across the row"
    ~expect:"255,0,0"
    ~got:(px 5 0);
  check "and holds down the tile" ~expect:"0,0,255" ~got:(px 3 7);
  (* Against the chip's own palette expansion, not just against itself. *)
  check_int
    "cram_rgb matches the CRAM entry"
    ~expect:0xFF0000
    ~got:(Debug.cram_rgb m 1)
;;

(* --- 5. the sprite table stops at the terminator ------------------------- *)

let test_sprites () =
  print_endline "\n=== the sprite table ===";
  let m = Machine.create ~rom:(rom_of [ 0x0000, spin ]) in
  Machine.run_frame m;
  let vdp = Machine.For_debug.vdp m in
  let sat = Vdp.For_tests.Regs.sprite_attr_base vdp in
  (* Three sprites, then the $D0 that ends the table, then junk that must not
     be reported. *)
  List.iteri
    (fun i (y, x, tile) ->
      Vdp.For_tests.set_vram_byte vdp ~addr:(sat + i) ~data:y;
      Vdp.For_tests.set_vram_byte vdp ~addr:(sat + 0x80 + (i * 2)) ~data:x;
      Vdp.For_tests.set_vram_byte
        vdp
        ~addr:(sat + 0x80 + (i * 2) + 1)
        ~data:tile)
    [ 96, 104, 0x40; 96, 112, 0x41; 88, 204, 0x7C ];
  Vdp.For_tests.set_vram_byte vdp ~addr:(sat + 3) ~data:0xD0;
  Vdp.For_tests.set_vram_byte vdp ~addr:(sat + 4) ~data:0x20;
  let list, terminated = Debug.sprites m in
  check_int
    "three sprites before the terminator"
    ~expect:3
    ~got:(List.length list);
  check_bool "and the terminator was found" ~expect:true ~got:terminated;
  let s = List.nth list 2 in
  check
    "the third sprite reads back"
    ~expect:"2 88 204 124"
    ~got:
      (Printf.sprintf
         "%d %d %d %d"
         s.Debug.index
         s.Debug.y
         s.Debug.x
         s.Debug.tile)
;;

(* --- 6. reading does not disturb the machine ---------------------------- *)

(* The point of [Machine.For_debug] being read-only. A panel that changed the
   run it was describing would be a bug no test of the emulator itself would
   catch. *)
let test_read_only () =
  print_endline "\n=== inspection does not perturb the run ===";
  let a = Machine.create ~rom:(rom_of [ 0x0000, listing_program ]) in
  let b = Machine.create ~rom:(rom_of [ 0x0000, listing_program ]) in
  for _ = 1 to 3 do
    Machine.run_frame a;
    Machine.run_frame b;
    (* Everything the debug view does, every frame, to b only. *)
    ignore (Debug.disassemble b ~at:(Machine.For_debug.pc b) ~count:16);
    ignore (Debug.tile_sheet b ~palette:0 : Bytes.t);
    ignore (Debug.tilemap b : Bytes.t);
    ignore (Debug.sprites b);
    ignore (Debug.cram b : int array);
    ignore (Debug.vdp_state b)
  done;
  check_int
    "same PC"
    ~expect:(Machine.For_debug.pc a)
    ~got:(Machine.For_debug.pc b);
  check_int
    "same frame count"
    ~expect:(Machine.frame_count a)
    ~got:(Machine.frame_count b);
  check_bool
    "same picture"
    ~expect:true
    ~got:(Bytes.equal (Machine.framebuffer a) (Machine.framebuffer b))
;;

let () =
  test_listing ();
  test_data_bytes ();
  test_step ();
  test_pattern ();
  test_sprites ();
  test_read_only ();
  if !failures = 0
  then print_endline "\nall debug checks passed"
  else (
    Printf.printf "\n%d check(s) failed\n" !failures;
    exit 1)
;;
