(* Drives the whole console from hand-assembled ROMs.

   Every other test builds one chip and pokes it directly. This one only ever
   calls [Machine], so the path it covers is the assembled machine: the CPU
   fetches instructions from the cartridge through the memory bus, its port
   instructions reach the VDP through the I/O bus and the port adapter, and
   the frame loop clocks the two against each other. *)

(* A program is (mnemonic, bytes) pairs. The mnemonic is data rather than a
   comment so that nothing can drift between an instruction and its
   disassembly, and so a failing test can print the listing. *)
let assemble program = List.concat_map snd program

(* Two 16 KB banks, so the mapper has something well-formed to page. *)
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
  if expect = got
  then Printf.printf "  PASS  %s\n" name
  else (
    incr failures;
    Printf.printf "  FAIL  %s: expected %d, got %d\n" name expect got)
;;

let check_between name ~low ~high ~got =
  if got >= low && got <= high
  then Printf.printf "  PASS  %s (%d)\n" name got
  else (
    incr failures;
    Printf.printf "  FAIL  %s: expected %d..%d, got %d\n" name low high got)
;;

(* --- 1. a picture ------------------------------------------------------- *)

(* Leaves the display off, which makes every pixel the backdrop colour -- the
   simplest output a ROM can produce that proves the whole chain ran. The
   backdrop is sprite-palette entry R7 & $0F, i.e. CRAM 16 here. *)
let backdrop_program =
  [ "di", [ 0xF3 ]
  ; "ld a,$10 ; CRAM address 16", [ 0x3E; 0x10 ]
  ; "out ($bf),a", [ 0xD3; 0xBF ]
  ; "ld a,$c0 ; code 3 = CRAM write", [ 0x3E; 0xC0 ]
  ; "out ($bf),a", [ 0xD3; 0xBF ]
  ; "ld a,$30 ; --BBGGRR = blue", [ 0x3E; 0x30 ]
  ; "out ($be),a", [ 0xD3; 0xBE ]
  ; "ld a,$00 ; R7 value", [ 0x3E; 0x00 ]
  ; "out ($bf),a", [ 0xD3; 0xBF ]
  ; "ld a,$87 ; -> register 7", [ 0x3E; 0x87 ]
  ; "out ($bf),a", [ 0xD3; 0xBF ]
  ; "jr $ ; spin forever", [ 0x18; 0xFE ]
  ]
;;

let test_backdrop () =
  let m = Machine.create ~rom:(rom_of [ 0x0000, backdrop_program ]) in
  print_endline "=== one frame from a hand-assembled ROM ===";
  Machine.run_frame m;
  check "a frame completed" ~expect:1 ~got:(Machine.frame_count m);
  let w, h = Machine.frame_size m in
  check "width" ~expect:256 ~got:w;
  check "height" ~expect:192 ~got:h;
  let fb = Machine.framebuffer m in
  let pixel i =
    ( Char.code (Bytes.get fb (i * 3))
    , Char.code (Bytes.get fb ((i * 3) + 1))
    , Char.code (Bytes.get fb ((i * 3) + 2)) )
  in
  print_endline "=== backdrop written by the ROM ===";
  let r, g, b = pixel 0 in
  check "top-left red" ~expect:0 ~got:r;
  check "top-left green" ~expect:0 ~got:g;
  check "top-left blue" ~expect:255 ~got:b;
  let r, g, b = pixel ((w * h) - 1) in
  check "bottom-right red" ~expect:0 ~got:r;
  check "bottom-right green" ~expect:0 ~got:g;
  check "bottom-right blue" ~expect:255 ~got:b;
  print_endline "=== the loop keeps running ===";
  for _ = 1 to 60 do
    Machine.run_frame m
  done;
  check "61 frames" ~expect:61 ~got:(Machine.frame_count m)
;;

(* --- 2. interrupts ------------------------------------------------------ *)

(* The wiring most likely to be subtly wrong: the VDP raises its frame
   interrupt, the CPU has to take it, and the handler has to be able to clear
   it by reading the status port. If the line were latched rather than
   polled, or never dropped on acknowledgement, this counter would stay at
   zero or run away.

   SP is set before EI on purpose. It powers up at zero, so an interrupt
   would push onto $FFFF/$FFFE -- which are mapper registers, meaning taking
   one would silently page the cartridge out from under the program. *)
let irq_main =
  [ "di", [ 0xF3 ]
  ; "ld sp,$dff0", [ 0x31; 0xF0; 0xDF ]
  ; "im 1", [ 0xED; 0x56 ]
  ; "xor a", [ 0xAF ]
  ; "ld ($c000),a ; counter = 0", [ 0x32; 0x00; 0xC0 ]
  ; "ld a,$60 ; display on + frame irq", [ 0x3E; 0x60 ]
  ; "out ($bf),a", [ 0xD3; 0xBF ]
  ; "ld a,$81 ; -> register 1", [ 0x3E; 0x81 ]
  ; "out ($bf),a", [ 0xD3; 0xBF ]
  ; "ei", [ 0xFB ]
  ; "jr $ ; spin forever", [ 0x18; 0xFE ]
  ]
;;

(* IM 1 vectors every maskable interrupt to $0038. *)
let irq_handler =
  [ "in a,($bf) ; read status = acknowledge", [ 0xDB; 0xBF ]
  ; "ld a,($c000)", [ 0x3A; 0x00; 0xC0 ]
  ; "inc a", [ 0x3C ]
  ; "ld ($c000),a", [ 0x32; 0x00; 0xC0 ]
  ; "ei", [ 0xFB ]
  ; "reti", [ 0xED; 0x4D ]
  ]
;;

let test_interrupts () =
  print_endline "=== VDP frame interrupt reaches the CPU ===";
  let rom = rom_of [ 0x0000, irq_main; 0x0038, irq_handler ] in
  let m = Machine.create ~rom in
  let counter () = Machine.For_tests.read_byte m 0xC000 in
  Machine.run_frame m;
  check "one frame, one interrupt" ~expect:1 ~got:(counter ());
  for _ = 1 to 9 do
    Machine.run_frame m
  done;
  check "ten frames, ten interrupts" ~expect:10 ~got:(counter ());
  check "frames agree" ~expect:10 ~got:(Machine.frame_count m)
;;

(* --- 3. the controller -------------------------------------------------

   The joypad is the only chip a ROM can observe that the *host* drives, so
   it is the one path the other tests cannot reach: everything else is set in
   motion by the ROM itself. This closes the loop -- Machine.press, through
   the joypad, out port $DC, into a register, into RAM, back out through
   For_tests.read_byte.

   The program polls both controller ports forever and leaves the last value
   it saw in RAM, so the test can press a button between frames and look.

   The loop body is twelve bytes and JR counts from the instruction after
   itself, so the displacement back to the first IN is -12 = $F4. *)
let poll_pads =
  [ "di", [ 0xF3 ]
  ; "in a,($dc) ; controller port A", [ 0xDB; 0xDC ]
  ; "ld ($c000),a", [ 0x32; 0x00; 0xC0 ]
  ; "in a,($dd) ; controller port B", [ 0xDB; 0xDD ]
  ; "ld ($c001),a", [ 0x32; 0x01; 0xC0 ]
  ; "jr -12 ; poll forever", [ 0x18; 0xF4 ]
  ]
;;

let test_joypad () =
  print_endline "=== the host's button presses reach the ROM ===";
  let m = Machine.create ~rom:(rom_of [ 0x0000, poll_pads ]) in
  let port_a () = Machine.For_tests.read_byte m 0xC000 in
  let port_b () = Machine.For_tests.read_byte m 0xC001 in
  Machine.run_frame m;
  (* Active low, so nothing pressed reads all ones. *)
  check "idle port A" ~expect:0xFF ~got:(port_a ());
  check "idle port B" ~expect:0xFF ~got:(port_b ());
  (* Player 1 sits entirely in port A. *)
  Machine.press m Machine.One Machine.Up;
  Machine.run_frame m;
  check "P1 Up clears A bit 0" ~expect:0xFE ~got:(port_a ());
  Machine.press m Machine.One Machine.Button2;
  Machine.run_frame m;
  check "and Button2 clears bit 5" ~expect:0xDE ~got:(port_a ());
  Machine.release m Machine.One Machine.Up;
  Machine.run_frame m;
  check "releasing Up restores bit 0" ~expect:0xDF ~got:(port_a ());
  (* Player 2 is the interesting one: its d-pad straddles both ports, so a
     ROM reading only $DC would miss Left and Right entirely. *)
  Machine.press m Machine.Two Machine.Down;
  Machine.press m Machine.Two Machine.Left;
  Machine.run_frame m;
  check "P2 Down is in port A, bit 7" ~expect:0x5F ~got:(port_a ());
  check "P2 Left is in port B, bit 0" ~expect:0xFE ~got:(port_b ());
  (* And the frontend's panic button. *)
  Machine.release_all m;
  Machine.run_frame m;
  check "release_all clears port A" ~expect:0xFF ~got:(port_a ());
  check "release_all clears port B" ~expect:0xFF ~got:(port_b ())
;;

(* --- 4. sound comes out with the picture --------------------------------

   The frontend schedules one AudioBuffer per frame back to back, so the
   number of samples a frame yields is what its drift handling is built on:
   59736 T-states at 3579545 Hz is 16.69 ms, or 736 samples at 44.1 kHz.
   That is fractionally more than the 735 a 60 Hz display consumes, which is
   exactly why the frontend has to be able to drop. *)
let test_audio () =
  print_endline "=== a frame's worth of sound ===";
  let m = Machine.create ~rom:(rom_of [ 0x0000, backdrop_program ]) in
  check "rate" ~expect:44100 ~got:(Machine.audio_rate m);
  Machine.run_frame m;
  let samples = Machine.audio m in
  let n = Array.length samples in
  (* 59736 T-states is 735.9 samples, so frames alternate between 735 and
     736 and neither is the "right" answer on its own. *)
  check_between "about a frame of samples" ~low:735 ~high:736 ~got:n;
  check "taking empties the queue" ~expect:0 ~got:(Machine.audio_pending m);
  (* This ROM never writes to the PSG, and a chip that powers up at full
     volume would buzz through the whole game. *)
  let loudest =
    Array.fold_left (fun m s -> Float.max m (Float.abs s)) 0.0 samples
  in
  check "silent until written to" ~expect:0 ~got:(int_of_float (loudest *. 1e6));
  (* Ten frames without draining must not accumulate ten frames of backlog
     beyond the one-second cap. *)
  for _ = 1 to 10 do
    Machine.run_frame m
  done;

  check_between
    "ten frames queue ten frames"
    ~low:7350
    ~high:7365
    ~got:(Machine.audio_pending m);
  Machine.drop_audio m;
  check "drop_audio empties it" ~expect:0 ~got:(Machine.audio_pending m)
;;

let () =
  test_backdrop ();
  test_interrupts ();
  test_joypad ();
  test_audio ();
  if !failures = 0
  then print_endline "\nmachine: ALL PASS"
  else (
    Printf.printf "\nmachine: %d FAILED\n" !failures;
    exit 1)
;;
