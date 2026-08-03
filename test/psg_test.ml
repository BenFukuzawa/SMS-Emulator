open Uints

(* SN76489. Every expectation below is quoted from the SN76489 page on SMS
   Power or Maxim's SN76489 document in the comment above it.

   Nothing here plays a sound: the chip's whole observable behaviour is the
   sample stream, so the tests measure that -- frequencies by counting zero
   crossings, the noise register by its period. *)

let failures = ref 0

let check ~name ~expect ~actual =
  if expect = actual
  then Printf.printf "  PASS  %s\n" name
  else (
    incr failures;
    Printf.printf "  FAIL  %s: expected %d, got %d\n" name expect actual)
;;

let check_close ~name ~expect ~actual ~within =
  if Float.abs (expect -. actual) <= within
  then Printf.printf "  PASS  %s (%.1f)\n" name actual
  else (
    incr failures;
    Printf.printf
      "  FAIL  %s: expected %.1f +/- %.1f, got %.1f\n"
      name
      expect
      within
      actual)
;;

let group name = Printf.printf "=== %s ===\n" name
let w t byte = Psg.write t (Uint8.of_int byte)

(* One second of chip time, in T-states. *)
let psg_clock = 3579545

let run t cycles =
  (* Feed it in instruction-sized bites rather than one huge number, which is
     how it is actually driven. *)
  let left = ref cycles in
  while !left > 0 do
    let n = min 17 !left in
    Psg.step t ~cycles:n;
    left := !left - n
  done
;;

(* --- the write protocol ------------------------------------------------

   "if bit 7 is 1 then the byte is a LATCH/DATA byte formatted as 1cctdddd"
   ... "If bit 7 is 0 then the byte is a DATA byte. The low 6 bits of the
   byte are placed into the high 6 bits of the latched register." *)

let test_latch_and_data () =
  group "latch and data bytes";
  let t = Psg.create () in
  (* $8F = latch channel 0, tone, low nibble $F. Then $3F is a data byte
     carrying six more bits, giving the full 10-bit $3FF. *)
  w t 0x8F;
  check
    ~name:"latch byte sets the low nibble"
    ~expect:0x00F
    ~actual:(Psg.For_tests.tone_register t 0);
  w t 0x3F;
  check
    ~name:"data byte sets the high six bits"
    ~expect:0x3FF
    ~actual:(Psg.For_tests.tone_register t 0);
  (* Only the low six bits of a data byte are used. *)
  w t 0x81;
  w t 0xFF;
  check
    ~name:"data byte ignores bits 7-6"
    ~expect:0x3F1
    ~actual:(Psg.For_tests.tone_register t 0)
;;

let test_channel_and_type_bits () =
  group "channel and type bits";
  let t = Psg.create () in
  (* 1cctdddd: bits 6-5 are the channel, bit 4 picks volume. *)
  w t 0xA5 (* 1 01 0 0101: channel 1, tone, $5 *);
  check ~name:"channel 1 tone" ~expect:0x005 ~actual:(Psg.For_tests.tone_register t 1);
  w t 0xC3 (* 1 10 0 0011: channel 2, tone, $3 *);
  check ~name:"channel 2 tone" ~expect:0x003 ~actual:(Psg.For_tests.tone_register t 2);
  w t 0x97 (* 1 00 1 0111: channel 0, volume, $7 *);
  check ~name:"channel 0 volume" ~expect:7 ~actual:(Psg.For_tests.volume t 0);
  w t 0xF0 (* 1 11 1 0000: channel 3, volume, $0 *);
  check ~name:"channel 3 volume" ~expect:0 ~actual:(Psg.For_tests.volume t 3);
  (* Channel 0's tone must not have moved while all that happened. *)
  check ~name:"other channels untouched" ~expect:0 ~actual:(Psg.For_tests.tone_register t 0)
;;

let test_latch_survives () =
  group "the latch persists";
  let t = Psg.create () in
  w t 0x8E (* latch channel 0 tone *);
  (* A volume latch for another channel moves the latch, as it is itself a
     latch byte -- so the following data byte belongs to channel 1's volume,
     not channel 0's tone. *)
  w t 0xB0 (* latch channel 1 volume *);
  w t 0x0A;
  check ~name:"data followed the new latch" ~expect:0x0A ~actual:(Psg.For_tests.volume t 1);
  check
    ~name:"channel 0 tone kept its nibble"
    ~expect:0x00E
    ~actual:(Psg.For_tests.tone_register t 0);
  let channel, is_volume = Psg.For_tests.latched t in
  check ~name:"latched channel" ~expect:1 ~actual:channel;
  check ~name:"latched as volume" ~expect:1 ~actual:(if is_volume then 1 else 0)
;;

(* --- volume -------------------------------------------------------------

   "00 is full volume and 11 is silence", 2dB per step. *)

let test_volume_table () =
  group "volume table";
  let table = Psg.For_tests.volume_table in
  check ~name:"0 is full scale" ~expect:32767 ~actual:table.(0);
  check ~name:"15 is true silence" ~expect:0 ~actual:table.(15);
  (* Each step is -2dB, a factor of 10^(-0.1) = 0.794. *)
  check_close
    ~name:"one step down is 2dB"
    ~expect:(float_of_int table.(0) *. 0.7943)
    ~actual:(float_of_int table.(1))
    ~within:2.0;
  (* Monotonic all the way down. *)
  let ok = ref true in
  for i = 0 to 14 do
    if table.(i) <= table.(i + 1) then ok := false
  done;
  check ~name:"strictly decreasing" ~expect:1 ~actual:(if !ok then 1 else 0)
;;

(* --- tone ---------------------------------------------------------------

   "Input clock (3579545) / (2 x register value x divider 16)". The
   document's own example: "a register value of 0x0fe gives 440.4Hz". *)

(* Count how often the mixed output crosses zero in one second, which is
   twice the frequency of a single square wave. *)
let measure_hz t =
  let samples = ref [] in
  run t psg_clock;
  samples := [ Psg.take t ];
  let all = List.hd !samples in
  let crossings = ref 0 in
  for i = 1 to Array.length all - 1 do
    if (all.(i - 1) < 0.0 && all.(i) >= 0.0)
       || (all.(i - 1) >= 0.0 && all.(i) < 0.0)
    then incr crossings
  done;
  float_of_int !crossings /. 2.0
;;

let play_tone ~channel ~n ~volume =
  let t = Psg.create () in
  (* Silence the three channels we are not measuring. *)
  for c = 0 to 3 do
    w t (0x90 lor (c lsl 5) lor 0x0F)
  done;
  w t (0x80 lor (channel lsl 5) lor (n land 0x0F));
  w t ((n lsr 4) land 0x3F);
  w t (0x90 lor (channel lsl 5) lor volume);
  t
;;

let test_tone_frequency () =
  group "tone frequency";
  (* The documented example. *)
  let t = play_tone ~channel:0 ~n:0x0FE ~volume:0 in
  check_close ~name:"N = $0FE is A4" ~expect:440.4 ~actual:(measure_hz t) ~within:1.0;
  (* "The lowest possible tone using register value $3ff is 109Hz" *)
  let t = play_tone ~channel:1 ~n:0x3FF ~volume:0 in
  check_close ~name:"N = $3FF is the lowest" ~expect:109.3 ~actual:(measure_hz t) ~within:1.0;
  (* And an octave up from A4 needs half the divider. *)
  let t = play_tone ~channel:2 ~n:0x07F ~volume:0 in
  check_close ~name:"N = $07F is an octave up" ~expect:880.8 ~actual:(measure_hz t) ~within:2.0
;;

(* "If the register value is zero then the output is a constant value
   of +1." *)
let test_tone_zero_is_constant () =
  group "tone register zero";
  let t = play_tone ~channel:0 ~n:0 ~volume:0 in
  run t (psg_clock / 100);
  let samples = Psg.take t in
  let varying = ref false in
  Array.iter (fun s -> if Float.abs (s -. samples.(0)) > 1e-9 then varying := true) samples;
  check ~name:"output never moves" ~expect:0 ~actual:(if !varying then 1 else 0);
  check ~name:"and it is positive" ~expect:1 ~actual:(if samples.(0) > 0.0 then 1 else 0)
;;

let test_silence_is_silent () =
  group "silence";
  let t = Psg.create () in
  for c = 0 to 3 do
    w t (0x90 lor (c lsl 5) lor 0x0F)
  done;
  run t (psg_clock / 100);
  let samples = Psg.take t in
  let loudest = Array.fold_left (fun m s -> Float.max m (Float.abs s)) 0.0 samples in
  check_close ~name:"attenuation $F is exactly zero" ~expect:0.0 ~actual:loudest ~within:1e-9
;;

(* --- noise --------------------------------------------------------------

   Shift rates are counter reload values of $10, $20, $40, or tone 2's
   register; bit 2 picks white (1) or periodic (0). *)

let test_noise_register () =
  group "noise register";
  let t = Psg.create () in
  w t 0xE0;
  check ~name:"rate 0 reloads $10" ~expect:0x10 ~actual:(Psg.For_tests.noise_period t);
  w t 0xE1;
  check ~name:"rate 1 reloads $20" ~expect:0x20 ~actual:(Psg.For_tests.noise_period t);
  w t 0xE2;
  check ~name:"rate 2 reloads $40" ~expect:0x40 ~actual:(Psg.For_tests.noise_period t);
  (* Rate 3 follows tone generator 2, which is how a game pitches a drum. *)
  w t 0xC0;
  w t 0x08 (* channel 2 tone = $080 *);
  w t 0xE3;
  check ~name:"rate 3 follows tone 2" ~expect:0x080 ~actual:(Psg.For_tests.noise_period t);
  (* Only three bits of the register mean anything. *)
  w t 0xEF;
  check ~name:"only the low 3 bits are kept" ~expect:0x07 ~actual:(Psg.For_tests.noise_register t)
;;

(* "When the noise register is written to, the shift register is reset, such
   that all bits are zero except for the highest bit." *)
let test_noise_write_resets_lfsr () =
  group "noise write resets the shift register";
  let t = Psg.create () in
  w t 0xE4 (* white noise, fastest *);
  w t 0xF0 (* channel 3 full volume *);
  run t 100_000;
  let moved = Psg.For_tests.lfsr t <> 0x8000 in
  check ~name:"it shifts while running" ~expect:1 ~actual:(if moved then 1 else 0);
  w t 0xE4;
  check ~name:"and a write resets it" ~expect:0x8000 ~actual:(Psg.For_tests.lfsr t)
;;

(* Periodic noise feeds bit 0 straight back, so the register is a plain
   rotate: 16 shifts return it to where it started. White noise taps bits 0
   and 3 and must not. *)
let test_lfsr_modes () =
  group "white and periodic noise";
  let periodic = Psg.create () in
  w periodic 0xE0 (* bit 2 clear: periodic *);
  w periodic 0xF0;
  (* $10 reload, shifting on every other expiry: 16 shifts is 16*2*$10
     internal ticks, or that many times 16 T-states. *)
  run periodic (16 * 2 * 0x10 * 16);
  check
    ~name:"periodic returns to the reset value after 16 shifts"
    ~expect:0x8000
    ~actual:(Psg.For_tests.lfsr periodic);
  let white = Psg.create () in
  w white 0xE4 (* bit 2 set: white *);
  w white 0xF0;
  run white (16 * 2 * 0x10 * 16);
  check
    ~name:"white does not"
    ~expect:0
    ~actual:(if Psg.For_tests.lfsr white = 0x8000 then 1 else 0)
;;

(* --- the sample stream -------------------------------------------------- *)

let test_sample_rate () =
  group "sample rate";
  let t = Psg.create () in
  run t psg_clock;
  let produced = Psg.pending t in
  (* One second of chip time is one second of audio, to within a sample. *)
  check_close
    ~name:"44100 samples in a second"
    ~expect:44100.0
    ~actual:(float_of_int produced)
    ~within:2.0;
  let taken = Psg.take t in
  check ~name:"take returns them all" ~expect:produced ~actual:(Array.length taken);
  check ~name:"and empties the queue" ~expect:0 ~actual:(Psg.pending t)
;;

let test_custom_rate () =
  group "a different sample rate";
  let t = Psg.create ~sample_rate:22050 () in
  check ~name:"reported" ~expect:22050 ~actual:(Psg.sample_rate t);
  run t psg_clock;
  check_close
    ~name:"22050 samples in a second"
    ~expect:22050.0
    ~actual:(float_of_int (Psg.pending t))
    ~within:2.0
;;

(* A host that never drains must not be able to grow the queue without
   bound. A browser tab left running is 350 KB a second otherwise. *)
let test_queue_is_bounded () =
  group "the sample queue is bounded";
  let t = Psg.create () in
  run t (psg_clock / 2);
  let after_half_a_second = Psg.pending t in
  run t (psg_clock * 10);
  let after_ten_more = Psg.pending t in
  check
    ~name:"half a second fits"
    ~expect:1
    ~actual:(if after_half_a_second > 20_000 then 1 else 0);
  check
    ~name:"ten more seconds do not accumulate"
    ~expect:1
    ~actual:(if after_ten_more <= 44_100 then 1 else 0)
;;

let () =
  test_latch_and_data ();
  test_queue_is_bounded ();
  test_channel_and_type_bits ();
  test_latch_survives ();
  test_volume_table ();
  test_tone_frequency ();
  test_tone_zero_is_constant ();
  test_silence_is_silent ();
  test_noise_register ();
  test_noise_write_resets_lfsr ();
  test_lfsr_modes ();
  test_sample_rate ();
  test_custom_rate ();
  print_newline ();
  if !failures = 0
  then print_endline "psg: ALL PASS"
  else (
    Printf.printf "psg: %d FAILED\n" !failures;
    exit 1)
;;
