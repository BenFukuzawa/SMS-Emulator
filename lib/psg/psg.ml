open Uints

(* Texas Instruments SN76489, the Master System's sound chip.

   Three square-wave tone channels and one noise channel, four bits of volume
   each. It is write-only: there is no register a program can read back, so
   the only way a ROM can tell whether anything is listening is the silence.

   Everything here is from the SN76489 page on SMS Power and Maxim's SN76489
   document, quoted at the point it is used. Where both are silent -- how the
   four outputs actually combine in the analogue stage -- the choice is
   marked as a convention rather than a fact.

   The chip is clocked from the same 3.579545 MHz crystal as the Z80 and
   divides it by 16, so [step] takes the T-states an instruction cost, the
   same currency the VDP is driven in. *)

let psg_clock = 3579545
let internal_divider = 16
let default_sample_rate = 44100

(* "00 is full volume and 11 is silence... attenuates the volume by 2dB for
   each step". The table is Maxim's. The last entry is a true zero rather
   than the -30dB the pattern would give, so silence is actually silent. *)
let volume_table =
  [| 32767
   ; 26028
   ; 20675
   ; 16422
   ; 13045
   ; 10362
   ; 8231
   ; 6568
   ; 5193
   ; 4125
   ; 3277
   ; 2603
   ; 2067
   ; 1642
   ; 1304
   ; 0
  |]
;;

type channel =
  { mutable reg : int (* 10 bits of tone, or 3 of noise control *)
  ; mutable volume : int (* attenuation, 0 loudest .. 15 silent *)
  ; mutable counter : int (* when reg reaches 0, restart *)
  ; mutable output : bool (* which half of the square wave we are in *)
  }

type t =
  { (* 0-2 are the tone channels, 3 is noise. One array, because the latch
       below is handed a channel number and should not have to branch. *)
    channels : channel array
  ; mutable lfsr : int (* the noise shift register, 16 bits *)
  ; mutable latched : int (* channel a bare data byte belongs to *)
  ; mutable latched_volume : bool (* ... and whether it is a volume byte *)
  ; mutable cycle_acc : int (* T-states not yet worth an internal tick *)
  ; (* Resampling. The chip runs at 223 kHz and a host wants 44.1, so every
       output sample is the mean of the ~5.07 internal ticks inside it: a box
       filter, which costs nothing and stops the high tones aliasing down
       into the audible range. *)
    sample_rate : int
  ; mutable sample_acc : int
  ; mutable sum : int
  ; mutable sum_count : int
  ; mutable samples : float array
  ; mutable pending : int
  }

(* "When the noise register is written to, the shift register is reset, such
   that all bits are zero except for the highest bit." *)
let lfsr_reset = 0x8000

(* Power-on volume is the silent end of the table: a chip that came up at
   full volume would buzz until the ROM got round to quietening it. *)
let fresh_channel () =
  { reg = 0; volume = 0x0F; counter = 1; output = false }
;;

let create ?(sample_rate = default_sample_rate) () =
  { channels = Array.init 4 (fun _ -> fresh_channel ())
  ; lfsr = lfsr_reset
  ; latched = 0
  ; latched_volume = false
  ; cycle_acc = 0
  ; sample_rate
  ; sample_acc = 0
  ; sum = 0
  ; sum_count = 0
  ; samples = Array.make sample_rate 0.0 (* one second, never grows *)
  ; pending = 0
  }
;;

(* --- the write protocol -------------------------------------------------

   "When a byte is written to the SN76489, if bit 7 is 1 then the byte is a
   LATCH/DATA byte formatted as 1cctdddd, where bits 6 and 5 (cc) give the
   channel to be latched, bit 4 (t) determines whether to latch volume (1) or
   tone/noise (0) data, and the remaining 4 bits (dddd) are placed into the
   low 4 bits of the relevant register."

   "If bit 7 is 0 then the byte is a DATA byte. The low 6 bits of the byte
   are placed into the high 6 bits of the latched register."

   A full 10-bit tone therefore takes two writes, and the latch between them
   survives whatever else the program does in between. *)

let write_noise_control t value =
  t.channels.(3).reg <- value land 0x07;
  t.lfsr <- lfsr_reset
;;

let write t byte =
  let byte = Uint8.to_int byte in
  if byte land 0x80 <> 0
  then (
    let channel = (byte lsr 5) land 0x03 in
    let is_volume = byte land 0x10 <> 0 in
    let data = byte land 0x0F in
    t.latched <- channel;
    t.latched_volume <- is_volume;
    if is_volume
    then t.channels.(channel).volume <- data
    else if channel = 3
    then write_noise_control t data
    else (
      let c = t.channels.(channel) in
      c.reg <- c.reg land 0x3F0 lor data))
  else if t.latched_volume
  then t.channels.(t.latched).volume <- byte land 0x0F
  else if t.latched = 3
  then
    (* "If a data byte is written, its low 3 bits update the shift rate and
       mode in the same way" -- which resets the shift register too. *)
    write_noise_control t (byte land 0x07)
  else (
    let c = t.channels.(t.latched) in
    c.reg <- c.reg land 0x0F lor ((byte land 0x3F) lsl 4))
;;

(* --- generating ---------------------------------------------------------

   A tone counter runs down to zero, flips the output and reloads, so a full
   cycle takes two reloads: f = clock / 16 / 2N = 3579545 / 32N. The
   document's own check is N = $0FE giving 440.4 Hz, an A4. *)

(* "If the register value is zero then the output is a constant value of +1."
   That is not the same as N = 1, and it is how a game parks a channel
   without touching its volume. *)
let tick_tone c =
  if c.reg = 0
  then c.output <- true
  else (
    c.counter <- c.counter - 1;
    if c.counter <= 0
    then (
      c.counter <- c.reg;
      c.output <- not c.output))
;;

(* Shift rates are counter reload values of $10, $20, $40, or "tone generator
   2"'s register -- which is how a game gets a pitched drum. A zero there
   would stall the counter, so it is floored at one. *)
let noise_period t =
  match t.channels.(3).reg land 0x03 with
  | 0 -> 0x10
  | 1 -> 0x20
  | 2 -> 0x40
  | _ -> max 1 t.channels.(2).reg
;;

(* "For the SMS (1 and 2), Genesis and Game Gear, the tapped bits are bits 0
   and 3 ($0009), fed back into bit 15." Bit 2 of the noise register picks
   white (1) or periodic (0); periodic feeds bit 0 straight back, turning the
   register into a plain rotate, which buzzes rather than hisses. *)
let shift_lfsr t =
  let white = t.channels.(3).reg land 0x04 <> 0 in
  let feedback =
    if white
    then t.lfsr land 1 lxor ((t.lfsr lsr 3) land 1)
    else t.lfsr land 1
  in
  t.lfsr <- (t.lfsr lsr 1) lor (feedback lsl 15)
;;

let tick_noise t =
  let c = t.channels.(3) in
  c.counter <- c.counter - 1;
  if c.counter <= 0
  then (
    c.counter <- noise_period t;
    c.output <- not c.output;
    (* The counter is a square wave like a tone channel's and the register
       shifts on one edge of it, which is what makes the documented rates
       clock/512, clock/1024 and clock/2048 rather than half those. *)
    if c.output then shift_lfsr t)
;;

let level channel high =
  let volume = volume_table.(channel.volume) in
  if high then volume else -volume
;;

(* How the four outputs combine in the analogue stage is in neither document.
   Summing them and scaling by four channels' worth is the convention: it
   cannot clip, and silence is exactly zero because every channel swings
   symmetrically about it. *)
let mix t =
  level t.channels.(0) t.channels.(0).output
  + level t.channels.(1) t.channels.(1).output
  + level t.channels.(2) t.channels.(2).output
  + level t.channels.(3) (t.lfsr land 1 = 1)
;;

(* The queue holds a second of audio and no more. A host is expected to take
   samples every frame, but one that forgets -- or a headless run that only
   wants the picture -- must not grow this without bound: at 44.1 kHz that is
   350 KB a second, which in a browser tab left open is a leak rather than a
   slow frame.

   When it fills, the older half goes. Audio that late is no use to anyone
   anyway, and dropping in bulk keeps the common case free of shuffling. *)
let push t sample =
  if t.pending >= Array.length t.samples
  then (
    let keep = t.pending / 2 in
    Array.blit t.samples (t.pending - keep) t.samples 0 keep;
    t.pending <- keep);
  t.samples.(t.pending) <- sample;
  t.pending <- t.pending + 1
;;

let scale = float_of_int (4 * 32767)

let internal_tick t =
  tick_tone t.channels.(0);
  tick_tone t.channels.(1);
  tick_tone t.channels.(2);
  tick_noise t;
  t.sum <- t.sum + mix t;
  t.sum_count <- t.sum_count + 1;
  (* A sample is due every psg_clock/sample_rate internal ticks. Counting in
     units of clock*rate keeps that exact rather than drifting the way a
     floating-point period would over a few million samples. *)
  t.sample_acc <- t.sample_acc + (internal_divider * t.sample_rate);
  if t.sample_acc >= psg_clock
  then (
    t.sample_acc <- t.sample_acc - psg_clock;
    push t (float_of_int t.sum /. float_of_int t.sum_count /. scale);
    t.sum <- 0;
    t.sum_count <- 0)
;;

let step t ~cycles =
  t.cycle_acc <- t.cycle_acc + cycles;
  while t.cycle_acc >= internal_divider do
    t.cycle_acc <- t.cycle_acc - internal_divider;
    internal_tick t
  done
;;

(* --- output ------------------------------------------------------------ *)

let sample_rate t = t.sample_rate
let pending t = t.pending

let take t =
  let out = Array.sub t.samples 0 t.pending in
  t.pending <- 0;
  out
;;

let drop t = t.pending <- 0

module For_tests = struct
  let tone_register t n = t.channels.(n).reg
  let volume t n = t.channels.(n).volume
  let noise_register t = t.channels.(3).reg
  let lfsr t = t.lfsr
  let latched t = t.latched, t.latched_volume
  let noise_period = noise_period
  let volume_table = volume_table
end
