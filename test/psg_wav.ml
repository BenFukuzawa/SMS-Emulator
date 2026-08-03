(* Plays a short tune on the PSG and writes it out as a WAV.

   The audio counterpart of vdp_frame: psg_test.ml asserts against numbers,
   but a number cannot tell you the noise channel is a click or that the
   envelope is inside out. This is the thing you listen to.

   Everything goes in as bytes to the chip's one write port, the way a ROM
   would send them.

   Usage: psg_wav [out.wav]   (default: psg.wav) *)

open Uints

let sample_rate = 44100
let psg_clock = 3579545
let w t byte = Psg.write t (Uint8.of_int byte)

(* 1cctdddd then 0-dddddd: a 10-bit period, low four bits first. *)
let tone t ~channel ~n =
  w t (0x80 lor (channel lsl 5) lor (n land 0x0F));
  w t ((n lsr 4) land 0x3F)
;;

let volume t ~channel ~attenuation =
  w t (0x90 lor (channel lsl 5) lor (attenuation land 0x0F))
;;

let noise t ~control = w t (0xE0 lor (control land 0x07))

(* f = 3579545 / 32N, so N = 3579545 / 32f. *)
let period_of_hz hz = int_of_float (float_of_int psg_clock /. (32.0 *. hz))

let run t seconds =
  let left = ref (int_of_float (float_of_int psg_clock *. seconds)) in
  while !left > 0 do
    let n = min 64 !left in
    Psg.step t ~cycles:n;
    left := !left - n
  done
;;

(* --- the tune ----------------------------------------------------------- *)

let a4 = 440.0
let semitone = 2.0 ** (1.0 /. 12.0)
let note n = a4 *. (semitone ** float_of_int n)

let play t =
  (* Everything off to begin with. *)
  for channel = 0 to 3 do
    volume t ~channel ~attenuation:0x0F
  done;
  (* An arpeggio on channel 0, so a wrong divider is audible as a wrong
     interval rather than just a wrong pitch. *)
  volume t ~channel:0 ~attenuation:0;
  List.iter
    (fun n ->
      tone t ~channel:0 ~n:(period_of_hz (note n));
      run t 0.12)
    [ 0; 4; 7; 12; 7; 4 ];
  volume t ~channel:0 ~attenuation:0x0F;
  (* Two channels at once, a fifth apart, to prove they mix. *)
  tone t ~channel:0 ~n:(period_of_hz (note 0));
  tone t ~channel:1 ~n:(period_of_hz (note 7));
  volume t ~channel:0 ~attenuation:2;
  volume t ~channel:1 ~attenuation:4;
  run t 0.5;
  volume t ~channel:0 ~attenuation:0x0F;
  volume t ~channel:1 ~attenuation:0x0F;
  (* White noise at each of the three fixed rates: a hiss that drops in
     pitch. Then periodic, which should buzz instead. *)
  volume t ~channel:3 ~attenuation:2;
  List.iter
    (fun control ->
      noise t ~control;
      run t 0.25)
    [ 0x04; 0x05; 0x06 ];
  noise t ~control:0x00;
  run t 0.35;
  volume t ~channel:3 ~attenuation:0x0F;
  (* And a pitched drum: noise rate 3 follows tone generator 2. *)
  volume t ~channel:3 ~attenuation:1;
  List.iter
    (fun hz ->
      tone t ~channel:2 ~n:(period_of_hz hz);
      noise t ~control:0x07;
      run t 0.12)
    [ 200.0; 150.0; 110.0; 80.0 ];
  volume t ~channel:3 ~attenuation:0x0F;
  run t 0.1
;;

(* --- WAV ---------------------------------------------------------------- *)

let le out value bytes =
  for i = 0 to bytes - 1 do
    output_byte out ((value lsr (8 * i)) land 0xFF)
  done
;;

(* 16-bit mono PCM, which is the least a player can be asked to understand. *)
let write_wav samples path =
  let out = open_out_bin path in
  let data_bytes = Array.length samples * 2 in
  output_string out "RIFF";
  le out (36 + data_bytes) 4;
  output_string out "WAVEfmt ";
  le out 16 4 (* PCM header size *);
  le out 1 2 (* format: PCM *);
  le out 1 2 (* channels: mono *);
  le out sample_rate 4;
  le out (sample_rate * 2) 4 (* bytes per second *);
  le out 2 2 (* block align *);
  le out 16 2 (* bits per sample *);
  output_string out "data";
  le out data_bytes 4;
  Array.iter
    (fun s ->
      let clamped = Float.max (-1.0) (Float.min 1.0 s) in
      let v = int_of_float (clamped *. 32767.0) in
      le out (v land 0xFFFF) 2)
    samples;
  close_out out
;;

let () =
  let path = if Array.length Sys.argv > 1 then Sys.argv.(1) else "psg.wav" in
  let t = Psg.create ~sample_rate () in
  play t;
  let samples = Psg.take t in
  write_wav samples path;
  let peak =
    Array.fold_left (fun m s -> Float.max m (Float.abs s)) 0.0 samples
  in
  Printf.printf
    "wrote %s: %d samples, %.2f s at %d Hz, peak %.3f\n"
    path
    (Array.length samples)
    (float_of_int (Array.length samples) /. float_of_int sample_rate)
    sample_rate
    peak
;;
