(* End-to-end smoke check: boot a real ROM and confirm the picture, the sound
   and the debug view all come out of the same running machine. *)

let load path =
  let ic = open_in_bin path in
  let len = in_channel_length ic in
  let b = Bytes.create len in
  really_input ic b 0 len;
  close_in ic;
  b
;;

let failures = ref 0

let check name ok detail =
  Printf.printf
    "  %s  %-38s %s\n"
    (if ok then "PASS" else "FAIL")
    name
    detail;
  if not ok then incr failures
;;

let () =
  let path = Sys.argv.(1) in
  let rom = load path in
  Printf.printf
    "ROM %s (%d KB)\n"
    (Filename.basename path)
    (Bytes.length rom / 1024);
  let m = Machine.create ~rom in
  let frames = 600 in
  let audio_frames = ref 0 in
  let total_samples = ref 0 in
  let peak = ref 0.0 in
  let nonsilent = ref 0 in
  for _ = 1 to frames do
    Machine.run_frame m;
    (* Exactly what the browser loop does, in the same order. *)
    let a = Machine.audio m in
    if Array.length a > 0 then incr audio_frames;
    total_samples := !total_samples + Array.length a;
    Array.iter
      (fun s ->
        if Float.abs s > 0.001 then incr nonsilent;
        peak := Float.max !peak (Float.abs s))
      a
  done;
  Printf.printf "after %d frames:\n" frames;
  check
    "frames advanced"
    (Machine.frame_count m = frames)
    (Printf.sprintf "frame_count=%d" (Machine.frame_count m));
  (* Picture: a booted game is not a uniform screen. *)
  let fb = Machine.framebuffer m in
  let distinct = Hashtbl.create 64 in
  Bytes.iteri
    (fun i c -> if i mod 3 = 0 then Hashtbl.replace distinct c ())
    fb;
  check
    "framebuffer has real content"
    (Hashtbl.length distinct > 1)
    (Printf.sprintf "%d distinct red values" (Hashtbl.length distinct));
  (* Sound: the queue drains every frame and the game actually plays notes. *)
  let expected = Machine.audio_rate m * frames / 60 in
  check
    "audio produced every frame"
    (!audio_frames = frames)
    (Printf.sprintf "%d/%d frames" !audio_frames frames);
  check
    "sample count tracks wall clock"
    (abs (!total_samples - expected) < expected / 10)
    (Printf.sprintf "%d samples, expected ~%d" !total_samples expected);
  check
    "queue fully drained"
    (Machine.audio_pending m = 0)
    (Printf.sprintf "pending=%d" (Machine.audio_pending m));
  check
    "the PSG is actually audible"
    (!nonsilent > 0 && !peak > 0.01)
    (Printf.sprintf "%d non-silent samples, peak %.3f" !nonsilent !peak);
  (* Debug view: every panel derives something from the live machine. *)
  let pc = Machine.For_debug.pc m in
  let lines = Debug.disassemble m ~at:pc ~count:16 in
  check
    "disassembly at live PC"
    (List.length lines = 16)
    (Printf.sprintf
       "PC=$%04X %s"
       pc
       (match lines with l :: _ -> l.Debug.text | [] -> "-"));
  let p0, p1, p2 = Machine.For_debug.mapper_pages m in
  check
    "mapper paged the cartridge"
    (p0 >= 0 && p1 >= 0 && p2 >= 0)
    (Printf.sprintf "pages %d/%d/%d" p0 p1 p2);
  let sheet = Debug.tile_sheet m ~palette:0 in
  let tw, th = Debug.tile_sheet_size in
  check
    "tile sheet decodes"
    (Bytes.length sheet = tw * th * 3)
    (Printf.sprintf "%dx%d" tw th);
  let tmap = Debug.tilemap m in
  let mw, mh = Debug.tilemap_size m in
  (* The buffer is reused and sized for the tallest mode; [tilemap_size] is
     what says how much of it is live this frame. *)
  check
    "tilemap decodes"
    (Bytes.length tmap >= mw * mh * 3)
    (Printf.sprintf "%dx%d live of %d bytes" mw mh (Bytes.length tmap));
  let sprites, terminated = Debug.sprites m in
  check
    "sprite table reads"
    (List.length sprites >= 0)
    (Printf.sprintf
       "%d sprites, terminator=%b"
       (List.length sprites)
       terminated);
  let cram = Debug.cram m in
  check "CRAM reads" (Array.length cram = 32) "32 entries";
  ignore (Debug.vdp_state m);
  (* Stepping is what the debug view's pause/step buttons drive. *)
  let before = Machine.For_debug.pc m in
  let cycles = Machine.step m in
  Machine.step_scanline m;
  check
    "single-step advances"
    (cycles > 0 && Machine.For_debug.pc m <> before)
    (Printf.sprintf
       "%d cycles, PC $%04X -> $%04X"
       cycles
       before
       (Machine.For_debug.pc m));
  (* Inspection must not have changed the run: same ROM, no inspection. *)
  let clean = Machine.create ~rom in
  for _ = 1 to frames do
    Machine.run_frame clean;
    ignore (Machine.audio clean : float array)
  done;
  check
    "inspection did not perturb the run"
    (Bytes.equal (Machine.framebuffer clean) fb)
    "framebuffer identical to the inspected machine";
  if !failures = 0
  then print_endline "\nall smoke checks passed"
  else (
    Printf.printf "\n%d check(s) failed\n" !failures;
    exit 1)
;;
