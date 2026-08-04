(* Browser frontend. Compiles to JS (see web/dune) and wires the SMS core to
   a canvas, the keyboard, and -- when it is switched on -- a set of panels
   showing the machine's insides while it runs.

   - requestAnimationFrame loop -> Machine.run_frame (the ~60 fps driver)
   - framebuffer -> putImageData (RGB expanded to canvas RGBA)
   - keydown/keyup -> Joypad.button_of_char -> Machine.press/release
   - a .sms file input builds the Machine; window blur releases all keys.

   The debug view is off by default and costs nothing while it is: the loop
   below runs exactly the two lines it always did. That matters more than it
   sounds. Hiding a panel with CSS would leave the tile sheet being decoded
   -- 32,768 pixels, four VRAM reads each -- into a canvas nobody is looking
   at. Switching a panel off here means the work does not happen. *)
open Js_of_ocaml

let by_id id = Dom_html.getElementById id
let opt_id id = Dom_html.getElementById_opt id
let doc = Dom_html.document
let status = by_id "status"
let set_status s = status##.innerHTML := Js.string s
let set_text el s = el##.textContent := Js.some (Js.string s)

(* The running machine, or None until a ROM is loaded, and the key mapping. *)
let machine : Machine.t option ref = ref None
let mode = ref Joypad.One_player
let running = ref true

(* --- canvas plumbing ------------------------------------------------------

   One ImageData per canvas, kept across frames. The previous version built a
   fresh one every frame, which is a ~200 KB allocation sixty times a second
   handed straight to the garbage collector. *)

type surface =
  { c : Dom_html.canvasRenderingContext2D Js.t
  ; mutable img : Dom_html.imageData Js.t option
  ; mutable dims : int * int
  }

let surface id =
  let el =
    Js.Opt.get
      (Dom_html.CoerceTo.canvas (by_id id))
      (fun () -> failwith ("missing #" ^ id))
  in
  { c = el##getContext Dom_html._2d_; img = None; dims = 0, 0 }
;;

(* Blit a three-bytes-per-pixel buffer. Canvas wants four and an opaque
   alpha, so the expansion happens here. *)
let blit s ~src ~w ~h =
  let img =
    match s.img with
    | Some i when s.dims = (w, h) -> i
    | _ ->
      let i = s.c##createImageData w h in
      s.img <- Some i;
      s.dims <- w, h;
      i
  in
  let data = img##.data in
  for i = 0 to (w * h) - 1 do
    let a = i * 3
    and b = i * 4 in
    Dom_html.pixel_set data b (Char.code (Bytes.unsafe_get src a));
    Dom_html.pixel_set
      data
      (b + 1)
      (Char.code (Bytes.unsafe_get src (a + 1)));
    Dom_html.pixel_set
      data
      (b + 2)
      (Char.code (Bytes.unsafe_get src (a + 2)));
    Dom_html.pixel_set data (b + 3) 255
  done;
  s.c##putImageData img (Js.number_of_float 0.) (Js.number_of_float 0.)
;;

let screen = surface "screen"

let render m =
  let w, h = Machine.frame_size m in
  blit screen ~src:(Machine.framebuffer m) ~w ~h
;;

(* --- panels ---------------------------------------------------------------

   Each panel owns a switch and a flag. Nothing below runs unless its flag is
   set, which is the whole point of the switches. *)

let debug_on = ref false

let panels =
  [ "cpu", ref true
  ; "dis", ref true
  ; "vram", ref true
  ; "tmap", ref true
  ; "cram", ref true
  ; "spr", ref true
  ]
;;

let is_on key =
  !debug_on
  && match List.assoc_opt key panels with Some r -> !r | None -> false
;;

let hex n width =
  let s = Printf.sprintf "%X" n in
  let pad = width - String.length s in
  "$" ^ String.make (max 0 pad) '0' ^ s
;;

(* --- CPU panel ----------------------------------------------------------- *)

let reg_names = [ "AF"; "BC"; "DE"; "HL"; "IX"; "IY"; "SP"; "PC"; "R" ]

let reg_cells =
  lazy
    (let host = by_id "regs" in
     List.map
       (fun name ->
         let d = Dom_html.createDiv doc in
         d##.className := Js.string (if name = "PC" then "reg pc" else "reg");
         let k = Dom_html.createSpan doc in
         k##.className := Js.string "k";
         set_text k name;
         let v = Dom_html.createSpan doc in
         v##.className := Js.string "v";
         Dom.appendChild d k;
         Dom.appendChild d v;
         Dom.appendChild host d;
         name, v)
       reg_names)
;;

let flag_cells =
  lazy
    (let host = by_id "flags" in
     List.map
       (fun ch ->
         let i = Dom_html.createI doc in
         set_text i (String.make 1 ch);
         Dom.appendChild host i;
         i)
       [ 'S'; 'Z'; 'x'; 'H'; 'y'; 'P'; 'N'; 'C' ])
;;

let update_cpu m =
  let open Registers in
  let r = Machine.For_debug.registers m in
  let rr16 x = Uints.Uint16.to_int (read_rr r x) in
  let value = function
    | "AF" -> hex (rr16 AF) 4
    | "BC" -> hex (rr16 BC) 4
    | "DE" -> hex (rr16 DE) 4
    | "HL" -> hex (rr16 HL) 4
    | "IX" -> hex (rr16 IX) 4
    | "IY" -> hex (rr16 IY) 4
    | "SP" -> hex (rr16 SP) 4
    | "PC" -> hex (Machine.For_debug.pc m) 4
    | _ ->
      let _, _, _, _, refresh, _ = Machine.For_debug.interrupt_state m in
      hex refresh 2
  in
  List.iter
    (fun (name, el) -> set_text el (value name))
    (Lazy.force reg_cells);
  (* Flag order matches Registers.show_f: S Z x H y P N C. *)
  let flags =
    [ Sign
    ; Zero
    ; Flag_x
    ; Half_carry
    ; Flag_y
    ; Parity_overflow
    ; Subtraction
    ; Carry
    ]
  in
  List.iter2
    (fun el f ->
      el##.className := Js.string (if read_flag r f then "set" else ""))
    (Lazy.force flag_cells)
    flags;
  let iff1, _, im, _, _, halted = Machine.For_debug.interrupt_state m in
  set_text (by_id "iff") (if iff1 then "1" else "0");
  set_text (by_id "im") (string_of_int im);
  let st = Debug.vdp_state m in
  set_text
    (by_id "scan")
    (string_of_int st.Debug.line ^ if halted then "  HALT" else "");
  let a, b, c = Machine.For_debug.mapper_pages m in
  set_text
    (by_id "banks")
    (Printf.sprintf "$0000 %02d   $4000 %02d   $8000 %02d" a b c)
;;

(* --- disassembly panel ---------------------------------------------------

   Sixteen rows built once and overwritten, rather than rebuilt. The target
   of a jump gets its own span so it can be coloured: it is the one operand a
   reader is actually chasing. *)

let dis_rows_count = 16

let dis_rows =
  lazy
    (let host = by_id "disasm" in
     List.init dis_rows_count (fun _ ->
       let row = Dom_html.createDiv doc in
       row##.className := Js.string "dis-row";
       let mk cls =
         let s = Dom_html.createSpan doc in
         s##.className := Js.string cls;
         Dom.appendChild row s;
         s
       in
       let a = mk "a"
       and b = mk "b" in
       let t = mk "t" in
       let em = Dom_html.createEm doc in
       Dom.appendChild t em;
       Dom.appendChild host row;
       row, a, b, t, em))
;;

(* Control-flow operands end in the address they land on. Splitting there
   lets the target be coloured without re-parsing the mnemonic. *)
let split_target text =
  let is_branch =
    List.exists
      (fun p ->
        String.length text >= String.length p
        && String.sub text 0 (String.length p) = p)
      [ "JR "; "DJNZ "; "JP "; "CALL "; "RST " ]
  in
  if not is_branch
  then text, ""
  else (
    match String.rindex_opt text '$' with
    | Some i when i > 0 ->
      String.sub text 0 i, String.sub text i (String.length text - i)
    | _ -> text, "")
;;

let update_disasm m =
  let pc = Machine.For_debug.pc m in
  let lines = Debug.disassemble m ~at:pc ~count:dis_rows_count in
  List.iteri
    (fun i (row, a, b, t, em) ->
      match List.nth_opt lines i with
      | None ->
        row##.className := Js.string "dis-row";
        set_text a "";
        set_text b "";
        set_text t "";
        set_text em ""
      | Some (l : Debug.line) ->
        let cls =
          "dis-row"
          ^ (if l.addr = pc then " cur" else "")
          ^ if l.is_data then " data" else ""
        in
        row##.className := Js.string cls;
        set_text a (Printf.sprintf "%04X" l.addr);
        set_text
          b
          (String.concat " " (List.map (Printf.sprintf "%02X") l.bytes));
        let head, tail = split_target l.text in
        set_text t head;
        set_text em tail)
    (Lazy.force dis_rows)
;;

(* --- palette -------------------------------------------------------------- *)

let cram_cells =
  lazy
    (List.concat_map
       (fun half ->
         let host = by_id ("cram" ^ string_of_int half) in
         List.init 16 (fun _ ->
           let i = Dom_html.createI doc in
           Dom.appendChild host i;
           i))
       [ 0; 1 ])
;;

let update_cram m =
  List.iteri
    (fun i el ->
      let rgb = Debug.cram_rgb m i in
      el##.style##.background
      := Js.string
           (Printf.sprintf
              "rgb(%d,%d,%d)"
              ((rgb lsr 16) land 0xFF)
              ((rgb lsr 8) land 0xFF)
              (rgb land 0xFF)))
    (Lazy.force cram_cells)
;;

(* --- sprites -------------------------------------------------------------- *)

let spr_rows =
  lazy
    (let host = by_id "sprites" in
     List.init 65 (fun _ ->
       let row = Dom_html.createDiv doc in
       row##.className := Js.string "spr-row";
       let cells =
         List.map
           (fun cls ->
             let s = Dom_html.createSpan doc in
             s##.className := Js.string cls;
             Dom.appendChild row s;
             s)
           [ "i"; ""; ""; "" ]
       in
       Dom.appendChild host row;
       row, cells))
;;

let update_sprites m =
  let list, terminated = Debug.sprites m in
  let n = List.length list in
  List.iteri
    (fun i (row, cells) ->
      let set cls vs =
        row##.className := Js.string cls;
        List.iter2 set_text cells vs
      in
      if i < n
      then (
        let s = List.nth list i in
        set
          "spr-row"
          [ Printf.sprintf "%02d" s.Debug.index
          ; string_of_int s.Debug.y
          ; string_of_int s.Debug.x
          ; hex s.Debug.tile 2
          ])
      else if i = n && terminated
      then (
        (* $D0 in the Y byte means this sprite and every one after it is not
           drawn. Saying so beats showing 60 rows of leftover table. *)
        row##.className := Js.string "spr-row term";
        List.iteri
          (fun j c ->
            set_text
              c
              (match j with
               | 0 -> Printf.sprintf "%02d" n
               | 1 -> "y = $D0 -- terminator"
               | _ -> ""))
          cells)
      else set "spr-row" [ ""; ""; ""; "" ])
    (Lazy.force spr_rows)
;;

(* --- pattern and tilemap views ------------------------------------------- *)

let vram_surface = lazy (surface "vram")
let tmap_surface = lazy (surface "tmap")
let palette = ref 0

let update_vram m =
  let w, h = Debug.tile_sheet_size in
  blit
    (Lazy.force vram_surface)
    ~src:(Debug.tile_sheet m ~palette:!palette)
    ~w
    ~h
;;

(* The viewport rectangle wraps, so it is drawn twice: once shifted a full
   map to the left. Otherwise it vanishes exactly when the scroll is most
   interesting to watch. *)
let update_tmap m =
  let s = Lazy.force tmap_surface in
  let w, h = Debug.tilemap_size m in
  blit s ~src:(Debug.tilemap m) ~w ~h;
  let x, y, vw, vh = Debug.viewport m in
  s.c##.strokeStyle := Js.string "#FFAA00";
  s.c##.lineWidth := Js.number_of_float 2.;
  let f x = Js.number_of_float (float_of_int x) in
  let rect ox = s.c##strokeRect (f (x + ox + 1)) (f y) (f (vw - 2)) (f vh) in
  rect 0;
  rect (-w)
;;

(* --- the loop -------------------------------------------------------------

   Three tiers. The picture every frame, because that is the game. Text at
   about eight times a second, because nobody can read a register that
   changes sixty times a second and the DOM writes are not free. The two
   pattern views at about ten, for the same reason -- a tile sheet redrawn
   sixty times a second looks exactly like one redrawn ten times. *)

let last_text = ref 0.
let last_heavy = ref 0.
let fps_acc = ref 0.
let fps_frames = ref 0
let fps_el = by_id "fps"

let update_fps now dt =
  fps_acc := !fps_acc +. dt;
  incr fps_frames;
  if !fps_acc >= 500.
  then (
    let f = float_of_int !fps_frames *. 1000. /. !fps_acc in
    let n = int_of_float (Float.round f) in
    set_text fps_el (string_of_int (min n 60));
    fps_el##.className
    := Js.string (if n < 30 then "bad" else if n < 52 then "warn" else "");
    fps_acc := 0.;
    fps_frames := 0);
  ignore now
;;

let refresh_panels m now =
  if now -. !last_text > 120.
  then (
    last_text := now;
    if is_on "cpu" then update_cpu m;
    if is_on "dis" then update_disasm m;
    if is_on "cram" then update_cram m;
    if is_on "spr" then update_sprites m);
  if now -. !last_heavy > 100.
  then (
    last_heavy := now;
    if is_on "vram" then update_vram m;
    if is_on "tmap" then update_tmap m)
;;

let prev = ref 0.

let rec loop stamp =
  let now = Js.float_of_number stamp in
  if !prev > 0. then update_fps now (now -. !prev);
  prev := now;
  (match !machine with
   | Some m ->
     if !running then Machine.run_frame m;
     render m;
     (* The whole cost of the debug view sits behind this one branch. Off,
        the loop is exactly what it was before any of it existed. *)
     if !debug_on then refresh_panels m now
(* --- audio ---------------------------------------------------------------

   js_of_ocaml has no WebAudio binding, so this is all Js.Unsafe. The graph
   is as small as it can be: one AudioBuffer per frame, scheduled back to
   back, straight at the destination.

   An AudioContext cannot be created before the user has interacted with the
   page -- browsers refuse, silently -- so it is built inside the ROM
   picker's handler rather than at start-up. *)

let audio_ctx = ref None

(* Where the sound already scheduled runs out, on the context's clock. *)
let next_time = ref 0.0

(* How far ahead of the clock to stay. Under about 50 ms a slow frame is
   audible as a gap; over about 200 ms the sound lags the picture visibly. *)
let lead = 0.08
let max_ahead = 0.20

let ensure_audio () =
  match !audio_ctx with
  | Some _ as c -> c
  | None ->
    let ctor : _ Js.optdef =
      Js.Unsafe.get Js.Unsafe.global (Js.string "AudioContext")
    in
    (match Js.Optdef.to_option ctor with
     | None -> None (* no WebAudio here; the picture still runs *)
     | Some ctor ->
       let c = Js.Unsafe.new_obj ctor [||] in
       (* A context can still come up suspended; asking costs nothing. *)
       ignore (Js.Unsafe.meth_call c "resume" [||]);
       next_time := 0.0;
       audio_ctx := Some c;
       !audio_ctx)
;;

let now_of c : float = Js.Unsafe.get c (Js.string "currentTime")

(* The buffer carries its own rate, so it is built at the PSG's 44.1 kHz
   whatever the device is running at and WebAudio resamples. That is one
   fewer thing to get wrong than matching the context's rate by hand. *)
let queue c samples rate =
  let n = Array.length samples in
  if n > 0
  then (
    let buffer =
      Js.Unsafe.meth_call
        c
        "createBuffer"
        [| Js.Unsafe.inject 1
         ; Js.Unsafe.inject n
         ; Js.Unsafe.inject (float_of_int rate)
        |]
    in
    let channel =
      Js.Unsafe.meth_call buffer "getChannelData" [| Js.Unsafe.inject 0 |]
    in
    for i = 0 to n - 1 do
      Js.Unsafe.set channel i samples.(i)
    done;
    let source = Js.Unsafe.meth_call c "createBufferSource" [||] in
    Js.Unsafe.set source (Js.string "buffer") buffer;
    ignore
      (Js.Unsafe.meth_call
         source
         "connect"
         [| Js.Unsafe.get c (Js.string "destination") |]);
    let at = Float.max (now_of c +. lead) !next_time in
    ignore (Js.Unsafe.meth_call source "start" [| Js.Unsafe.inject at |]);
    next_time := at +. (float_of_int n /. float_of_int rate))
;;

(* requestAnimationFrame runs at the display's 60 Hz; the console runs at
   59.92. So a frame's worth of samples is very slightly more than a frame's
   worth of time, and the queue creeps forward by about a millisecond a
   second -- a tenth of a second of lag every minute or two. Dropping a
   frame of audio once the lead gets too big is what keeps it bounded.

   The other direction is a stall: if the tab was in the background the
   clock has run on without us, and the cursor has to be pulled forward or
   everything queued afterwards is already late. *)
let feed_audio m =
  match !audio_ctx with
  | None -> Machine.drop_audio m
  | Some c ->
    let now = now_of c in
    if !next_time -. now > max_ahead
    then Machine.drop_audio m
    else (
      if !next_time < now then next_time := now +. lead;
      queue c (Machine.audio m) (Machine.audio_rate m))
;;

let rec loop _ =
  (match !machine with
   | Some m ->
     Machine.run_frame m;
     render m;
     feed_audio m
   | None -> ());
  ignore (Dom_html.window##requestAnimationFrame (Js.wrap_callback loop))
;;

(* --- keyboard ------------------------------------------------------------- *)

let key_char e =
  let k = Js.to_string (Js.Unsafe.get e (Js.string "key")) in
  if String.length k = 1 then Some (Char.lowercase_ascii k.[0]) else None
;;

let set_debug on =
  debug_on := on;
  doc##.body##.className := Js.string (if on then "" else "nodebug");
  (match opt_id "master" with
   | Some el ->
     Js.Opt.iter (Dom_html.CoerceTo.input el) (fun cb ->
       cb##.checked := Js.bool on)
   | None -> ());
  (* Coming back on, redraw immediately rather than waiting for the tier. *)
  last_text := 0.;
  last_heavy := 0.
;;

let on_key down e =
  (match !machine, key_char e with
   | _, Some '`' when down ->
     set_debug (not !debug_on);
     ()
   | Some m, Some 'p' when down -> Machine.pause m
   | Some m, Some c ->
     (match Joypad.button_of_char !mode c with
      | Some (player, button) ->
        if down
        then Machine.press m player button
        else Machine.release m player button
      | None -> ())
   | _ -> ());
  Js._true
;;

let install_keyboard () =
  ignore
    (Dom_html.addEventListener
       doc
       Dom_html.Event.keydown
       (Dom.handler (on_key true))
       Js._true);
  ignore
    (Dom_html.addEventListener
       doc
       Dom_html.Event.keyup
       (Dom.handler (on_key false))
       Js._true);
  (* Losing focus must release everything, or a held key sticks down. *)
  ignore
    (Dom_html.addEventListener
       Dom_html.window
       Dom_html.Event.blur
       (Dom.handler (fun _ ->
          (match !machine with Some m -> Machine.release_all m | None -> ());
          Js._true))
       Js._true)
;;

(* --- two-player toggle ---------------------------------------------------- *)

let install_toggle () =
  match Dom_html.CoerceTo.input (by_id "twoplayer") |> Js.Opt.to_option with
  | None -> ()
  | Some cb ->
    cb##.onchange
    := Dom.handler (fun _ ->
         mode
         := if Js.to_bool cb##.checked
            then Joypad.Two_player
            else Joypad.One_player;
         (* Remapping mid-hold could strand a key; clear both pads. *)
         (match !machine with Some m -> Machine.release_all m | None -> ());
         Js._true)
;;

(* --- debug switches, transport, scale ------------------------------------- *)

let data el name =
  Js.to_string
    (Js.Unsafe.get (Js.Unsafe.get el (Js.string "dataset")) (Js.string name))
;;

(* Walk a NodeList, coercing each entry to an element. *)
let each selector f =
  let nodes = doc##querySelectorAll (Js.string selector) in
  for i = 0 to nodes##.length - 1 do
    Js.Opt.iter
      (nodes##item i)
      (fun node -> Js.Opt.iter (Dom_html.CoerceTo.element node) f)
  done
;;

let set_panel key on =
  (match List.assoc_opt key panels with Some r -> r := on | None -> ());
  each
    ("[data-panel=\"" ^ key ^ "\"]")
    (fun el ->
      el##.className := Js.string (if on then "panel" else "panel off"))
;;

let install_panel_switches () =
  each "[data-toggle]" (fun el ->
    Js.Opt.iter (Dom_html.CoerceTo.input el) (fun cb ->
      let key = data cb "toggle" in
      cb##.onchange
      := Dom.handler (fun _ ->
           set_panel key (Js.to_bool cb##.checked);
           (* Redraw at once rather than waiting out the refresh tier. *)
           last_text := 0.;
           last_heavy := 0.;
           Js._true)))
;;

let install_master () =
  match opt_id "master" with
  | None -> ()
  | Some el ->
    Js.Opt.iter (Dom_html.CoerceTo.input el) (fun cb ->
      cb##.onchange
      := Dom.handler (fun _ ->
           set_debug (Js.to_bool cb##.checked);
           Js._true))
;;

(* Integer multiples only. A 256-pixel-wide image stretched to a fractional
   width duplicates some columns and not others, and on pixel art that is
   visible as shimmer down every vertical edge. The canvas takes an exact
   multiple and the panel sizes itself around it. *)
let install_scale () =
  each "#scales button" (fun el ->
    el##.onclick
    := Dom.handler (fun _ ->
         ignore
           (Js.Unsafe.meth_call
              doc##.documentElement##.style
              "setProperty"
              [| Js.Unsafe.inject (Js.string "--scale")
               ; Js.Unsafe.inject (Js.string (data el "scale"))
              |]);
         each "#scales button" (fun other ->
           other##setAttribute (Js.string "data-on") (Js.string "false"));
         el##setAttribute (Js.string "data-on") (Js.string "true");
         Js._true))
;;

let set_running v =
  running := v;
  match opt_id "run" with
  | None -> ()
  | Some el ->
    set_text el (if v then "Pause" else "Run");
    el##setAttribute
      (Js.string "data-on")
      (Js.string (if v then "false" else "true"))
;;

let on_click id f =
  match opt_id id with
  | None -> ()
  | Some el ->
    el##.onclick
    := Dom.handler (fun _ ->
         f ();
         Js._true)
;;

let install_transport () =
  on_click "run" (fun () -> set_running (not !running));
  on_click "step" (fun () ->
    set_running false;
    match !machine with
    | Some m ->
      ignore (Machine.step m : int);
      render m
    | None -> ());
  on_click "line" (fun () ->
    set_running false;
    match !machine with
    | Some m ->
      Machine.step_scanline m;
      render m
    | None -> ());
  on_click "frame" (fun () ->
    set_running false;
    match !machine with
    | Some m ->
      Machine.run_frame m;
      render m
    | None -> ());
  on_click "palette" (fun () ->
    palette := 1 - !palette;
    (match opt_id "palette" with
     | Some el ->
       set_text el (if !palette = 0 then "background" else "sprite")
     | None -> ());
    last_heavy := 0.)
;;

(* --- ROM loading ---------------------------------------------------------- *)

let start_rom bytes =
  machine := Some (Machine.create ~rom:bytes);
  set_running true;
  set_status (Printf.sprintf "running (%d KB)" (Bytes.length bytes / 1024))
  (* Picking a file is the user gesture that lets an AudioContext exist. *)
  let sound = ensure_audio () <> None in
  set_status
    (Printf.sprintf
       "running (%d KB)%s"
       (Bytes.length bytes / 1024)
       (if sound then "" else " - no audio in this browser"))
;;

let read_rom file =
  let reader = new%js File.fileReader in
  reader##.onload
  := Dom.handler (fun _ ->
       (match
          File.CoerceTo.arrayBuffer reader##.result |> Js.Opt.to_option
        with
        | Some ab ->
          let ta = new%js Typed_array.uint8Array_fromBuffer ab in
          let len = ta##.length in
          let b = Bytes.create len in
          for i = 0 to len - 1 do
            Bytes.set b i (Char.chr (Typed_array.unsafe_get ta i))
          done;
          start_rom b
        | None -> ());
       Js._true);
  reader##readAsArrayBuffer file
;;

let install_rom_input () =
  match Dom_html.CoerceTo.input (by_id "rom") |> Js.Opt.to_option with
  | None -> ()
  | Some input ->
    input##.onchange
    := Dom.handler (fun _ ->
         Js.Opt.iter (input##.files##item 0) read_rom;
         Js._true)
;;

let () =
  install_keyboard ();
  install_toggle ();
  install_rom_input ();
  install_panel_switches ();
  install_master ();
  install_scale ();
  install_transport ();
  set_debug false;
  set_status "load a .sms ROM";
  ignore (Dom_html.window##requestAnimationFrame (Js.wrap_callback loop))
;;
