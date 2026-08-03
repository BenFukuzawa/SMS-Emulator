(* Browser frontend. Compiles to JS (see web/dune) and wires the SMS core to a
   canvas and the keyboard:

     - requestAnimationFrame loop  -> Machine.run_frame  (the ~60 fps driver)
     - framebuffer -> putImageData (RGB expanded to canvas RGBA)
     - keydown/keyup -> Joypad.button_of_char -> Machine.press/release
     - a .sms file input builds the Machine; window blur releases all keys. *)
open Js_of_ocaml

let by_id id = Dom_html.getElementById id

let canvas =
  Js.Opt.get
    (Dom_html.CoerceTo.canvas (by_id "screen"))
    (fun () -> failwith "missing #screen canvas")
;;

let ctx = canvas##getContext Dom_html._2d_
let status = by_id "status"
let set_status s = status##.innerHTML := Js.string s

(* The running machine, or None until a ROM is loaded, and the key mapping. *)
let machine : Machine.t option ref = ref None
let mode = ref Joypad.One_player

(* Blit one frame: the core's framebuffer is 3 bytes/pixel (RGB); canvas
   ImageData is 4 (RGBA), so we expand and force alpha to 255. *)
let render m =
  let w, h = Machine.frame_size m in
  let fb = Machine.framebuffer m in
  let img = ctx##createImageData w h in
  let data = img##.data in
  for i = 0 to (w * h) - 1 do
    let s = i * 3 and d = i * 4 in
    Dom_html.pixel_set data d (Char.code (Bytes.get fb s));
    Dom_html.pixel_set data (d + 1) (Char.code (Bytes.get fb (s + 1)));
    Dom_html.pixel_set data (d + 2) (Char.code (Bytes.get fb (s + 2)));
    Dom_html.pixel_set data (d + 3) 255
  done;
  ctx##putImageData img (Js.number_of_float 0.) (Js.number_of_float 0.)
;;

let rec loop _ =
  (match !machine with
   | Some m ->
     Machine.run_frame m;
     render m
   | None -> ());
  ignore (Dom_html.window##requestAnimationFrame (Js.wrap_callback loop))
;;

(* --- keyboard ----------------------------------------------------------- *)

let key_char e =
  let k = Js.to_string (Js.Unsafe.get e (Js.string "key")) in
  if String.length k = 1 then Some (Char.lowercase_ascii k.[0]) else None
;;

let on_key down e =
  (match !machine, key_char e with
   | Some m, Some 'p' when down -> Machine.pause m
   | Some m, Some c ->
     (match Joypad.button_of_char !mode c with
      | Some (player, button) ->
        if down then Machine.press m player button else Machine.release m player button
      | None -> ())
   | _ -> ());
  Js._true
;;

let install_keyboard () =
  ignore
    (Dom_html.addEventListener
       Dom_html.document
       Dom_html.Event.keydown
       (Dom.handler (on_key true))
       Js._true);
  ignore
    (Dom_html.addEventListener
       Dom_html.document
       Dom_html.Event.keyup
       (Dom.handler (on_key false))
       Js._true);
  (* Losing focus must release everything, or a held key sticks down. *)
  ignore
    (Dom_html.addEventListener
       Dom_html.window
       Dom_html.Event.blur
       (Dom.handler (fun _ ->
          (match !machine with
           | Some m -> Machine.release_all m
           | None -> ());
          Js._true))
       Js._true)
;;

(* --- two-player toggle -------------------------------------------------- *)

let install_toggle () =
  match Dom_html.CoerceTo.input (by_id "twoplayer") |> Js.Opt.to_option with
  | None -> ()
  | Some cb ->
    cb##.onchange
    := Dom.handler (fun _ ->
         mode
         := (if Js.to_bool cb##.checked
             then Joypad.Two_player
             else Joypad.One_player);
         (* Remapping mid-hold could strand a key; clear both pads. *)
         (match !machine with
          | Some m -> Machine.release_all m
          | None -> ());
         Js._true)
;;

(* --- ROM loading -------------------------------------------------------- *)

let start_rom bytes =
  machine := Some (Machine.create ~rom:bytes);
  set_status (Printf.sprintf "running (%d KB)" (Bytes.length bytes / 1024))
;;

let read_rom file =
  let reader = new%js File.fileReader in
  reader##.onload
  := Dom.handler (fun _ ->
       (match File.CoerceTo.arrayBuffer reader##.result |> Js.Opt.to_option with
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
  set_status "load a .sms ROM";
  ignore (Dom_html.window##requestAnimationFrame (Js.wrap_callback loop))
;;
