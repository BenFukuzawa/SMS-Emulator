(* Renders one synthetic frame and writes it out as a PPM.

   Not an assertion suite -- vdp_test.ml is that. This exists because a
   thousand passing index comparisons still cannot tell you that the picture
   is upside down, and because every scene below exercises a rule that is
   easy to get subtly wrong: both palettes, the priority bit, sprite
   transparency, the eight-per-line limit and collision.

   Everything goes in through the $BE/$BF ports rather than the test
   backdoor, so a run of this also exercises the command protocol.

   Usage: vdp_frame [out.ppm]   (default: frame.ppm) *)

open Uints

let ctrl t byte = Vdp.write_control t (Uint8.of_int byte)
let data t byte = Vdp.write_data t (Uint8.of_int byte)

let set_reg t n v =
  ctrl t v;
  ctrl t (0x80 lor n)
;;

let set_write_address t addr =
  ctrl t (addr land 0xFF);
  ctrl t (0x40 lor (addr lsr 8))
;;

let write_vram t ~addr bytes =
  set_write_address t addr;
  List.iter (fun b -> data t b) bytes
;;

(* CRAM is reached with code 3 rather than code 1. *)
let write_cram t ~entry values =
  ctrl t entry;
  ctrl t 0xC0;
  Array.iter (fun v -> data t v) values
;;

(* --- the scene --------------------------------------------------------- *)

(* --BBGGRR. Two arbitrary but well-separated spreads, so that a channel
   swap or an off-by-one in the palette split is obvious on sight. *)
let background_palette =
  [| 0x00; 0x01; 0x02; 0x03; 0x04; 0x08; 0x0C; 0x0D
   ; 0x10; 0x20; 0x30; 0x31; 0x32; 0x3C; 0x2A; 0x3F
  |]
;;

let sprite_palette =
  [| 0x00; 0x03; 0x0C; 0x30; 0x3F; 0x0F; 0x33; 0x3C
   ; 0x15; 0x2A; 0x01; 0x04; 0x10; 0x14; 0x28; 0x11
  |]
;;

(* One pattern row, as the four bitplane bytes it is stored as. *)
let plane_bytes row =
  List.init 4 (fun plane ->
    let byte = ref 0 in
    Array.iteri
      (fun x colour ->
        if (colour lsr plane) land 1 = 1 then byte := !byte lor (1 lsl (7 - x)))
      row;
    !byte)
;;

let pattern rows = List.concat_map plane_bytes (Array.to_list rows)

(* Tiles 0-15: solid colour n, so a column of tile n is a stripe of CRAM
   entry n. *)
let solid n = Array.init 8 (fun _ -> Array.make 8 n)

(* Tile 16: an X, whose empty pixels are colour 0 and therefore transparent
   when a sprite uses it. *)
let cross colour =
  Array.init 8 (fun y ->
    Array.init 8 (fun x -> if x = y || x = 7 - y then colour else 0))
;;

let name_table = 0x3800
let sat = 0x3F00

let put_entry t ~row ~col ~tile ~palette ~priority =
  let addr = name_table + (((row * 32) + col) * 2) in
  write_vram
    t
    ~addr
    [ tile land 0xFF
    ; ((tile lsr 8) land 1) lor (palette lsl 3) lor if priority then 0x10 else 0
    ]
;;

let put_sprite t ~index ~y ~x ~tile =
  write_vram t ~addr:(sat + index) [ y ];
  write_vram t ~addr:(sat + 0x80 + (index * 2)) [ x; tile ]
;;

let build t =
  set_reg t 0 0x06 (* mode 4, no scroll locks, left column shown *);
  set_reg t 1 0xE0 (* display on, 192 lines, 8x8 sprites *);
  set_reg t 2 0xFF (* name table at $3800 *);
  set_reg t 5 0xFF (* sprite attributes at $3F00 *);
  set_reg t 6 0xFB (* sprite patterns at $0000 *);
  set_reg t 7 0x00;
  write_cram t ~entry:0 background_palette;
  write_cram t ~entry:16 sprite_palette;
  for n = 0 to 15 do
    write_vram t ~addr:(n * 32) (pattern (solid n))
  done;
  write_vram t ~addr:(16 * 32) (pattern (cross 5));
  (* Rows 0-7: the background palette as vertical stripes, twice across.
     Rows 8-15: the same tiles through the sprite palette, which is the one
     thing that distinguishes the palette-select bit from a tile change.
     Rows 16-23: a flat field, with priority set on columns 8-15 only. *)
  for row = 0 to 23 do
    for col = 0 to 31 do
      let tile, palette, priority =
        if row < 8
        then col land 15, 0, false
        else if row < 16
        then col land 15, 1, false
        else 6, 0, col >= 8 && col < 16
      in
      put_entry t ~row ~col ~tile ~palette ~priority
    done
  done;
  (* Ten sprites on one scanline. Only eight may be drawn, and the first two
     sit on top of each other so the collision flag has something to find.
     They cross the priority band, where they must disappear behind it. *)
  for i = 0 to 63 do
    write_vram t ~addr:(sat + i) [ 0xC0 ]
  done;
  put_sprite t ~index:0 ~y:139 ~x:8 ~tile:16;
  put_sprite t ~index:1 ~y:139 ~x:8 ~tile:16;
  for i = 2 to 9 do
    put_sprite t ~index:i ~y:139 ~x:(8 + (i * 24)) ~tile:16
  done
;;

(* --- output ------------------------------------------------------------ *)

let write_ppm t path =
  let width, height = Vdp.frame_size t in
  let out = open_out_bin path in
  Printf.fprintf out "P6\n%d %d\n255\n" width height;
  output_bytes out (Bytes.sub (Vdp.framebuffer t) 0 (width * height * 3));
  close_out out
;;

let () =
  let path = if Array.length Sys.argv > 1 then Sys.argv.(1) else "frame.ppm" in
  let t = Vdp.create () in
  build t;
  (* One frame, through the engine rather than by rendering lines directly. *)
  for _ = 1 to 262 do
    Vdp.step t ~cycles:228
  done;
  write_ppm t path;
  let width, height = Vdp.frame_size t in
  let status = Vdp.read_status t |> Uint8.to_int in
  Printf.printf "wrote %s (%dx%d), %d frame(s)\n" path width height
    (Vdp.frame_count t);
  Printf.printf
    "status $%02X: vblank=%b overflow=%b collision=%b\n"
    status
    (status land 0x80 <> 0)
    (status land 0x40 <> 0)
    (status land 0x20 <> 0)
;;
