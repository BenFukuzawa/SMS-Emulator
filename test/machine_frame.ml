(* Boots a hand-assembled ROM on the assembled machine and dumps the frame.

   Everything else in test/ reaches into one component. This is the first
   thing that runs the whole chain: Z80 instructions fetched through the
   cartridge mapper and memory bus, OUT instructions decoded by the I/O bus,
   landing on the VDP, clocked by the T-states the CPU reports back, and
   ending in a framebuffer.

   The ROM programs the VDP the way a real one does -- registers, palette,
   patterns and name table, all through ports $BE/$BF -- then spins. If the
   picture below is right, every link in that chain is carrying real data.

   Usage: machine_frame [out.ppm]   (default: machine.ppm) *)

(* --- a very small Z80 assembler ---------------------------------------- *)

let code = Buffer.create 16384
let byte b = Buffer.add_char code (Char.chr (b land 0xFF))
let bytes l = List.iter byte l

(* The only instructions this ROM needs. *)
let di () = byte 0xF3
let ld_a v = bytes [ 0x3E; v ] (* LD A, n *)
let out_c port = bytes [ 0xD3; port ] (* OUT (n), A *)
let jr_here () = bytes [ 0x18; 0xFE ] (* JR $ -- spin forever *)

(* A VDP command is a pair of bytes to $BF, low half first. *)
let vdp_command ~low ~high =
  ld_a low;
  out_c 0xBF;
  ld_a high;
  out_c 0xBF
;;

let set_reg n v = vdp_command ~low:v ~high:(0x80 lor n)

let set_vram_write addr =
  vdp_command ~low:(addr land 0xFF) ~high:(0x40 lor (addr lsr 8))
;;

let set_cram_write entry = vdp_command ~low:entry ~high:0xC0

(* The address counter auto-increments, so a run of bytes needs one command
   and then a stream of writes to $BE. *)
let stream l =
  List.iter
    (fun v ->
      ld_a v;
      out_c 0xBE)
    l
;;

(* --- the ROM ------------------------------------------------------------ *)

let background_palette =
  [ 0x00; 0x01; 0x02; 0x03; 0x04; 0x08; 0x0C; 0x0D
  ; 0x10; 0x20; 0x30; 0x31; 0x32; 0x3C; 0x2A; 0x3F
  ]
;;

(* Tile n is a solid block of colour n: every row has all eight pixels set in
   the bitplanes that n's bits call for. *)
let solid_tile n =
  List.concat_map
    (fun _ -> List.init 4 (fun plane -> if (n lsr plane) land 1 = 1 then 0xFF else 0x00))
    (List.init 8 (fun i -> i))
;;

let assemble () =
  Buffer.clear code;
  di ();
  (* Display off while VRAM is loaded, which is what a real ROM does -- the
     chip is easier to write to during blanking. *)
  set_reg 0 0x06 (* mode 4, no scroll locks, left column shown *);
  set_reg 1 0xA0 (* display OFF for now *);
  set_reg 2 0xFF (* name table at $3800 *);
  set_reg 5 0xFF (* sprite attributes at $3F00 *);
  set_reg 6 0xFB (* sprite patterns at $0000 *);
  set_reg 7 0x00;
  set_cram_write 0;
  stream background_palette;
  set_vram_write 0x0000;
  List.iter (fun n -> stream (solid_tile n)) (List.init 16 (fun i -> i));
  (* 32 columns by 24 visible rows, column c showing tile c mod 16, so the
     screen is the palette twice over. *)
  set_vram_write 0x3800;
  for _row = 0 to 23 do
    for col = 0 to 31 do
      stream [ col land 15; 0x00 ]
    done
  done;
  set_reg 1 0xE0 (* display on *);
  jr_here ();
  (* A 32 KB cartridge, which is two mapper banks. *)
  let rom = Bytes.make 0x8000 '\x00' in
  Bytes.blit (Buffer.to_bytes code) 0 rom 0 (Buffer.length code);
  rom
;;

(* --- output ------------------------------------------------------------- *)

let write_ppm m path =
  let width, height = Machine.frame_size m in
  let out = open_out_bin path in
  Printf.fprintf out "P6\n%d %d\n255\n" width height;
  output_bytes out (Bytes.sub (Machine.framebuffer m) 0 (width * height * 3));
  close_out out
;;

let () =
  let path =
    if Array.length Sys.argv > 1 then Sys.argv.(1) else "machine.ppm"
  in
  let rom = assemble () in
  Printf.printf "ROM: %d bytes of code in a %d byte cartridge\n"
    (Buffer.length code)
    (Bytes.length rom);
  let m = Machine.create ~rom in
  (* Setting up VRAM a byte at a time costs more than a frame, so give the
     ROM several before looking. *)
  for _ = 1 to 10 do
    Machine.run_frame m
  done;
  write_ppm m path;
  let width, height = Machine.frame_size m in
  Printf.printf
    "ran %d frames, wrote %s (%dx%d)\n"
    (Machine.frame_count m)
    path
    width
    height
;;
