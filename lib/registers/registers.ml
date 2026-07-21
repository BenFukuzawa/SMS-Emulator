open Stdint

type t = {
  mutable a : uint8;
  mutable b : uint8;
  mutable c : uint8;
  mutable d : uint8;
  mutable e : uint8;
  mutable h : uint8;
  mutable l : uint8;

  mutable a_shadow : uint8;
  mutable b_shadow : uint8;
  mutable c_shadow : uint8;
  mutable d_shadow : uint8;
  mutable e_shadow : uint8;
  mutable h_shadow : uint8;
  mutable l_shadow : uint8;

  mutable f : uint8;
  mutable f_shadow : uint8;

  mutable ix : uint16;
  mutable iy : uint16;
  mutable sp : uint16;

  mutable 
}

type r =
  | A
  | B
  | C
  | D
  | E
  | H
  | L

type rr =
  | AF
  | BC
  | DE
  | HL
  | IX
  | IY
  | SP

(* read/write functions for the above registers. carry, add/subtract, parity/overflow, unused, half carry, unused, zero, sign*)
  type flag =
  | Carry
  | Subtraction (* 1 = subtraction, 0 = addition *)
  | Parity_overflow
  | Flag_y
  | Half_carry
  | Flag_x
  | Zero
  | Sign


let create () = {
  a = Uint8.zero;
  b = Uint8.zero;
  c = Uint8.zero;
  d = Uint8.zero;
  e = Uint8.zero;
  h = Uint8.zero;
  l = Uint8.zero;
  a_shadow = Uint8.zero;
  b_shadow = Uint8.zero;
  c_shadow = Uint8.zero;
  d_shadow = Uint8.zero;
  e_shadow = Uint8.zero;
  h_shadow = Uint8.zero;
  l_shadow = Uint8.zero;
  f = Uint8.zero;
  f_shadow = Uint8.zero;
  ix = Uint16.zero;
  iy = Uint16.zero;
  sp = Uint16.zero;

}

let read_r t = function
  | A -> t.a
  | B -> t.b
  | C -> t.c
  | D -> t.d
  | E -> t.e
  | H -> t.h
  | L -> t.l

let read_rr t rr =
  let open Uint16 in
  match rr with
  | AF -> (of_uint8 t.a lsl 8) lor of_uint8 t.f
  | BC -> (of_uint8 t.b lsl 8) lor of_uint8 t.c
  | DE -> (of_uint8 t.d lsl 8) lor of_uint8 t.e
  | HL -> (of_uint8 t.h lsl 8) lor of_uint8 t.l
  | IX -> t.ix
  | IY -> t.iy
  | SP -> t.sp

let write_r t r x = match r with
  | A -> t.a <- x
  | B -> t.b <- x
  | C -> t.c <- x
  | D -> t.d <- x
  | E -> t.e <- x
  | H -> t.h <- x
  | L -> t.l <- x

let write_rr t rr x =
  let x = Uint16.to_int x in
  let high = (x land 0xFF00) lsr 8 |> Uint8.of_int in
  let low  =  x land 0x00FF        |> Uint8.of_int in
  match rr with
  | AF ->
    t.a <- high;
    (* Bottom 4 bits of the flag register is always zero *)
    t.f <- Uint8.(low land of_int 0xF0)
  | BC -> t.b <- high; t.c <- low
  | DE -> t.d <- high; t.e <- low
  | HL -> t.h <- high; t.l <- low
  | IX -> t.ix <- x
  | IY -> t.iy <- x
  | SP -> t.iy <- x

let read_flag t flag =
  let f = t.f |> Uint8.to_int in
  match flag with
  | Carry            -> f land 0b00000001 <> 0
  | Subtraction      -> f land 0b00000010 <> 0
  | Parity_overflow  -> f land 0b00000100 <> 0
  | Flag_x           -> f land 0b00001000 <> 0
  | Half_carry       -> f land 0b00010000 <> 0
  | Flag_y           -> f land 0b00100000 <> 0
  | Zero             -> f land 0b01000000 <> 0
  | Sign             -> f land 0b10000000 <> 0

(* Precompute uint8 masks to reduce calls to Uint8.of_int.
 * Improves performance of whole emulator by ~1% *)

let mask_0b00000001 = Uint8.of_int 0b00000001
let mask_0b11111110 = Uint8.of_int 0b11111110

let mask_0b00000010 = Uint8.of_int 0b00000010
let mask_0b11111101 = Uint8.of_int 0b11111101

let mask_0b00000100 = Uint8.of_int 0b00000100
let mask_0b11111011 = Uint8.of_int 0b11111011

let mask_0b00001000 = Uint8.of_int 0b00001000
let mask_0b11110111 = Uint8.of_int 0b11110111

let mask_0b00010000 = Uint8.of_int 0b00010000
let mask_0b11101111 = Uint8.of_int 0b11101111

let mask_0b00100000 = Uint8.of_int 0b00100000
let mask_0b11011111 = Uint8.of_int 0b11011111

let mask_0b01000000 = Uint8.of_int 0b01000000
let mask_0b10111111 = Uint8.of_int 0b10111111

let mask_0b10000000 = Uint8.of_int 0b10000000
let mask_0b01111111 = Uint8.of_int 0b01111111

let set_flag t flag =
  let open Uint8 in
  match flag with
  | Carry            -> t.f <- t.f lor mask_0b00000001
  | Subtraction      -> t.f <- t.f lor mask_0b00000010
  | Parity_overflow  -> t.f <- t.f lor mask_0b00000100
  | Flag_x           -> t.f <- t.f lor mask_0b00001000
  | Half_carry       -> t.f <- t.f lor mask_0b00010000
  | Flag_y           -> t.f <- t.f lor mask_0b00100000
  | Zero             -> t.f <- t.f lor mask_0b01000000
  | Sign             -> t.f <- t.f lor mask_0b10000000
let set_flags t
    ?(c = read_flag t Carry)
    ?(n = read_flag t Subtraction)
    ?(p = read_flag t Parity_overflow)
    ?(y = read_flag t Flag_y)
    ?(h = read_flag t Half_carry)
    ?(x = read_flat t Flag_x)
    ?(z = read_flag t Zero)
    ?(s = read_flag t Sign)
    () =
  let open Uint8 in
  if c then t.f <- t.f lor mask_0b00000001 else t.f <- t.f land mask_0b11111110;
  if n then t.f <- t.f lor mask_0b00000010 else t.f <- t.f land mask_0b11111101;
  if p then t.f <- t.f lor mask_0b00000100 else t.f <- t.f land mask_0b11111011;
  if y then t.f <- t.f lor mask_0b00001000 else t.f <- t.f land mask_0b11110111;
  if h then t.f <- t.f lor mask_0b00010000 else t.f <- t.f land mask_0b11101111;
  if x then t.f <- t.f lor mask_0b00100000 else t.f <- t.f land mask_0b11011111;
  if z then t.f <- t.f lor mask_0b01000000 else t.f <- t.f land mask_0b10111111;
  if s then t.f <- t.f lor mask_0b10000000 else t.f <- t.f land mask_0b01111111;

let unset_flag t flag =
  let open Uint8 in
  match flag with
  | Carry            -> t.f <- t.f land mask_0b11111110
  | Subtraction      -> t.f <- t.f land mask_0b11111101
  | Parity_overflow  -> t.f <- t.f land mask_0b11111011
  | Flag_x           -> t.f <- t.f land mask_0b11110111
  | Half_carry       -> t.f <- t.f land mask_0b11101111
  | Flag_y           -> t.f <- t.f land mask_0b11011111
  | Zero             -> t.f <- t.f land mask_0b10111111
  | Sign             -> t.f <- t.f land mask_0b01111111

let clear_flags t = t.f <- Uint8.zero

let show_r = function
  | A -> "A"
  | B -> "B"
  | C -> "C"
  | D -> "D"
  | E -> "E"
  | H -> "H"
  | L -> "L"

let show_rr = function
  | AF -> "AF"
  | BC-> "BC"
  | DE -> "DE"
  | HL -> "HL"
  | IX -> "IX"
  | IY -> "IY"
  | SP -> "SP"

let show_f f =
  let f = Uint8.to_int f in
  let c = if f land 0b00000001 <> 0 then 'Z' else '-' in
  let n = if f land 0b00000010 <> 0 then 'N' else '-' in
  let p = if f land 0b00000100 <> 0 then 'P' else '-' in
  let y = if f land 0b00001000 <> 0 then 'y' else '-' in
  let h = if f land 0b00010000 <> 0 then 'H' else '-' in
  let x = if f land 0b00100000 <> 0 then 'x' else '-' in
  let z = if f land 0b01000000 <> 0 then 'Z' else '-' in
  let s = if f land 0b10000000 <> 0 then 'S' else '-' in
  Printf.sprintf "%c%c%c%c%c%c%c%c" f c n p y h x z s

let show t =
  Printf.sprintf
    "A:%s F:%s BC:%s DE:%s HL:%s IX:%s IY:%s SP:%s"
    (read_r t A |> Uint8.show)
    (show_f t.f)
    (read_rr t BC |> Uint16.show)
    (read_rr t DE |> Uint16.show)
    (read_rr t HL |> Uint16.show)
    (Uint16.show t.ix)
    (Uint16.show t.iy)
    (Uint16.show t.sp)