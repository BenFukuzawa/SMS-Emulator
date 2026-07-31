open Uints

(* SMS controller ports.

   The console has two controller connectors, each a 4-way d-pad plus two fire
   buttons (labelled 1 and 2). Software reads them through two I/O ports whose
   bits are active-low (0 = pressed). Pad 2's bits are split across both ports:

     0xDC (port A) bit0 Up  bit1 Down bit2 Left bit3 Right
                   bit4 button1 bit5 button2  bit6 pad-2 Up  bit7 pad-2 Down
     0xDD (port B) bit0 pad-2 Left bit1 pad-2 Right bit2 pad-2 button1
                   bit3 pad-2 button2  bit4 Reset  bit5 unused  bit6-7 TH pins

   0x3F is the I/O port control register: it sets the TH output levels used for
   light-gun latching and region detection.

   Reset and the TH pins are not modelled yet (they idle high). *)

type button =
  | Up
  | Down
  | Left
  | Right
  | Button1
  | Button2

type player =
  | One
  | Two

type mode =
  | One_player
  | Two_player

type pad =
  { mutable up : bool
  ; mutable down : bool
  ; mutable left : bool
  ; mutable right : bool
  ; mutable button1 : bool
  ; mutable button2 : bool
  }

type t =
  { p1 : pad
  ; p2 : pad
  ; mutable control : int (* last byte written to 0x3F *)
  }

let fresh_pad () =
  { up = false
  ; down = false
  ; left = false
  ; right = false
  ; button1 = false
  ; button2 = false
  }
;;

let create () = { p1 = fresh_pad (); p2 = fresh_pad (); control = 0 }

let controller t = function
  | One -> t.p1
  | Two -> t.p2
;;

let set t player button pressed =
  let p = controller t player in
  match button with
  | Up -> p.up <- pressed
  | Down -> p.down <- pressed
  | Left -> p.left <- pressed
  | Right -> p.right <- pressed
  | Button1 -> p.button1 <- pressed
  | Button2 -> p.button2 <- pressed
;;

let press t player button = set t player button true
let release t player button = set t player button false

(* Release every button on both pads. Used on input-focus loss, pause/reset,
   or when the frontend changes key mapping (see the .mli). *)
let clear_pad p =
  p.up <- false;
  p.down <- false;
  p.left <- false;
  p.right <- false;
  p.button1 <- false;
  p.button2 <- false
;;

let release_all t =
  clear_pad t.p1;
  clear_pad t.p2
;;

(* Keyboard mapping. Player 1's d-pad is WASD in both modes; the rest depends
   on how many players are active (see the .mli for the full table). *)
let button_of_char mode c =
  match mode, Char.lowercase_ascii c with
  | _, 'w' -> Some (One, Up)
  | _, 'a' -> Some (One, Left)
  | _, 's' -> Some (One, Down)
  | _, 'd' -> Some (One, Right)
  | One_player, 'j' -> Some (One, Button1)
  | One_player, 'k' -> Some (One, Button2)
  | Two_player, 'z' -> Some (One, Button1)
  | Two_player, 'x' -> Some (One, Button2)
  | Two_player, 'i' -> Some (Two, Up)
  | Two_player, 'j' -> Some (Two, Left)
  | Two_player, 'k' -> Some (Two, Down)
  | Two_player, 'l' -> Some (Two, Right)
  | Two_player, 'n' -> Some (Two, Button1)
  | Two_player, 'm' -> Some (Two, Button2)
  | _ -> None
;;

(* Active low: a pressed button drives its line to 0, a released one to 1. *)
let lvl pressed = if pressed then 0 else 1

(* Port A (0xDC): pad 1's six inputs, plus pad 2's Up/Down in bits 6-7. *)
let read_port_a t =
  Uint8.of_int
    (lvl t.p1.up
     lor (lvl t.p1.down lsl 1)
     lor (lvl t.p1.left lsl 2)
     lor (lvl t.p1.right lsl 3)
     lor (lvl t.p1.button1 lsl 4)
     lor (lvl t.p1.button2 lsl 5)
     lor (lvl t.p2.up lsl 6)
     lor (lvl t.p2.down lsl 7))
;;

(* Port B (0xDD): pad 2's Left/Right/buttons in bits 0-3; the Reset button
   (bit 4), an unused line (bit 5), and the two TH pins (bits 6-7) all idle
   high. Reset and TH are not modelled yet. *)
let read_port_b t =
  Uint8.of_int
    (lvl t.p2.left
     lor (lvl t.p2.right lsl 1)
     lor (lvl t.p2.button1 lsl 2)
     lor (lvl t.p2.button2 lsl 3)
     lor (1 lsl 4) (* Reset: not pressed *)
     lor (1 lsl 5) (* unused *)
     lor (1 lsl 6) (* TH port A *)
     lor (1 lsl 7) (* TH port B *))
;;

let write_control t data = t.control <- Uint8.to_int data

(* Exposed for tests/debugging: the last value written to 0x3F. *)
let control t = Uint8.of_int t.control
