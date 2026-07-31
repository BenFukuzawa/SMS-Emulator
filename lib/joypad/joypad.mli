open Uints

type t

val create : unit -> t

(* A controller's inputs: a 4-way d-pad and two fire buttons. *)
type button =
  | Up
  | Down
  | Left
  | Right
  | Button1
  | Button2

(* Which of the two controller ports a button belongs to. *)
type player =
  | One
  | Two

(* Whether the keyboard drives one or two players (see button_of_char). *)
type mode =
  | One_player
  | Two_player

(* Set/clear a button on a player's pad; a frontend calls these on key down/up. *)
val press : t -> player -> button -> unit
val release : t -> player -> button -> unit

(* Release every button on both pads at once. Call this on input-focus loss,
   pause/reset, and whenever the key mapping changes, so a key held across the
   change cannot leave a button stuck down. *)
val release_all : t -> unit

(* Map a keyboard character to (player, button):
     one player  - WASD = P1 d-pad, J/K = P1 buttons
     two players - WASD = P1 d-pad, Z/X = P1 buttons,
                   IJKL = P2 d-pad, N/M = P2 buttons
   Any other key is None. *)
val button_of_char : mode -> char -> (player * button) option

(* The last byte written to the 0x3F control register; exposed for tests. *)
val control : t -> uint8

include Joypad_intf.S with type t := t
