(* Exercises the SMS controller: active-low port A/B reads for both pads, the
   one- and two-player key mappings, and the 0x3F control-register write path. *)
open Uints

let failures = ref 0

let check name ~expect ~got =
  if expect = got
  then Printf.printf "ok   %-28s 0x%02X\n" name got
  else (
    incr failures;
    Printf.printf "FAIL %-28s expected 0x%02X got 0x%02X\n" name expect got)
;;

let () =
  let j = Joypad.create () in
  let pa () = Uint8.to_int (Joypad.read_port_a j) in
  let pb () = Uint8.to_int (Joypad.read_port_b j) in
  (* Nothing pressed: active-low, so every bit reads high on both ports. *)
  check "idle port A" ~expect:0xFF ~got:(pa ());
  check "idle port B" ~expect:0xFF ~got:(pb ());
  (* Player 1 lives entirely in port A, bits 0-5. *)
  let p1_bit name button ~expect =
    Joypad.press j Joypad.One button;
    check name ~expect ~got:(pa ());
    Joypad.release j Joypad.One button
  in
  p1_bit "P1 Up -> A bit0" Joypad.Up ~expect:0xFE;
  p1_bit "P1 Down -> A bit1" Joypad.Down ~expect:0xFD;
  p1_bit "P1 Left -> A bit2" Joypad.Left ~expect:0xFB;
  p1_bit "P1 Right -> A bit3" Joypad.Right ~expect:0xF7;
  p1_bit "P1 Button1 -> A bit4" Joypad.Button1 ~expect:0xEF;
  p1_bit "P1 Button2 -> A bit5" Joypad.Button2 ~expect:0xDF;
  (* Player 2 is split: Up/Down in port A bits 6-7, the rest in port B. *)
  Joypad.press j Joypad.Two Joypad.Up;
  check "P2 Up -> A bit6" ~expect:0xBF ~got:(pa ());
  Joypad.release j Joypad.Two Joypad.Up;
  Joypad.press j Joypad.Two Joypad.Down;
  check "P2 Down -> A bit7" ~expect:0x7F ~got:(pa ());
  Joypad.release j Joypad.Two Joypad.Down;
  let p2_bit_b name button ~expect =
    Joypad.press j Joypad.Two button;
    check name ~expect ~got:(pb ());
    Joypad.release j Joypad.Two button
  in
  p2_bit_b "P2 Left -> B bit0" Joypad.Left ~expect:0xFE;
  p2_bit_b "P2 Right -> B bit1" Joypad.Right ~expect:0xFD;
  p2_bit_b "P2 Button1 -> B bit2" Joypad.Button1 ~expect:0xFB;
  p2_bit_b "P2 Button2 -> B bit3" Joypad.Button2 ~expect:0xF7;
  (* Both pads at once stay independent across the two ports. *)
  Joypad.press j Joypad.One Joypad.Up;
  Joypad.press j Joypad.Two Joypad.Left;
  check "P1 Up in A" ~expect:0xFE ~got:(pa ());
  check "P2 Left in B" ~expect:0xFE ~got:(pb ());
  Joypad.release j Joypad.One Joypad.Up;
  Joypad.release j Joypad.Two Joypad.Left;
  (* Key mapping: 1 = maps as expected, 0 = wrong. *)
  let mapped mode c expected =
    match Joypad.button_of_char mode c with
    | Some pb -> if pb = expected then 1 else 0
    | None -> 0
  in
  let unmapped mode c =
    match Joypad.button_of_char mode c with
    | None -> 1
    | Some _ -> 0
  in
  (* One-player mode: WASD + J/K on player 1; P2 keys inert. *)
  check "1P 'w' -> P1 Up" ~expect:1
    ~got:(mapped Joypad.One_player 'w' (Joypad.One, Joypad.Up));
  check "1P 'j' -> P1 Button1" ~expect:1
    ~got:(mapped Joypad.One_player 'j' (Joypad.One, Joypad.Button1));
  check "1P 'k' -> P1 Button2" ~expect:1
    ~got:(mapped Joypad.One_player 'k' (Joypad.One, Joypad.Button2));
  check "1P 'i' -> None" ~expect:1 ~got:(unmapped Joypad.One_player 'i');
  check "1P 'z' -> None" ~expect:1 ~got:(unmapped Joypad.One_player 'z');
  (* Two-player mode: P1 buttons move to Z/X; P2 is IJKL + N/M. *)
  check "2P 'z' -> P1 Button1" ~expect:1
    ~got:(mapped Joypad.Two_player 'z' (Joypad.One, Joypad.Button1));
  check "2P 'x' -> P1 Button2" ~expect:1
    ~got:(mapped Joypad.Two_player 'x' (Joypad.One, Joypad.Button2));
  check "2P 'i' -> P2 Up" ~expect:1
    ~got:(mapped Joypad.Two_player 'i' (Joypad.Two, Joypad.Up));
  check "2P 'j' -> P2 Left" ~expect:1
    ~got:(mapped Joypad.Two_player 'j' (Joypad.Two, Joypad.Left));
  check "2P 'k' -> P2 Down" ~expect:1
    ~got:(mapped Joypad.Two_player 'k' (Joypad.Two, Joypad.Down));
  check "2P 'l' -> P2 Right" ~expect:1
    ~got:(mapped Joypad.Two_player 'l' (Joypad.Two, Joypad.Right));
  check "2P 'n' -> P2 Button1" ~expect:1
    ~got:(mapped Joypad.Two_player 'n' (Joypad.Two, Joypad.Button1));
  check "2P 'm' -> P2 Button2" ~expect:1
    ~got:(mapped Joypad.Two_player 'm' (Joypad.Two, Joypad.Button2));
  check "2P 'q' -> None" ~expect:1 ~got:(unmapped Joypad.Two_player 'q');
  (* release_all clears every button on both pads at once. *)
  Joypad.press j Joypad.One Joypad.Up;
  Joypad.press j Joypad.One Joypad.Button2;
  Joypad.press j Joypad.Two Joypad.Right;
  Joypad.release_all j;
  check "release_all -> A idle" ~expect:0xFF ~got:(pa ());
  check "release_all -> B idle" ~expect:0xFF ~got:(pb ());
  (* Control register: a write to 0x3F is stored and reads back. *)
  Joypad.write_control j (Uint8.of_int 0x55);
  check "write_control readback" ~expect:0x55
    ~got:(Uint8.to_int (Joypad.control j));
  if !failures = 0
  then Printf.printf "\njoypad: ALL PASS\n"
  else (
    Printf.printf "\njoypad: %d FAILED\n" !failures;
    exit 1)
;;
