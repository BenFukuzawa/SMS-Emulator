(* Verifies SMS I/O port decoding by wiring the bus to stub peripherals that
   record what they were asked to do. *)
open Uints

module Test_vdp = struct
  type t =
    { mutable data_w : int
    ; mutable ctrl_w : int
    ; data_r : int
    ; ctrl_r : int
    ; v : int
    ; h : int
    }

  let create () =
    { data_w = -1
    ; ctrl_w = -1
    ; data_r = 0xD0
    ; ctrl_r = 0xC0
    ; v = 0x7E
    ; h = 0x7F
    }
  ;;

  let read_data t = Uint8.of_int t.data_r
  let write_data t b = t.data_w <- Uint8.to_int b
  let read_control t = Uint8.of_int t.ctrl_r
  let write_control t b = t.ctrl_w <- Uint8.to_int b
  let read_v_counter t = Uint8.of_int t.v
  let read_h_counter t = Uint8.of_int t.h
end

module Test_psg = struct
  type t = { mutable last : int }

  let create () = { last = -1 }
  let write t b = t.last <- Uint8.to_int b
end

module Test_joypad = struct
  type t =
    { mutable ctrl : int
    ; a : int
    ; b : int
    }

  let create () = { ctrl = -1; a = 0xAA; b = 0xBB }
  let read_port_a t = Uint8.of_int t.a
  let read_port_b t = Uint8.of_int t.b
  let write_control t b = t.ctrl <- Uint8.to_int b
end

module Io = Io_bus.Make (Test_vdp) (Test_psg) (Test_joypad)

let failures = ref 0

let check name ~expect ~got =
  if expect = got
  then Printf.printf "ok   %-24s 0x%02X\n" name got
  else (
    incr failures;
    Printf.printf "FAIL %-24s expected 0x%02X got 0x%02X\n" name expect got)
;;

let () =
  let vdp = Test_vdp.create () in
  let psg = Test_psg.create () in
  let joy = Test_joypad.create () in
  let io = Io.create ~vdp ~psg ~joypad:joy in
  let rd p = Uint8.to_int (Io.read_port io ~port:(Uint8.of_int p)) in
  let wr p v =
    Io.write_port io ~port:(Uint8.of_int p) ~data:(Uint8.of_int v)
  in
  (* reads *)
  check "0x7E V counter" ~expect:0x7E ~got:(rd 0x7E);
  check "0x7F H counter" ~expect:0x7F ~got:(rd 0x7F);
  check "0xBE VDP data" ~expect:0xD0 ~got:(rd 0xBE);
  check "0xBF VDP status" ~expect:0xC0 ~got:(rd 0xBF);
  check "0xDC joypad A" ~expect:0xAA ~got:(rd 0xDC);
  check "0xDD joypad B" ~expect:0xBB ~got:(rd 0xDD);
  check "0x00 open bus" ~expect:0xFF ~got:(rd 0x00);
  (* writes routed to the right peripheral *)
  wr 0xBE 0x11;
  check "0xBE -> VDP data" ~expect:0x11 ~got:vdp.data_w;
  wr 0xBF 0x22;
  check "0xBF -> VDP ctrl" ~expect:0x22 ~got:vdp.ctrl_w;
  wr 0x7F 0x33;
  check "0x7F -> PSG" ~expect:0x33 ~got:psg.last;
  wr 0x40 0x44;
  check "0x40 -> PSG" ~expect:0x44 ~got:psg.last;
  wr 0x3F 0x55;
  check "0x3F -> joypad ctrl" ~expect:0x55 ~got:joy.ctrl;
  if !failures = 0
  then Printf.printf "\nio_bus: ALL PASS\n"
  else (
    Printf.printf "\nio_bus: %d FAILED\n" !failures;
    exit 1)
;;
