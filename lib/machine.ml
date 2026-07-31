open Uints

(* --- controller ---------------------------------------------------------

   Both pads live in Joypad, which owns the port $DC/$DD bit layout including
   the awkward part -- player 2's inputs are split across the two ports. The
   board only forwards.

   Pause is not a pad button: it is on the console itself and wired to the
   CPU's NMI, which is why [pause] below does not go through here.

   These re-export the pad's types so that a frontend can drive the machine
   without naming Joypad. The keyboard mapping deliberately stays out: this
   module knows nothing about keys. *)

type player = Joypad.player =
  | One
  | Two

type button = Joypad.button =
  | Up
  | Down
  | Left
  | Right
  | Button1
  | Button2

(* --- the board ---------------------------------------------------------- *)

module Mem = Mem_bus.Make (Cartridge)
module Io = Io_bus.Make (Vdp_port) (Psg) (Joypad)
module Cpu = Z80.Make (Mem) (Io)

type t =
  { cpu : Cpu.t
  ; vdp : Vdp.t
  ; joypad : Joypad.t
  ; bus : Mem.t
  }

let create ~rom =
  let cartridge = Cartridge.create ~rom in
  (* 8 KB at $C000; the $E000-$FFFF mirror is the bus's job, not the RAM's. *)
  let ram =
    Ram.create
      ~start_addr:(Uint16.of_int 0xC000)
      ~end_addr:(Uint16.of_int 0xDFFF)
  in
  let bus = Mem.create ~cartridge ~ram in
  let vdp = Vdp.create () in
  let psg = Psg.create () in
  let joypad = Joypad.create () in
  let io = Io.create ~vdp ~psg ~joypad in
  let cpu = Cpu.create ~bus ~io ~registers:(Registers.create ()) in
  { cpu; vdp; joypad; bus }
;;

(* One frame is 262 lines of 228 cycles (vdp.ml). The bound is a backstop: an
   instruction that reported zero cycles would otherwise spin here forever,
   and in a browser that means a hung tab rather than a failed test. Hitting
   it leaves the frame half-drawn and the next call picks up where this one
   stopped. *)
let max_cycles_per_frame = 262 * 228 * 4

(* The CPU and the VDP share a clock, and T-states are the common currency:
   whatever an instruction cost, the VDP is advanced by exactly that much
   before the next one starts.

   The interrupt line is read back every instruction rather than latched. It
   is level triggered -- the VDP holds it high until the program reads the
   status port -- so a single check after an instruction that raised it would
   miss the moment it drops. *)
let run_frame t =
  let start = Vdp.frame_count t.vdp in
  let spent = ref 0 in
  while Vdp.frame_count t.vdp = start && !spent < max_cycles_per_frame do
    let cycles = Cpu.run_instruction t.cpu in
    spent := !spent + cycles;
    Vdp.step t.vdp ~cycles;
    Cpu.set_irq_line t.cpu (Vdp.irq t.vdp)
  done
;;

let framebuffer t = Vdp.framebuffer t.vdp
let frame_size t = Vdp.frame_size t.vdp
let frame_count t = Vdp.frame_count t.vdp
let press t player button = Joypad.press t.joypad player button
let release t player button = Joypad.release t.joypad player button
let release_all t = Joypad.release_all t.joypad

(* The Pause button is wired to the CPU's NMI, not to the controller port. *)
let pause t = Cpu.request_nmi t.cpu

module For_tests = struct
  let read_byte t addr =
    Uint8.to_int (Mem.read_byte t.bus (Uint16.of_int addr))
  ;;
end
