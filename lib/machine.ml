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
  ; psg : Psg.t
  ; joypad : Joypad.t
  ; bus : Mem.t
  ; (* The board has no use for the cartridge once it is wired to the bus. It
       is kept only so [For_debug] can read the mapper's page registers,
       which are the one part of the cart not reachable through [Mem]. *)
    cartridge : Cartridge.t
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
  { cpu; vdp; psg; joypad; bus; cartridge }
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
let step t =
  let cycles = Cpu.run_instruction t.cpu in
  Vdp.step t.vdp ~cycles;
  Psg.step t.psg ~cycles;
  Cpu.set_irq_line t.cpu (Vdp.irq t.vdp);
  cycles
;;

let run_frame t =
  let start = Vdp.frame_count t.vdp in
  let spent = ref 0 in
  while Vdp.frame_count t.vdp = start && !spent < max_cycles_per_frame do
    spent := !spent + step t
  done
;;

(* Same backstop as [run_frame], scaled to one line's worth of cycles. A
   scanline is 228 T-states, so the multiplier leaves room for an instruction
   that straddles the boundary without letting a zero-cycle instruction spin. *)
let step_scanline t =
  let start = Vdp.For_tests.line t.vdp in
  let spent = ref 0 in
  while Vdp.For_tests.line t.vdp = start && !spent < 228 * 4 do
    spent := !spent + step t
  done
;;

(* Sound comes out in the same currency as the picture: one call per frame,
   after run_frame, giving whatever the chip generated while that frame ran.
   Roughly 735 samples at 44.1 kHz and 60 Hz. *)
let audio t = Psg.take t.psg
let audio_rate t = Psg.sample_rate t.psg
let audio_pending t = Psg.pending t.psg
let drop_audio t = Psg.drop t.psg
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

(* Everything an out-of-band observer needs, and nothing it could change
   with. Reads only: [Mem.read_byte] has no side effects (the mapper snoops
   the write path, not the read path), so a debugger walking memory cannot
   perturb the run it is describing.

   Separate from [For_tests] because the two have different obligations. A
   test peephole may be narrowed whenever the test that wanted it goes away;
   this is a surface a frontend is built on. *)
module For_debug = struct
  let read_byte = For_tests.read_byte
  let vdp t = t.vdp
  let registers t = Cpu.For_tests.registers t.cpu
  let pc t = Uint16.to_int (Cpu.For_tests.pc t.cpu)
  let mapper_pages t = Cartridge.pages t.cartridge

  let interrupt_state t =
    let iff1, iff2, im, i, refresh, halted =
      Cpu.For_tests.interrupt_state t.cpu
    in
    iff1, iff2, im, Uint8.to_int i, Uint8.to_int refresh, halted
  ;;
end
