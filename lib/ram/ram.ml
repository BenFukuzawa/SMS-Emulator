open Uints

type t =
  { bytes : Bytes.t
  ; start_addr : uint16
  ; end_addr : uint16
  }

(* Zeroed, not [Bytes.create]: uninitialised bytes are whatever was on the
   OCaml heap, so the console's power-on RAM would depend on the host's
   allocation history. A program that reads RAM before writing it -- which a
   real one does, and which a runaway one does constantly -- would then take
   a different path on the fifth run than on the first. Hardware powers up
   arbitrary, but an emulator that cannot reproduce its own runs is not
   debuggable. *)
let create ~start_addr ~end_addr =
  let size = Uint16.(to_int (end_addr - start_addr + one)) in
  { bytes = Bytes.make size '\x00'; start_addr; end_addr }
;;

let accepts t addr = Uint16.(t.start_addr <= addr && addr <= t.end_addr)

let read_byte t addr =
  let offset = Uint16.(addr - t.start_addr) |> Uint16.to_int in
  Bytes.unsafe_get t.bytes offset |> Uint8.of_char
;;

let write_byte t ~addr ~data =
  let offset = Uint16.(addr - t.start_addr) |> Uint16.to_int in
  Bytes.unsafe_set t.bytes offset (Uint8.to_char data)
;;
