open Uints
open Instruction

module Make (Bus : Word_addressable_intf.S) = struct
    module RST_offset = struct
        let x00 = 0x00 |> Uint16.of_int
        let x08 = 0x08 |> Uint16.of_int
        let x10 = 0x10 |> Uint16.of_int
        let x18 = 0x18 |> Uint16.of_int
        let x20 = 0x20 |> Uint16.of_int
        let x28 = 0x28 |> Uint16.of_int
        let x30 = 0x30 |> Uint16.of_int
        let x38 = 0x38 |> Uint16.of_int
    end

    module Instruction_length = struct
        let l1 = 1 |> Uint16.of_int
        let l2 = 2 |> Uint16.of_int
        let l3 = 3 |> Uint16.of_int
        let l4 = 4 |> Uint16.of_int
    end
    let fields opcode =
        let x = opcode lsr 6 in
        let y = (opcode lsr 3) land 0x07 in
        let z = opcode land 0x07 in
        let p = y lsr 1 in
        let q = y land 0x01 in
        x, y, z, p, q

  let decode_base bus ~pc opcode =
    let open Instruction_length in
    let addr_after_pc = Uint16.succ pc in
    let next_byte () = Bus.read_byte bus addr_after_pc in
    let next_word () = Bus.read_word bus addr_after_pc in

    let x, y, z, p, q = fields opcode in

    match x with
        | 0 ->
        | 1 ->
        | 2 ->
        | 3 -> 
        