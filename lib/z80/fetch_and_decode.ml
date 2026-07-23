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
    type index_register =
    | IX
    | IY
    let add_to_pc pc offset =
        Uint16.add pc (Uint16.of_int offset)

    let read_byte_at bus ~pc ~offset =
        Bus.read_byte bus (add_to_pc pc offset)

    let read_word_at bus ~pc ~offset =
        Bus.read_word bus (add_to_pc pc offset)

    let fields opcode =
        let x = opcode lsr 6 in
        let y = (opcode lsr 3) land 0x07 in
        let z = opcode land 0x07 in
        let p = y lsr 1 in
        let q = y land 0x01 in
        x, y, z, p, q
    let decode_base bus ~pc opcode =
        let open Instruction_length in
        let next_byte () =
            read_byte_at bus ~pc ~offset:1
        in
        let next_word () =
            read_word_at bus ~pc ~offset:1
        in
        let r idx =
            Lookup.r
            ~prefix:Lookup.No_prefix
            ~next_byte
            idx
        in

        let rp idx =
            Lookup.rp
            ~prefix:Lookup.No_prefix
            idx
        in

        let rp2 idx =
            Lookup.rp2
            ~prefix:Lookup.No_prefix
            idx
        in
        let cc idx =
            Lookup.cc.(idx)
        in
        let x, y, z, p, q = fields opcode in

        match x with
            | 0 ->
                match z with 
                    | 0 ->
                        match y with 
                            | 0-> {
                                len = l1;
                                tcycles = { not_branched = 1; branched = 1 };
                                inst = NOP;
                                }
                            | 1 ->
                            | 2 ->
                            | 3->
                            | _ ->
                    | 1 ->
                        match q with 
                            | 0-> 
                            | 1->
                    | 2 ->
                        match q with 
                            | 0-> 
                                match p with
                                    | 0
                                    | 1
                                    | 2
                                    | 3
                            | 1->
                                match p with
                                    | 0
                                    | 1
                                    | 2
                                    | 3
                    | 3 ->
                        match q with 
                            | 0 ->
                            | 1 ->
                    | 4 ->
                    | 5 ->
                    | 6 ->
                    | 7 ->
                        match y with 
                            | 0 ->
                            | 1 ->
                            | 2 ->
                            | 3 ->
                            | 4 ->
                            | 5 ->
                            | 6 ->
                            | 7 ->
            | 1 ->
                match z with
                    | 6 ->
                        match y with 
                            | 6->
                            | _-> 
                    | _ ->
            | 2 ->
            | 3 -> 
                match z with 
                    | 0 ->
                    | 1 ->
                        match q with 
                            | 0->
                            | 1 -> 
                                match p with
                                    | 0 ->
                                    | 1->
                                    | 2-> 
                                    | 3->
                    | 2 ->
                    | 3 ->
                        match y with 
                            | 0 ->
                            | 1 ->
                            | 2 ->
                            | 3 ->
                            | 4 ->
                            | 5 ->
                            | 6 ->
                            | 7 ->
                    | 4 ->
                    | 5 ->
                        match q with 
                            | 0->
                            | 1 -> 
                                match p with
                                    | 0 ->
                                    | 1->
                                    | 2-> 
                                    | 3->
                    | 6 ->
                    | 7 ->
    let decode_index_cb bus ~pc ~cb_offset index =
        let open Instruction_length in

        let displacement =
            read_byte_at bus ~pc ~offset:(cb_offset + 1)
        in

        let opcode = read_byte_at bus ~pc ~offset:(cb_offset + 2)
        |> Uint8.to_int
        in

        let x, y, z, _, _ = fields opcode in

        match index, x with
        | IX, 0 ->
            match z with 
                | 6 -> 
                | _->
        | IX, 1 ->
        | IX, 2 ->
            match z with 
                | 6 -> 
                | _->
        | IX, 3 ->
            match z with 
                | 6 -> 
                | _->
        | IY, 0 ->
            match z with 
                | 6 -> 
                | _->
        | IY, 1 ->
        | IY, 2 ->
            match z with 
                | 6 -> 
                | _->
        | IY, 3 ->
            match z with 
                | 6 -> 
                | _->
        | _, _ ->
            assert false 
    let decode_index_opcode bus ~pc ~opcode_offset index opcode =
        let open Instruction_length in
        let x, y, z, p, q = fields opcode in
        match index with
            | IX ->
            | IY ->

    let decode_cb bus ~pc =
        let open Instruction_length in
        let opcode =
            read_byte_at bus ~pc ~offset:1
            |> Uint8.to_int
        in

        let x, y, z, _,_ = fields opcode in
        match x with 
            | 0 ->
            | 1->
            | 2->
            | 3->
            | _-> assert false
    let decode_ed_at_offset bus ~pc ~prefix_offset : Inst_info.t =
        let open Instruction_length in
        let opcode =
            read_byte_at bus ~pc ~offset:(prefix_offset + 1)
            |> Uint8.to_int
        in
        let x, y, z, _, q = fields opcode in
        match x with 
            | 1 -> 
                match z with 
                    | 0-> 
                        match y with
                            | 6->
                            | _ ->
                    | 1->
                        match y with
                            | 6->
                            | _ -> 
                    | 2-> 
                        match q with 
                            | 0->
                            | 1-> 
                    | 3->
                        match q with 
                            | 0->
                            | 1-> 
                    | 4->
                    | 5->
                        match y with
                            | 1->
                            | _-> 
                    | 6->
                    | 7->
                        match y with 
                            | 0 ->
                            | 1 ->
                            | 2 ->
                            | 3 ->
                            | 4 ->
                            | 5 ->
                            | 6 ->
                            | 7 ->

            | 2 ->
                match z<=3 with 
                    | true -> 
                        match y>=4 with 
                            | true ->
                            | false -> assert false 
                    |false ->
            | _-> assert false 
    let decode_ed bus ~pc : Inst_info.t =
        decode_ed_at_offset bus ~pc ~prefix_offset:0

    let rec decode_index bus ~pc ~opcode_offset index =
        let opcode =
        read_byte_at bus ~pc ~offset:opcode_offset
        |> Uint8.to_int
        in

        match opcode with
        | 0xDD ->
            decode_index
            bus
            ~pc
            ~opcode_offset:(opcode_offset + 1)
            IX

        | 0xFD ->
            decode_index
            bus
            ~pc
            ~opcode_offset:(opcode_offset + 1)
            IY

        | 0xED ->
            decode_ed_at_offset
            bus
            ~pc
            ~prefix_offset:opcode_offset

        | 0xCB ->
            decode_index_cb
            bus
            ~pc
            ~cb_offset:opcode_offset
            index
        | opcode ->
            decode_index_opcode
            bus
            ~pc
            ~opcode_offset
            index
            opcode

(* final function *)
    let f bus ~pc : Inst_info.t =
        let opcode =
        Bus.read_byte bus pc
        |> Uint8.to_int
        in

        match opcode with
            | 0xCB ->
                decode_cb bus ~pc

            | 0xED ->
                decode_ed bus ~pc

            | 0xDD ->
                decode_index
                bus
                ~pc
                ~opcode_offset:1
                IX

            | 0xFD ->
                decode_index
                bus
                ~pc
                ~opcode_offset:1
                IY

            | opcode ->
                decode_base bus ~pc opcode
        end
                