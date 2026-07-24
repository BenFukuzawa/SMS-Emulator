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

  let add_to_pc pc offset = Uint16.add pc (Uint16.of_int offset)
  let read_byte_at bus ~pc ~offset = Bus.read_byte bus (add_to_pc pc offset)
  let read_word_at bus ~pc ~offset = Bus.read_word bus (add_to_pc pc offset)

  let mk ~len ~t inst : Inst_info.t =
    let t = t + adj in
    { len; tcycles = { taken = t; not_taken = t }; inst }
  ;;

  let fields opcode =
    let x = opcode lsr 6 in
    let y = (opcode lsr 3) land 0x07 in
    let z = opcode land 0x07 in
    let p = y lsr 1 in
    let q = y land 0x01 in
    x, y, z, p, q
  ;;

  let decode_base bus ~pc opcode ~prefix ~base =
    let open Instruction_length in
    let next_byte () = read_byte_at bus ~pc ~offset:1 in
    let next_word () = read_word_at bus ~pc ~offset:1 in
    let r idx =
      Lookup.r ~prefix:Lookup.No_prefix ~touches_mem:false ~next_byte idx
    in
    let rp idx = Lookup.rp ~prefix:Lookup.No_prefix idx in
    let rp2 idx = Lookup.rp2 ~prefix:Lookup.No_prefix idx in
    let cc idx = Lookup.cc.(idx) in
    let x, y, z, p, q = fields opcode in
    match x with
    | 0 ->
      (match z with
       | 0 ->
         (match y with
          | 0 ->
            { len = l1; tcycles = { taken = 4; not_taken = 4 }; inst = NOP }
          | 1 ->
            { len = l1
            ; tcycles = { taken = 4; not_taken = 4 }
            ; inst = EX_AF_AF
            }
          | 2 ->
            { len = l2
            ; tcycles = { taken = 13; not_taken = 8 }
            ; inst = DJNZ (Int8.of_byte (next_byte ()))
            }
          | 3 ->
            { len = l2
            ; tcycles = { taken = 12; not_taken = 12 }
            ; inst = JR (None, Int8.of_byte (next_byte ()))
            }
          | 4 | 5 | 6 | 7 ->
            { len = l2
            ; tcycles = { taken = 12; not_taken = 7 }
            ; inst = JR (cc (y - 4), Int8.of_byte (next_byte ()))
            }
          | _ -> assert false
          | 1 ->
            (match q with
             | 0 ->
               { len = l3
               ; tcycles = { taken = 10; not_taken = 10 }
               ; inst = LD16 (RR (rp p), Immediate16 (next_word ()))
               }
             | 1 ->
               { len = l1
               ; tcycles = { taken = 11; not_taken = 11 }
               ; inst = ADD16 (RR Registers.HL, RR (rp p))
               }
             | 2 ->
               (match q with
                | 0 ->
                  (match p with
                   | 0 ->
                     { len = l1
                     ; tcycles = { taken = 7; not_taken = 7 }
                     ; inst = LD8 (RR_indirect Registers.BC, R Registers.A)
                     }
                   | 1 ->
                     { len = l1
                     ; tcycles = { taken = 7; not_taken = 7 }
                     ; inst = LD8 (RR_indirect Registers.DE, R Registers.A)
                     }
                   | 2 ->
                     { len = l3
                     ; tcycles = { taken = 16; not_taken = 16 }
                     ; inst = LD16 (Direct16 (next_word ()), RR Registers.HL)
                     }
                   | 3 ->
                     { len = l3
                     ; tcycles = { taken = 13; not_taken = 13 }
                     ; inst = LD8 (Direct8 (next_word ()), R Registers.A)
                     }
                   | _ -> assert false
                   | 1 ->
                     (match p with
                      | 0 ->
                        { len = l1
                        ; tcycles = { taken = 7; not_taken = 7 }
                        ; inst = LD8 (R Registers.A, RR_indirect Registers.BC)
                        }
                      | 1 ->
                        { len = l1
                        ; tcycles = { taken = 7; not_taken = 7 }
                        ; inst = LD8 (R Registers.A, RR_indirect Registers.DE)
                        }
                      | 2 ->
                        { len = l3
                        ; tcycles = { taken = 16; not_taken = 16 }
                        ; inst =
                            LD16 (RR Registers.HL, Direct16 (next_word ()))
                        }
                      | 3 ->
                        { len = l3
                        ; tcycles = { taken = 13; not_taken = 13 }
                        ; inst = LD8 (R Registers.A, Direct8 (next_word ()))
                        }
                      | _ -> assert false
                      | _ -> assert false
                      | 3 ->
                        (match q with
                         | 0 ->
                           { len = l1
                           ; tcycles = { taken = 6; not_taken = 6 }
                           ; inst = INC16 (RR (rp p))
                           }
                         | 1 ->
                           { len = l1
                           ; tcycles = { taken = 6; not_taken = 6 }
                           ; inst = DEC16 (RR (rp p))
                           }
                         | _ -> assert false
                         | 4 ->
                           let operand = r y in
                           { len = l1
                           ; tcycles =
                               (if y = 6
                                then { taken = 11; not_taken = 11 }
                                else { taken = 4; not_taken = 4 })
                           ; inst = INC8 operand
                           }
                         | 5 ->
                           { len = l1
                           ; tcycles =
                               (if y = 6
                                then { taken = 11; not_taken = 11 }
                                else { taken = 4; not_taken = 4 })
                           ; inst = DEC8 (r y)
                           }
                         | 6 ->
                           { len = l2
                           ; tcycles =
                               (if y = 6
                                then { taken = 10; not_taken = 10 }
                                else { taken = 7; not_taken = 7 })
                           ; inst = LD8 (r y, Immediate8 (next_byte ()))
                           }
                         | 7 ->
                           { len = l1
                           ; tcycles = { taken = 4; not_taken = 4 }
                           ; inst =
                               (match y with
                                | 0 -> RLCA
                                | 1 -> RRCA
                                | 2 -> RLA
                                | 3 -> RRA
                                | 4 -> DAA
                                | 5 -> CPL
                                | 6 -> SCF
                                | 7 -> CCF
                                | _ -> assert false)
                           }
                         | 1 ->
                           (match z with
                            | 6 ->
                              (match y with
                               | 6 ->
                                 { len = l1
                                 ; tcycles = { taken = 4; not_taken = 4 }
                                 ; inst = HALT
                                 }
                               | _ ->
                                 { len = l1
                                 ; tcycles =
                                     (if y = 6 || z = 6
                                      then { taken = 7; not_taken = 7 }
                                      else { taken = 4; not_taken = 4 })
                                 ; inst = LD8 (r y, r z)
                                 }
                               | _ ->
                                 { len = l1
                                 ; tcycles =
                                     (if y = 6 || z = 6
                                      then { taken = 7; not_taken = 7 }
                                      else { taken = 4; not_taken = 4 })
                                 ; inst = LD8 (r y, r z)
                                 }
                               | 2 ->
                                 { len = l1
                                 ; tcycles =
                                     (if z = 6
                                      then { taken = 7; not_taken = 7 }
                                      else { taken = 4; not_taken = 4 })
                                 ; inst = Lookup.alu.(y) (r z)
                                 }
                               | 3 ->
                                 (match z with
                                  | 0 ->
                                    { len = l1
                                    ; tcycles = { taken = 11; not_taken = 5 }
                                    ; inst = RET (Some (cc y))
                                    }
                                  | 1 ->
                                    (match q with
                                     | 0 ->
                                       { len = l1
                                       ; tcycles =
                                           { taken = 10; not_taken = 10 }
                                       ; inst = POP (rp2 p)
                                       }
                                     | 1 ->
                                       (match p with
                                        | 0 ->
                                          { len = l1
                                          ; tcycles =
                                              { taken = 10; not_taken = 10 }
                                          ; inst = RET None
                                          }
                                        | 1 ->
                                          { len = l1
                                          ; tcycles =
                                              { taken = 4; not_taken = 4 }
                                          ; inst = EXX
                                          }
                                        | 2 ->
                                          { len = l1
                                          ; tcycles =
                                              { taken = 4; not_taken = 4 }
                                          ; inst = JP_indirect Registers.HL
                                          }
                                        | 3 ->
                                          { len = l1
                                          ; tcycles =
                                              { taken = 6; not_taken = 6 }
                                          ; inst =
                                              LD16
                                                ( RR Registers.SP
                                                , RR Registers.HL )
                                          }
                                        | _ -> assert false
                                        | 2 ->
                                          { len = l3
                                          ; tcycles =
                                              { taken = 10; not_taken = 10 }
                                          ; inst =
                                              JP (Some (cc y), next_word ())
                                          }
                                        | 3 ->
                                          (match y with
                                           | 0 ->
                                             { len = l3
                                             ; tcycles =
                                                 { taken = 10
                                                 ; not_taken = 10
                                                 }
                                             ; inst = JP (None, next_word ())
                                             }
                                           | 1 -> assert false
                                           | 2 ->
                                             { len = l2
                                             ; tcycles =
                                                 { taken = 11
                                                 ; not_taken = 11
                                                 }
                                             ; inst =
                                                 OUT
                                                   ( Immediate8 (next_byte ())
                                                   , R Registers.A )
                                             }
                                           | 3 ->
                                             { len = l2
                                             ; tcycles =
                                                 { taken = 11
                                                 ; not_taken = 11
                                                 }
                                             ; inst =
                                                 IN
                                                   ( R Registers.A
                                                   , Immediate8
                                                       (next_byte ()) )
                                             }
                                           | 4 ->
                                             { len = l1
                                             ; tcycles =
                                                 { taken = 19
                                                 ; not_taken = 19
                                                 }
                                             ; inst =
                                                 EX_SP_indirect Registers.HL
                                             }
                                           | 5 ->
                                             { len = l1
                                             ; tcycles =
                                                 { taken = 4; not_taken = 4 }
                                             ; inst = EX_DE_HL
                                             }
                                           | 6 ->
                                             { len = l1
                                             ; tcycles =
                                                 { taken = 4; not_taken = 4 }
                                             ; inst = DI
                                             }
                                           | 7 ->
                                             { len = l1
                                             ; tcycles =
                                                 { taken = 4; not_taken = 4 }
                                             ; inst = EI
                                             }
                                           | _ -> assert false
                                           | 4 ->
                                             { len = l3
                                             ; tcycles =
                                                 { taken = 17
                                                 ; not_taken = 10
                                                 }
                                             ; inst =
                                                 CALL
                                                   (Some (cc y), next_word ())
                                             }
                                           | 5 ->
                                             (match q with
                                              | 0 ->
                                                { len = l1
                                                ; tcycles =
                                                    { taken = 11
                                                    ; not_taken = 11
                                                    }
                                                ; inst = PUSH (rp2 p)
                                                }
                                              | 1 ->
                                                (match p with
                                                 | 0 ->
                                                   { len = l3
                                                   ; tcycles =
                                                       { taken = 17
                                                       ; not_taken = 17
                                                       }
                                                   ; inst =
                                                       CALL
                                                         (None, next_word ())
                                                   }
                                                 | 1 | 2 | 3 -> assert false
                                                 | _ -> assert false
                                                 | _ -> assert false
                                                 | 6 ->
                                                   { len = l2
                                                   ; tcycles =
                                                       { taken = 7
                                                       ; not_taken = 7
                                                       }
                                                   ; inst =
                                                       Lookup.alu.(y)
                                                         (Immediate8
                                                            (next_byte ()))
                                                   }
                                                 | 7 ->
                                                   { len = l1
                                                   ; tcycles =
                                                       { taken = 11
                                                       ; not_taken = 11
                                                       }
                                                   ; inst =
                                                       RST
                                                         (Uint16.of_int
                                                            (y * 8))
                                                   }
                                                 | _ -> assert false
                                                 | _ -> assert false)))))))))))))))
  ;;

  let decode_index_cb bus ~pc ~cb_offset index =
    let open Instruction_length in
    let displacement = read_byte_at bus ~pc ~offset:(cb_offset + 1) in
    let opcode =
      read_byte_at bus ~pc ~offset:(cb_offset + 2) |> Uint8.to_int
    in
    let x, y, z, _, _ = fields opcode in
    let indexed_operand =
      Indexed_indirect (index, Int8.of_byte displacement)
    in
    let len = Uint16.of_int (cb_offset + 3) in
    let destination =
      if z = 6
      then None
      else
        Some
          (Lookup.r
             ~prefix:Lookup.No_prefix
             ~touches_mem:false
             ~next_byte:(fun () -> failwith "index CB: no extra byte")
             z)
    in
    match x with
    | 0 ->
      { len
      ; tcycles = { taken = 23; not_taken = 23 }
      ; inst = Lookup.rot.(y) indexed_operand destination
      }
    | 1 ->
      { len
      ; tcycles = { taken = 20; not_taken = 20 }
      ; inst = BIT (y, indexed_operand)
      }
    | 2 ->
      { len
      ; tcycles = { taken = 23; not_taken = 23 }
      ; inst = RES (y, indexed_operand, destination)
      }
    | 3 ->
      { len
      ; tcycles = { taken = 23; not_taken = 23 }
      ; inst = SET (y, indexed_operand, destination)
      }
    | _ -> assert false
  ;;

  let decode_index_opcode bus ~pc ~opcode_offset index opcode : Inst_info.t =
    let open Instruction_length in
    let x, y, z, p, q = fields opcode in
    let slots =
      match x with
      | 0 when z >= 4 && z <= 6 -> [ y ] (* INC r[y], DEC r[y], LD r[y],n *)
      | 1 when not (y = 6 && z = 6) ->
        [ y; z ] (* LD r[y],r[z]; HALT excluded *)
      | 2 -> [ z ] (* alu[y] r[z] *)
      | _ -> []
    in
    let touches_mem = List.mem 6 slots in
    let imm_at = opcode_offset + 1 + if touches_mem then 1 else 0 in
    let next_byte () = read_byte_at bus ~pc ~offset:imm_at in
    let next_word () = read_word_at bus ~pc ~offset:imm_at in
    (* index is Lookup.IX / Lookup.IY, used directly as the prefix. *)
    let r idx =
      Lookup.r ~prefix:index ~touches_mem ~next_byte:read_disp idx
    in
    let rp idx = Lookup.rp ~prefix:index idx in
    let rp2 idx = Lookup.rp2 ~prefix:index idx in
    (* HL named explicitly -> IX/IY as a 16-bit operand or a bare register *)
    let ix_rr : Registers.rr =
      match index with Lookup.IX -> Registers.IX | _ -> Registers.IY
    in
    let ix : uint16 arg = RR ix_rr in
    (* base timing is the UNPREFIXED value; adj adds the delta. *)
    let disp_bytes = if touches_mem then 1 else 0 in
    let len ~imm = Uint16.of_int (opcode_offset + 1 + disp_bytes + imm) in
    let adj =
      if touches_mem
      then if x = 0 && z = 6 then 9 else 12
      else 4 * opcode_offset
    in
    match x, z, q, p with
    (* x=0 *)
    | 0, 1, 0, _ ->
      mk
        ~len:(len ~imm:2)
        ~t:10
        (LD16 (RR (rp p), Immediate16 (next_word ())))
    | 0, 1, 1, _ -> mk ~len:(len ~imm:0) ~t:11 (ADD16 (ix, RR (rp p)))
    | 0, 2, 0, 2 ->
      mk ~len:(len ~imm:2) ~t:16 (LD16 (Direct16 (next_word ()), ix))
    | 0, 2, 1, 2 ->
      mk ~len:(len ~imm:2) ~t:16 (LD16 (ix, Direct16 (next_word ())))
    | 0, 3, 0, _ -> mk ~len:(len ~imm:0) ~t:6 (INC16 (RR (rp p)))
    | 0, 3, 1, _ -> mk ~len:(len ~imm:0) ~t:6 (DEC16 (RR (rp p)))
    | 0, 4, _, _ ->
      mk ~len:(len ~imm:0) ~t:(if y = 6 then 11 else 4) (INC8 (r y))
    | 0, 5, _, _ ->
      mk ~len:(len ~imm:0) ~t:(if y = 6 then 11 else 4) (DEC8 (r y))
    | 0, 6, _, _ ->
      mk
        ~len:(len ~imm:1)
        ~t:(if y = 6 then 10 else 7)
        (LD8 (r y, Immediate8 (next_byte ())))
    | 1, 6, _, _ when y = 6 -> mk ~len:(len ~imm:0) ~t:4 HALT
    | 1, _, _, _ ->
      mk ~len:(len ~imm:0) ~t:(if touches_mem then 7 else 4) (LD8 (r y, r z))
    | 2, _, _, _ ->
      mk ~len:(len ~imm:0) ~t:(if z = 6 then 7 else 4) (Lookup.alu.(y) (r z))
    (* x=3 : only the explicit-HL opcodes change *)
    | 3, 1, 0, _ -> mk ~len:(len ~imm:0) ~t:10 (POP (RR (rp2 p)))
    | 3, 1, 1, 2 -> mk ~len:(len ~imm:0) ~t:4 (JP_indirect ix_rr)
    | 3, 1, 1, 3 -> mk ~len:(len ~imm:0) ~t:6 (LD16 (RR Registers.SP, ix))
    | 3, 3, _, _ when y = 4 ->
      mk ~len:(len ~imm:0) ~t:19 (EX_SP_indirect ix_rr)
    | 3, 5, 0, _ -> mk ~len:(len ~imm:0) ~t:11 (PUSH (RR (rp2 p)))
    | _ -> assert false
  ;;

  let decode_cb bus ~pc : Inst_info.t =
    let open Instruction_length in
    let opcode = read_byte_at bus ~pc ~offset:1 |> Uint8.to_int in
    let x, y, z, _, _ = fields opcode in
    let operand =
      Lookup.r
        ~prefix:Lookup.No_prefix
        ~touches_mem:false
        ~next_byte:(fun () -> failwith "CB: no displacement byte")
        z
    in
    let is_mem = z = 6 in
    let t = if is_mem then 15 else 8 in
    match x with
    | 0 ->
      { len = l2
      ; tcycles = { taken = t; not_taken = t }
      ; inst = Lookup.rot.(y) operand None
      }
    | 1 ->
      let t = if is_mem then 12 else 8 in
      { len = l2
      ; tcycles = { taken = t; not_taken = t }
      ; inst = BIT (y, operand)
      }
    | 2 ->
      { len = l2
      ; tcycles = { taken = t; not_taken = t }
      ; inst = RES (y, operand, None)
      }
    | 3 ->
      { len = l2
      ; tcycles = { taken = t; not_taken = t }
      ; inst = SET (y, operand, None)
      }
    | _ -> assert false
  ;;

  let decode_ed_at_offset bus ~pc ~prefix_offset : Inst_info.t =
    let open Instruction_length in
    let opcode =
      read_byte_at bus ~pc ~offset:(prefix_offset + 1) |> Uint8.to_int
    in
    let x, y, z, p, q = fields opcode in
    let normal_len = Uint16.of_int (prefix_offset + 2) in
    let word_len = Uint16.of_int (prefix_offset + 4) in
    let next_word = read_word_at bus ~pc ~offset:(prefix_offset + 2) in
    let register_of_y = function
      | 0 -> Registers.B
      | 1 -> Registers.C
      | 2 -> Registers.D
      | 3 -> Registers.E
      | 4 -> Registers.H
      | 5 -> Registers.L
      | 6 -> failwith "y = 6 has no register"
      | 7 -> Registers.A
      | _ -> assert false
    in
    let rr idx : uint16 Instruction.arg = RR (rp ~prefix:No_prefix idx) in
    match x with
    | 1 ->
      (match z with
       | 0 ->
         { len = normal_len
         ; tcycles = { taken = 12; not_taken = 12 }
         ; inst =
             (if y = 6
              then IN (None, Port_C)
              else IN (Some (register_of_y y), Port_C))
         }
       | 1 ->
         { len = normal_len
         ; tcycles = { taken = 12; not_taken = 12 }
         ; inst =
             (if y = 6
              then OUT (Port_C, Out_zero)
              else OUT (Port_C, Out_register (register_of_y y)))
         })
       | 2 ->
         { len = normal_len
         ; tcycles = { taken = 15; not_taken = 15 }
         ; inst =
             (if q = 0
              then SBC16 (RR Registers.HL, rr p)
              else ADC16 (RR Registers.HL, rr p))
         }
       | 3 ->
         { len = word_len
         ; tcycles = { taken = 20; not_taken = 20 }
         ; inst =
             (if q = 0
              then LD16 (Direct16 next_word, rr p)
              else LD16 (rr p, Direct16 next_word))
         }
       | 4 ->
         { len = normal_len
         ; tcycles = { taken = 8; not_taken = 8 }
         ; inst = NEG
         }
       | 5 ->
         { len = normal_len
         ; tcycles = { taken = 14; not_taken = 14 }
         ; inst = (if y = 1 then RETI else RETN)
         }
       | 6 ->
         { len = normal_len
         ; tcycles = { taken = 8; not_taken = 8 }
         ; inst = IM Lookup.im.(y)
         }
       | 7 ->
         { len = normal_len
         ; tcycles = { taken = 9; not_taken = 9 }
         ; inst =
             (match y with
              | 0 -> LD_I_A
              | 1 -> LD_R_A
              | 2 -> LD_A_I
              | 3 -> LD_A_R
              | 4 | 6 -> RRD
              | 5 | 7 -> RLD
              | _ -> assert false)
         }
       | _ -> assert false
       | 2 ->
         if z <= 3 && y >= 4
         then
           { len = normal_len
           ; tcycles =
               { taken = (if z >= 2 then 16 else 21)
               ; not_taken = (if z >= 2 then 16 else 16)
               }
           ; inst = Lookup.bli_lookup y z
           }
         else assert false
       | _ -> assert false)
  ;;

  let decode_ed bus ~pc : Inst_info.t =
    decode_ed_at_offset bus ~pc ~prefix_offset:0
  ;;

  let rec decode_index bus ~pc ~opcode_offset index =
    let opcode =
      read_byte_at bus ~pc ~offset:opcode_offset |> Uint8.to_int
    in
    match opcode with
    | 0xDD -> decode_index bus ~pc ~opcode_offset:(opcode_offset + 1) IX
    | 0xFD -> decode_index bus ~pc ~opcode_offset:(opcode_offset + 1) IY
    | 0xED -> decode_ed_at_offset bus ~pc ~prefix_offset:opcode_offset
    | 0xCB -> decode_index_cb bus ~pc ~cb_offset:opcode_offset index
    | opcode -> decode_index_opcode bus ~pc ~opcode_offset index opcode
  ;;

  (* final function *)
  let f bus ~pc : Inst_info.t =
    let opcode = Bus.read_byte bus pc |> Uint8.to_int in
    match opcode with
    | 0xCB -> decode_cb bus ~pc
    | 0xED -> decode_ed bus ~pc
    | 0xDD -> decode_index bus ~pc ~opcode_offset:1 IX
    | 0xFD -> decode_index bus ~pc ~opcode_offset:1 IY
    | opcode -> decode_base bus ~pc opcode
  ;;
end
