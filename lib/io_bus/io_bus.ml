open Uints

(* SMS I/O port decoding.

   The console only decodes address lines A7, A6 and A0 for I/O, giving four
   ranges (by A7/A6) each split into even/odd (by A0):

     0x00-0x3F  write even -> memory control (0x3E)
                write odd  -> I/O port control (0x3F)
                read       -> open bus (0xFF)
     0x40-0x7F  write      -> PSG
                read even  -> V counter        read odd -> H counter
     0x80-0xBF  even       -> VDP data (0xBE)  odd      -> VDP control (0xBF)
     0xC0-0xFF  read even  -> joypad port A/B (0xDC)
                read odd   -> joypad port B (0xDD)
                write      -> no effect *)
module Make (Vdp : Vdp_intf.S) (Psg : Psg_intf.S) (Joypad : Joypad_intf.S) =
struct
  type t =
    { vdp : Vdp.t
    ; psg : Psg.t
    ; joypad : Joypad.t
    ; mutable memory_control : uint8 (* port 0x3E *)
    }

  let create ~vdp ~psg ~joypad =
    { vdp; psg; joypad; memory_control = Uint8.zero }
  ;;

  let read_port t ~port =
    let p = Uint8.to_int port in
    match p land 0xC0 with
    | 0x00 -> Uint8.of_int 0xFF
    | 0x40 ->
      if p land 1 = 0
      then Vdp.read_v_counter t.vdp
      else Vdp.read_h_counter t.vdp
    | 0x80 ->
      if p land 1 = 0 then Vdp.read_data t.vdp else Vdp.read_control t.vdp
    | _ (* 0xC0 *) ->
      if p land 1 = 0
      then Joypad.read_port_a t.joypad
      else Joypad.read_port_b t.joypad
  ;;

  let write_port t ~port ~data =
    let p = Uint8.to_int port in
    match p land 0xC0 with
    | 0x00 ->
      if p land 1 = 0
      then t.memory_control <- data
      else Joypad.write_control t.joypad data
    | 0x40 -> Psg.write t.psg data
    | 0x80 ->
      if p land 1 = 0
      then Vdp.write_data t.vdp data
      else Vdp.write_control t.vdp data
    | _ (* 0xC0 *) -> ()
  ;;
end
