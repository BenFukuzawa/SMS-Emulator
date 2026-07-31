type t = Vdp.t

(* $BE: the data port. *)
let read_data = Vdp.read_data
let write_data = Vdp.write_data

(* $BF: commands going in, status coming out. The read is destructive -- it
   clears the interrupt flags and the control latch -- which is how a program
   acknowledges a VDP interrupt. *)
let read_control = Vdp.read_status
let write_control = Vdp.write_control

(* $7E and $7F: the raster counters. *)
let read_v_counter = Vdp.v_counter
let read_h_counter = Vdp.h_counter
