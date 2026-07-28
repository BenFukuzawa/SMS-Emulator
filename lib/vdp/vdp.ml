open Uints

type t =
  { vram : Uint8.t array
  ; cram : Uint8.t array
  ; registers : Uint8.t array
  ; mutable address : int
  ; mutable mode : access_mode
  ; mutable first_control_byte : Uint8.t option
  ; mutable read_buffer : Uint8.t
  ; mutable status : Uint8.t
  }
