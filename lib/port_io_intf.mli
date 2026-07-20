open Uints

module type S = sig
  type t
  val read_port  : t -> uint8 -> uint8
  val write_port : t -> port:uint8 -> data:uint8 -> unit
end