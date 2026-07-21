open! Core
open! Async
open Uints

module type S = sig
  type t

  include Addressable_intf.S with type t := t

  val read_16_byte : t -> uint16 -> uint8
  val write_16_byte : t -> addr:uint16 -> data:uint8 -> unit
end
