open! Core
open! Async
open Uints

module type S = sig
  type t

  include Addressable_intf.S with type t := t

  val read_word : t -> uint16 -> uint8
  val write_word : t -> addr:uint16 -> data:uint8 -> unit
end
