type t

(* Build a cartridge from raw ROM bytes, or load them from a file. *)
val create : rom:bytes -> t
val of_file : string -> t

(* The Sega mapper's three 16 KB page registers and the RAM-control register,
   exposed for tests/debugging. *)
val pages : t -> int * int * int

include Addressable_intf.S with type t := t
