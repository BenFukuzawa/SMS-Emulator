(** SN76489 programmable sound generator.

    Not implemented: this is a stub that drops what it is given. The chip is
    write-only from the CPU's side -- there is no register a program can read
    back to discover that nothing is listening -- so the only way a ROM can
    tell is by the silence. That makes it safe to assemble the machine around
    a stub and fill it in later.

    Any write to $40-$7F lands in [write]. *)

type t

val create : unit -> t

include Psg_intf.S with type t := t
