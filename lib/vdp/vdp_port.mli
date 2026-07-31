(** The VDP as the I/O bus sees it.

    [Vdp] names its entry points after what they do to the chip: a read of
    $BF hands back the status flags and clears them, so it is [read_status].
    [Vdp_intf.S] names them after the port they sit on, because a port number
    is all the bus knows. Both vocabularies are right for their own side;
    this module is the seam between them.

    No behaviour lives here. Every function is [Vdp]'s under another name,
    and the signature below is the interface itself, so the two cannot drift
    apart without breaking the build here rather than inside [Io_bus.Make]. *)

include Vdp_intf.S with type t = Vdp.t
