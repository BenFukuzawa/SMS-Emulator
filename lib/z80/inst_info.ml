open Uints

type tcycles =
  { taken : int
  ; not_taken : int
  }

type t =
  { len : uint16
  ; tcycles : tcycles
  ; inst : Instruction.t
  }
