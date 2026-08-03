# Live debug view — implementation plan

A second browser page that runs a ROM and shows the machine's insides while it
runs: register state, disassembly at PC, the tilemap and sprite table as their
own panels, and a VRAM tile sheet updating in real time.

Branch: `ben-debug-view`. The existing player page (`web/index.html` +
`web/main.ml`) is left untouched.

## Shape

```
lib/machine.mli        + For_debug (read-only inspection) + step
lib/debug/debug.ml     derivation: disassembly walk, tile decode, sprite table
test/debug_test.ml     headless assertions on the above
web/debug.ml           the DOM half: panels, rAF loop, step controls
web/debug.html         layout + styling
```

The split matters: everything that *derives* something (decode a tile, walk a
disassembly, resolve a CRAM entry to RGB) lives in `lib/debug` where it can be
tested without a browser. `web/debug.ml` only puts strings and pixels into the
DOM.

## Phase 1 — inspection surface on `Machine`

`Machine` currently exposes a frame and a framebuffer and nothing else; the
debugger needs to see inside and to advance in smaller steps.

**1a. Single-instruction stepping.** `run_frame`'s loop body becomes a named
function and gets exposed:

```ocaml
(** Fetch, decode and execute one instruction (or service an interrupt),
    then advance the VDP by exactly the T-states it cost. Returns that
    count. This is the unit [run_frame] is built from. *)
val step : t -> int
```

`run_frame` becomes `while not done do ignore (step t) done` — same behaviour,
same cycle accounting, no duplicated interrupt-line logic. Also worth adding
`val step_scanline : t -> unit` (run until `Vdp.For_tests.line` changes), which
is what you actually want when watching a raster effect.

**1b. `Machine.For_debug`**, mirroring the existing `For_tests` convention:

```ocaml
module For_debug : sig
  val read_byte : t -> int -> int          (* through mapper + RAM mirror *)
  val registers : t -> Registers.t
  val pc : t -> int
  val interrupt_state : t -> bool * bool * int * int * int * bool
  val vdp : t -> Vdp.t
  val mapper_pages : t -> int * int * int
end
```

All read-only. `Mem.read_byte` is already side-effect free (mapper registers
are write-only; the snoop in `mem_bus.ml` is on the write path), so reading
memory for a debugger cannot perturb the run. `Z80.For_tests` and
`Vdp.For_tests` already expose everything else needed — `pc`, `registers`,
`interrupt_state`, `vram_byte`, `cram_entry`, `register`, `line`,
`name_table_base`, `sprite_attr_base`, `sprite_pattern_base`. No new
peephole into the VDP is required.

Open question for review: `For_tests` vs `For_debug` naming. These are the same
kind of thing — a deliberate crack in the abstraction for an out-of-band
observer. I'd rather add `For_debug` than widen `For_tests`, because the two
have different stability expectations, but it's a judgement call.

## Phase 2 — `lib/debug/debug.ml`

Add `debug` to the `(dirs ...)` list in `lib/dune` — it is an explicit
allowlist, so a new subdirectory is otherwise silently not built.

**Disassembly.** `Fetch_and_decode.Make` is a functor over a bus, and
`Inst_info.t` already carries `len` and `inst`, with `Instruction.show` for the
text. So the walk is: decode at PC, print, advance by `len`, repeat.

The trick that makes this safe is to instantiate the decoder over a *read-only
adapter* rather than the real bus:

```ocaml
module Snoop = struct
  type t = Machine.t
  let read_byte m a = Uint8.of_int (Machine.For_debug.read_byte m (Uint16.to_int a))
  let write_byte _ ~addr:_ ~data:_ = ()   (* the decoder never writes *)
  ...
end
module Decode = Fetch_and_decode.Make (Snoop)
```

The type system then guarantees the disassembly view cannot mutate the machine,
which is the property you most want from a debugger.

**The decoder can raise, and this must be handled.** `fetch_and_decode.ml` has
`assert false` on unreachable opcode fields and one `failwith "index CB: no
extra byte"`; `lookup.ml` uses `invalid_arg`. Those are unreachable for the
*real* PC but very reachable when walking forward past the end of a routine
into data — which happens constantly. Each decode is wrapped, and a failure
emits `DB $xx` and advances one byte. Without this the panel throws roughly
once a frame.

```ocaml
type line =
  { addr : int
  ; bytes : int list
  ; text : string   (* "JR Z, +$0C", or "DB $C9" on a decode failure *)
  ; current : bool  (* addr = PC *)
  }

val disassemble : Machine.t -> at:int -> count:int -> line list
```

Two known limits, both inherent to linear disassembly and both fine to ship:
data inside a code stream desynchronises the following few lines until it
happens to resync, and there is no back-scroll (you cannot decode *backwards*
on a variable-length ISA without a trace). Showing a couple of lines of history
would need a small ring buffer of executed PCs — a good follow-up, not part of
this.

**Tiles.** `pattern_pixel` in `vdp.ml` is private, so `Debug` re-implements the
four-bitplane decode over `Vdp.For_tests.vram_byte`. 512 tiles of 8×8:

```ocaml
val tile_pixels : Vdp.t -> tile:int -> Bytes.t          (* 64 palette indices *)
val tile_sheet  : Vdp.t -> palette:int -> Bytes.t       (* 256x128 RGB, 32 tiles/row *)
val cram_rgb    : Vdp.t -> int -> int                   (* entry -> 0xRRGGBB *)
```

`cram_rgb`'s `expand2` (`v * 85`) is duplicated from `vdp.ml`. Small enough to
copy, but the honest fix is to expose the VDP's own — a divergence here would
mean the tile viewer lies about colour. I'd expose it.

**Tilemap.** Read the name table at `name_table_base` — 32×28 entries of two
bytes, `---pcvhnnnnnnnnn` little-endian — and give back tile index, palette,
flip bits and priority per cell, plus a composed RGB image so the panel can
draw the whole 256×224 map with the visible-viewport rectangle overlaid at
(R8, R9). Seeing the scroll window move across the map is the single most
explanatory thing on the page.

**Sprites.** Read the SAT at `sprite_attr_base`: Y at `base + i`, X and pattern
at `base + 0x80 + 2i`. Stop at the `$D0` terminator and mark it, as the mockup
does. Report `shift_sprites`, `tall_sprites` and `zoom_sprites` too, since they
change how the listed numbers should be read.

## Phase 3 — headless test

`test/debug_test.ml`, in the style of the existing harnesses, against the
hand-assembled ROM already used by `machine_test.ml`:

- `disassemble` at reset returns the mnemonics that ROM was written from
- `step` advances PC by the decoded instruction's `len` for a straight-line run
- `step` accumulated over a frame equals `run_frame`'s cycle count
- decoding a byte sequence that is not a valid instruction yields `DB $xx` and
  advances one, rather than raising
- a hand-written VRAM pattern decodes to the expected 64 palette indices

This is the part that keeps the debug view from quietly drifting away from the
emulator it is describing.

## Phase 4 — `web/debug.ml` + `web/debug.html`

Two `(executable)` stanzas in `web/dune` with disjoint `(modules)`, so
`main.bc.js` and `debug.bc.js` are built separately and the player page carries
none of this weight. `dune build @web/serve` gains `debug.html` and
`debug.bc.js` as deps.

Panels, following the mockup: screen with run/pause/step/frame controls, CPU
registers with flags as `SZxHyPNC` letters lit or dimmed, disassembly with the
PC line highlighted, VRAM tile sheet, CRAM strip, sprite table. Plus the
tilemap panel with the viewport rectangle, which the mockup doesn't show but is
the one that explains the most.

Controls: **step** (one instruction), **frame** (one full frame), **scanline**,
run/pause. Reuse `main.ml`'s keyboard and ROM-loading code as-is — the pad has
to keep working or you cannot reach anything interesting in Sonic.

## Phase 5 — making it fast enough to be honest

This is the real risk, and it should be treated as part of the work rather than
as polish. A full-speed frame is ~60,000 T-states; the panels above are a few
thousand DOM writes and ~90,000 decoded tile pixels. Done naively at 60 Hz it
will not hold frame rate, and a debug view that makes the game run at 20 fps
misrepresents the machine it is showing.

- **Tier the update rates.** Screen every frame. Registers and disassembly
  every frame *while paused or stepping*, ~10 Hz while running (they are
  unreadable at 60 Hz anyway). VRAM sheet, tilemap and CRAM at ~5–10 Hz.
- **Write to few nodes.** One `textContent` assignment per panel against a
  fixed layout, not a node per register. The disassembly is 16 pre-created rows
  whose text is overwritten, never rebuilt.
- **Reuse the `ImageData`.** One allocation per canvas, held across frames —
  `createImageData` per frame is the main cost in the current `render`.
- **Skip unchanged work.** Hash the VRAM/CRAM cheaply, or dirty-flag them on
  writes, and skip the redraw when nothing moved. Most frames touch very
  little VRAM.
- **Measure, and say so.** Show the achieved fps in a corner. If the panels
  cost frames, the user should be able to see that they do.

Order of work: Phase 1 → 2 → 3 gets a tested, headless debug core; Phase 4
makes it visible; Phase 5 makes it usable. Phases 1–3 are safe to land on their
own.
