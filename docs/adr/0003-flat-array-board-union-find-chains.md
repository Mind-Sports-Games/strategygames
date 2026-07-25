# 0003 — Flat padded byte-array board with union-find chains and pseudo-liberties

**Status:** Accepted (2026-07-24)

## Context

The engine (0001) must serve 9x9, 13x13, and 19x19 from one implementation and beat the
joansala engine by ≥5x on legal-move generation and full-game replay. The hot operations
are neighbor iteration, capture detection (does a chain have zero liberties?), and merging
chains on placement.

## Decision

- **Board:** one flat `Array[Byte]`, padded grid of stride N+2 with BORDER sentinel cells
  around the playing area; point states EMPTY/BLACK/WHITE/BORDER. A single layout
  parameterized by N serves all three sizes. Neighbor offsets are the four constants
  ±1, ±(N+2); the sentinel border removes all edge bounds checks.
- **Chains:** union-find over point indices (parent `Array[Int]`) plus a circular
  next-stone list per chain, with per-chain stone count and **pseudo-liberty** count
  (each empty neighbor counted once per adjacent stone, so a liberty shared by k stones
  counts k times). Pseudo-liberties == 0 is exact for "chain has no liberties", which is
  the only question capture detection asks; true liberty counts are computed only where
  actually needed.

## Alternatives

- **Bitboards** — 19x19 needs 361 bits (6 Longs); flood-fill and chain merge become
  multi-word shift/mask choreography for no win at these sizes, and per-chain liberty
  tracking still needs auxiliary structures. Rejected.
- **Object graph (Chain/Stone classes, sets of Pos)** — allocation and pointer-chasing on
  the hot path; this is roughly the shape the joansala engine already has. Rejected.
- **Unpadded array with bounds checks** — four branch tests per neighbor visit in the
  innermost loop; the sentinel border buys the same safety branch-free. Rejected.

## Consequences

- Placement, capture, and merge are O(α) array operations; state copies (see 0005) are a
  few KB of memcpy even at 19x19.
- Pseudo-liberties cannot answer "exactly one liberty?" directly; self-atari/capture
  logic that needs real liberty counts must count them explicitly for that chain.
- Point indices, not `Pos`, are the engine's working coordinates; conversion happens at the
  seam. Shared go `Pos` support for ranks 10–19 (`Pos.fromKey("a19")`) is verified in tests.
