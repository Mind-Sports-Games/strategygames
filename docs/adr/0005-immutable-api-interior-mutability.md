# 0005 — Immutable public API with contained interior mutability

**Status:** Accepted (2026-07-24)

## Context

The codebase is functional and immutable by design (project CLAUDE.md); consumers of
`Api.Position` expect value semantics — Situation/Game hold positions across turns and
replays fork freely. The old engine satisfied this with `deepCopy` of a mutable joansala
board, which shows up as a hot-path cost. The new engine's arrays (0003) are cheap to copy
but expensive to rebuild persistently per move.

## Decision

The engine's state (`GoState`) is an immutable value with a pure public API:
`apply(move)` copies the backing arrays and mutates only the private copy before it
escapes. That one memcpy (a few KB at 19x19) is the entire per-move copying cost; no
`deepCopy` and no other cloning exists on the hot path. Mutation never crosses the API
boundary: no method exposes or writes the internal arrays, and a published `GoState` is
never written again.

## Alternatives

- **Persistent structures throughout (Vector, HashMap)** — indirection and allocation on
  every probe of the hottest loops; defeats the point of 0003. Rejected.
- **Mutable engine + defensive `deepCopy` at the seam** — the old engine's shape; copy
  cost lands on every consumer touchpoint instead of once per apply, and forgetting a copy
  is an aliasing bug. Rejected.
- **Mutable API with undo (make/unmake)** — fastest, but pushes imperative protocol onto
  every consumer of a codebase whose contract is value semantics. Rejected.

## Consequences

- Consumers keep value semantics: sharing, replay forking, and multi-action turns need no
  copying discipline.
- Per-move cost is one array copy; profiling accepted this against the ≥5x target.
- The copy-then-mutate discipline is invariant-critical and local to the engine package:
  every mutating helper operates only on arrays the current call owns.
- Persistent members of the state (hash history per 0004, cached legal moves) share
  structure for free; lazily computed fields are cached per immutable state.
