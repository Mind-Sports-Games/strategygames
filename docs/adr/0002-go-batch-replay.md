# 0002 — Batch replay at the `Api.Position` seam

**Status:** **Superseded** by [0003](0003-go-rules-in-variant.md) (2026-08-08). The seam it decided
on, the batch path, the per-ply oracle and the engine below them are all deleted; go has one replay
path. Three of its rulings outlived it and 0003 states which, how each fared, and the evidence that
superseded the fourth:

- *a game resumed from a FEN counts only the actions it is handed* — **carried forward unchanged**
- *a pass cannot change the score* — **carried forward, and stronger**: it now holds structurally
  rather than by scheduling discipline
- *a drop-less game keeps `Score(0, 0)`* — **superseded on evidence.** It described an evaluation
  schedule rather than a rule of go, and the tree already contradicted it: the exported FEN read the
  position while `history.score` read zero at those same plies.
- the parked movegen options below are **moot**: they are optimisations of an engine that no longer
  exists.

Originally accepted 2026-07-25. The "Parked: engine-level movegen options" section was Proposed —
recorded for a later decision, never accepted for implementation.

## Context

The go speed dive ([docs/go-engine-speed-dive.md](../go-engine-speed-dive.md)) showed the
engine core was ~5% of production replay: the wrapper forced per-ply movegen and scoring,
re-replayed the whole prefix on every pass, and paid per-ply string planning. A batch
prototype cleared 100x the retired joansala engine at every board size. Before the batch
path could be called equivalent to the per-ply path, two behaviours the old paths disagreed
on had to become contract; and a further engine-level movegen prototype produced speedups no
production consumer can currently observe.

## Decision

`Replay.gameFromUciStrings` replays a game through a seam-level batch API and this is the
production default: `Api.positionFromVariantStartingFenAndMoves` plans the uci strings into
int moves in one pass, folds them through `BulkReplay.replay` on a mutable scratch state,
and publishes one final `Position`; wrapper assembly — `Board`/`Situation`/`Game`, pieceMap,
score, FEN — happens once in `go.Replay`, so no second engine-aware file exists. The per-ply
body is retained as `Replay.gameFromUciStringsPerPly`, the differential oracle the batch
path is measured against — it must never be redefined in terms of the batch path.
`Variant.validDrops` defers per-drop board construction into a lazy thunk instead of
applying every candidate drop eagerly.

Two rulings bind both replay paths:

- **Resumes count only the actions given.** A game resumed from a FEN takes pass state,
  turn and ply count from the FEN's own fields, once; the synthetic pass entries
  `Forsyth.<<@` seeds onto `board.uciMoves` are wrapper bookkeeping — never handed to the
  engine, never counted as plies, by any path — and `consecutivePasses` is single-counted.
  (The old pass path re-replayed the seeds, so the rendered full-move — and turn parity —
  depended on which kind of action came last.)
- **Pass plies do not rescore `history.score` — correct by construction, not a preserved
  bug.** A pass shares its predecessor's board array, area scoring reads only that array,
  and komi is constant per game, so a pass cannot change the score. Scoring evaluates once
  from the last scoring-bearing state (drop or replay `ss:`); a zero-drop game keeps
  `History.unscored` (`Score(0, 0)`) — a recorded contract, not an accident.

## Consequences

- Production replay clears 100x joansala at every size — 102x / 199x / 347x
  (9x9 / 13x13 / 19x19) — and the movegen consumer surface gains up to 57x from the
  validDrops deferral; tables in [docs/go-speed-results.md](../go-speed-results.md),
  protocol and full addendum in [docs/go-engine-speed-dive.md](../go-engine-speed-dive.md).
- The batch/per-ply equivalence suite must keep resumed-FEN cases (`passCount` 1–2 followed
  by a pass, a drop, and an `ss:`) and pass-terminated scoring cases — the corpora all start
  at `passCount` 0 and cannot catch either regression.
- Adding rescoring to the pass paths, or handing seeded entries to the engine as moves, is
  foreclosed unless this ADR is superseded.

## Parked: engine-level movegen options (Proposed, not accepted)

No production consumer is engine-movegen-bound — every caller goes through `validDrops`,
which is wrapper-dominated — so three measured engine options are parked behind one trigger:
an engine-internal movegen-bound consumer (go playouts, AI, or analysis-engine work).

- **A — incremental atari registry:** engine `legalDrops` 16–17x vs joansala @19x19, at a
  ~1.7x tax on apply — the operation replay leans on.
- **B — bitmask return contract (stacks on A):** ~28x, at the price of breaking the
  `Api.Position.legalDrops: Array[Int]` seam for a mutable-buffer discipline.
- **C — full bitboard core with mutable make/unmake state:** the only route past ~30x
  (floor ~0.4–0.5 µs @19x19); requires superseding ADR 0001's immutable contract for that
  consumer's path.

If the trigger fires, revisit in order A → B → C. Prototypes and parity evidence were
session artifacts preserved in git history; rebuild against the then-current engine rather
than treating the patches as durable storage.
