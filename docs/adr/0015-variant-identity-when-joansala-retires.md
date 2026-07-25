# 0015 — Variant identity when the joansala engine retires

**Status:** Proposed (2026-07-25)

## Context

The parallel go variants of 0001 exist so the pure-Scala engine can prove itself against
joansala before replacing it. When joansala is deleted (see "Retiring joansala" in
`docs/go-engine.md`), the variant ids stop lining up with the engines: `go9x9`/`go13x13`/
`go19x19` (ids 1, 2, 4) name joansala-backed rulesets, `go9x9Scala`/`go13x13Scala`/
`go19x19Scala` (ids 5, 6, 7) name the pure-Scala ones, and downstream applications hold
stored games, ratings and performance history keyed by the old ids.

The two rulesets are deliberately not identical: every divergence recorded under 0007
(empty-region scoring 0008, raw FEN counts 0009, superko refusal 0011, multi-digit row
parsing 0012, post-end behaviour 0013, positional superko 0014) means a stored joansala
game replayed under the scala engine can validate differently or score differently.

## Options

**A — Flip the old ids onto the scala engine.** Point ids 1, 2 and 4 at the pure-Scala
implementation and delete ids 5–7. Stored games keep their identity, urls and ratings;
nothing migrates. The cost is a retroactive rules change: every 0007 divergence now
applies to games that were played under joansala rules, so a replay of a stored game can
reject a move joansala accepted (superko, malformed rows) or report a different score
(empty regions, FEN counts).

**B — Delete the old ids and migrate app-side.** Ids 1, 2 and 4 die with the engine;
downstream applications migrate stored games, ratings and history onto ids 5–7 (or freeze
the old games as unreplayable archives). Replay honesty is preserved — no game is ever
re-interpreted under rules it was not played by — at the cost of a coordinated migration
in every consumer of this library.

## Decision

None yet. Recorded now so delete-day starts from the trade-off instead of rediscovering
it; the choice belongs to the joansala-deletion milestone and its downstream owners.

## Consequences

- Until decided, both variant families must stay live and `Variant.usesScalaEngine`
  remains the dispatch point (0002).
- Whichever option wins, the divergence table in `docs/go-engine.md` is the checklist of
  behaviours a stored-game audit must consider.
