# 0011 — Superko is refused up front, so `isRepetition` is permanently false

**Status:** Accepted (2026-07-24)

## Context

0004 chose to forbid a superko move at generation time. joansala instead permits board
repetition and reports it afterwards: the repeating move stays in `legalDrops`, playing it
sets `isRepetition = true`, and the wrapper turns that into a variant end / draw. This ADR
records what that timing difference does to the observable state, which is a separate
question from *which* superko rule applies (0014).

Probe A — `s@c6 s@c5 s@c7 s@d6 s@e6 s@e5 s@e7 s@d4 s@d8 s@e4 pass s@d7 s@d5` on 9x9 (the
sequence of `GoSituationTest` "Test repetition drops are not valid"):

| | joansala | scala |
|---|---|---|
| `legalDrops` contains d6 | yes (move 48) | no |
| playing d6 | accepted, `isRepetition = true` | rejected |
| wrapper `situation.drops` | 68, d6 absent | 68, d6 absent |

Probe B — upstream `BasicTests.notRepetitionOnPassTest`, 9x9 moves
`80 71 79 70 69 62 77 78 79 53 80 78 79 pass`. Upstream plays all fourteen plies and asserts
`game.isRepetition() == false`; the scala engine refuses ply 13 (the returning capture)
outright. A pass is never refused and never records a hash, so the trailing pass is
unaffected.

The wrapper reaches the same answer by a different road: `go.variant.Variant.validDrops`
plays each joansala candidate and drops it if `nextBoard.apiPosition.isRepetition`.

## Decision

The scala engine refuses the move. `isRepetition` on `ScalaPosition` is therefore a
permanent `false` — not a stub, but a true statement about an engine in which no repeating
position can ever be reached.

## Alternatives

- **Report repetition after the fact, as joansala does** — a rules engine that lets an
  illegal move be played and then narrates the illegality. Rejected by 0007.
- **Forbid *and* keep a live `isRepetition`** — nothing could ever set it. Rejected as dead
  state.

## Consequences

- `go.variant.Variant.specialDraw` is
  `scores equal || apiPosition.isRepetition`; on the scala variants only the first clause
  can fire. Repetition draws do not exist there — the cycle is prevented instead of scored.
- `variant.validDrops`' `isRepetition` filter is a no-op on the scala engine (its
  `legalDrops` already excludes the move) and load-bearing on joansala. Both engines offer
  the same wrapper drops, so this divergence is **not contract-visible** — with the one
  exception of the parity case in 0014.
- Where the raw legal sets differ, `GoDifferentialTest` asserts the difference explicitly
  via `superkoActionsByPly` rather than tolerating it.
- Consumers must not read `isRepetition == false` on a `go*Scala` position as "no cycle was
  attempted"; it means "no cycle was permitted".
