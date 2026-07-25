# 0008 — An empty region bordered by no colour scores for nobody

**Status:** Accepted (2026-07-24)

## Context

Under the area scoring of 0006 an empty region counts for a colour iff every stone
bordering it is that colour. Exactly one position per board size has a region bordered by
*no* colour at all: the initial position (any placed stone borders its own region).

joansala resolves the no-colour case in white's favour and awards the entire empty board
as white territory. The pure-Scala engine treats it as belonging to neither player.

Probe — `Api.positionFromVariant(v)`, `(p1Score, p2Score)` on the initial position:

| size | joansala | scala |
|------|----------|-------|
| 9x9 | `(0.0, 86.5)` | `(0.0, 5.5)` |
| 13x13 | `(0.0, 176.5)` | `(0.0, 7.5)` |
| 19x19 | `(0.0, 368.5)` | `(0.0, 7.5)` |

The scala values are komi only.

## Decision

A region with no bordering colour is neutral: it scores for neither player. No fix is
applied to the new engine — it is the correct side of this divergence.

## Alternatives

- **Match joansala (award the region to white)** — would score a game nobody has played as
  white by 86.5 on 9x9, and would contradict the repo's own data (below). Rejected.

## Consequences

- The repo's hardcoded `variant.initialFen` values agree with the scala engine
  (`... b - 0 55 0 0 55 0 1` for 9x9), so `Api.positionFromVariant(Go9x9Scala).fen ===
  Go9x9.initialFen`, whereas the joansala position does not round-trip its own variant's
  initial FEN. The divergence therefore also removes a pre-existing internal inconsistency.
- Contract-visible in principle (`p1Score`/`p2Score`), but only on positions that contain a
  no-colour region — in practice the initial position, where no result depends on it.
  Because the initial FEN already carries the scala numbers, the *wrapper* FEN agrees on
  both engines from the first move onward.
- Pinned from both sides in `src/test/scala/go/GoDifferentialTest.scala`, which asserts the
  joansala values too, so drift on either engine fails loudly.
