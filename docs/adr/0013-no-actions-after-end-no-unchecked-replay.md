# 0013 — A finished game offers no actions, and no action is applied unchecked

**Status:** Accepted (2026-07-24)

## Context

Two places where joansala will hand back, or act on, an action it should not. Both are
below the wrapper contract; both were probed directly on `Api.Position`.

**Legal actions after the game has ended.** Probe: the 12-move scripted 9x9 game plus
`pass pass ss:i1` — game over, P1 wins 15.0 to 11.5.

| | joansala | scala |
|---|---|---|
| `apiPosition.legalActions.size` | `71` | `0` |

joansala keeps listing drops and the pass after `hasEnded()` is true. (An earlier
known-divergence note claimed joansala also returned none; the probe in WO-11 disproved it.)

**Unchecked replay.** Probe:
`Api.positionFromVariant(v).makeMovesNoLegalCheck(List("s@e5", "s@e5"))`.

| | joansala | scala |
|---|---|---|
| result | replayed blindly; second placement is a no-op, `pieceMap` size stays 1 | throws — `GoState.apply` rejects the illegal move |

## Decision

The scala engine never reports an action it would not accept, and never accepts one it
considers illegal. A finished game has no legal actions, so `legalActions` is empty once
`ended`. `makeMovesNoLegalCheck` skips *filtering*, not *validation*: `GoState.apply`
still rejects an illegal move, with an `IllegalArgumentException`.

## Alternatives

- **List actions after the end** — invites a caller to play on past a decided result.
  Rejected by 0007.
- **Make `makeMovesNoLegalCheck` literally unchecked** — the fast path would then diverge
  from the legal path on malformed input and corrupt state silently instead of failing.
  Rejected; the name promises a skipped filter, and skipping the filter is all the speed
  there is to win. The one in-repo caller path (`Board.afterDrop` / `validDrops`) only ever
  feeds it actions it has already generated as legal.

## Consequences

- Neither divergence is contract-visible. `go.variant.Variant.possibleDrops` returns `None`
  once `situation.end`, so both wrappers offer no drops on a finished game; and nothing in
  the repo feeds `makeMovesNoLegalCheck` an illegal action.
- `variant.specialEnd` is `legalActions.isEmpty || gameEnd`. On joansala those clauses are
  independent; on the scala engine the first is implied by the second, so they are mutually
  reinforcing rather than two separate end conditions. Nothing depends on them being
  independent, but a future reader should not infer from the `||` that the scala engine can
  run out of actions without the game having ended.
- A caller reaching past the seam gets an exception rather than a quietly wrong position —
  the intended failure mode.
