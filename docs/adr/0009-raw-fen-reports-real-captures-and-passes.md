# 0009 — Raw engine FEN reports real capture counts and pass count

**Status:** Accepted (2026-07-24)

## Context

The FEN of 0006 has ten fields:

```
board[pocket] turn ko p1Score p2Score p1Captures p2Captures komi passCount fullMove
```

joansala hardcodes three of them at the `Api.Position.fenString` level: fields 5 and 6
(p1/p2 captures) are always `0 0`, and field 9 (`passCount`) is always `0`. The wrapper
hides this — `strategygames.go.format.Forsyth.exportBoardFen` overwrites the capture fields
from `board.history.captures` and the pass count from `board.uciMoves` before anyone sees
them.

Probes:

| probe | joansala raw FEN | scala raw FEN |
|-------|------------------|---------------|
| 12-move scripted 9x9 game in which white captures d5 | captures `0 0` | captures `0 1` |
| two consecutive passes | passCount `0` | passCount `2` |

Wrapper FENs are byte-identical on both engines in both probes (`0 1`, and `2`).

The pure-Scala engine has no wrapper to lean on: `GoFen.parse` is the entry point that
reconstructs a `GoState` from a FEN, so the numbers it emits must be the numbers it needs
back.

## Decision

The scala engine's raw FEN states the truth: real capture counts from
`GoState.capturesByBlack`/`capturesByWhite`, real pass count from
`GoGame.fenPassCount`. joansala's hardcoded zeroes are not reproduced.

## Alternatives

- **Reproduce the hardcoded zeroes** — makes the engine's own FEN lossy, so
  `parse(render(g))` loses capture counts and, worse, the pass count that distinguishes
  ongoing play from dead-stone selection (`passCount >= 3` ⇒ `deadStonesSelected`).
  Rejected: 0006 requires FEN round-trip, and the wrapper overwrite means bug-for-bug
  parity buys nothing.

## Consequences

- Not observable through the contract surface: every wrapper FEN is byte-identical between
  engines, because `Forsyth.exportBoardFen` overwrites all three fields anyway.
- Observable to anything reading `Api.Position.fenString` directly. In-repo that is the
  differential test and the engine itself; the PlayStrategy app reads wrapper FENs.
- The scala engine's FEN round-trips through its own parser, which the joansala FEN does
  not; see also 0010 for the two fields that round-trip on neither engine.
