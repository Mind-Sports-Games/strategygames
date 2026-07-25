# 0012 — Board rows use multi-digit empty runs and reject overruns

**Status:** Accepted (2026-07-24)

## Context

Upstream `BasicTests.toGoBoardFromStringTest2` pins joansala's `DiagramConverter`, whose
reader and writer disagree:

- the **reader** treats every digit character as its own empty run, so `199` is 1+9+9 = 19
  empties and `7X119` is 7 + X + 1 + 1 + 9 — and it silently discards whatever falls past
  the end of a row;
- the **writer** emits multi-digit run lengths — `19` for a full empty 19x19 row.

The consequence is that joansala diagrams containing a run of ten or more do not round-trip
through joansala's own parser. Upstream's own expectation records this: `199/…/4X191O2`
parses, and re-emits as `19/…/4X11O2`.

The board field of the FEN in 0006 is the same encoding, and the app, stored games, and the
differential corpus all contain 19x19 rows with runs of ten or more.

## Decision

`GoFen` reads and writes multi-digit run lengths, so parse/render is an exact round-trip.
A row whose runs overrun the board width is rejected with
`GoFenError.MalformedBoardRow(row, expected, found)` instead of being silently truncated,
and so is a row that falls short (`file != size`).

## Alternatives

- **Copy the single-digit reader** — makes `parse(render(board))` wrong for any 19x19 board
  with an empty row, which is most of them early in a game. Rejected outright: this is not
  a ruleset choice, it is a defect that the writer itself contradicts.
- **Truncate silently on overrun** — turns a corrupt FEN into a plausible-looking wrong
  board. Rejected; a parser that cannot recognize the whole input must not act on part of
  it.

## Consequences

- The scala engine parses a strict superset-free subset: every FEN either round-trips or is
  rejected with a located error, never silently reinterpreted.
- A diagram written by joansala's writer parses correctly on the scala side; a diagram
  written for joansala's *reader* (relying on `199` meaning 19) does not, and will be
  rejected rather than misread. No such diagram exists in the repo's fixtures.
- Ported as the upstream-suite cases "reject a row whose runs overflow the board" and "read
  multi digit empty runs and re-emit them" in
  `src/test/scala/go/engine/UpstreamJoansalaPortedTest.scala`, both marked as
  rules-correct divergences.
