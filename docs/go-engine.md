# The go rules

Go's rules are concrete `def`s on `src/main/scala/go/variant/Variant.scala`. The variants
`go9x9`, `go13x13` and `go19x19` (ids 1, 2, 4) differ only in board size, komi and handicap
layout; everything else they inherit. There is no engine and no seam behind them
([ADR 0003](adr/0003-go-rules-in-variant.md)).

| | |
|---|---|
| `go/variant/Variant.scala` | the rules — legality, placement, passing, settlement, area scoring |
| `go/Chain.scala` | connectivity: chains, liberties, and what a placement captures |
| `go/Board.scala` | the position: stones, ko point, pass run, settlement flag, komi |
| `go/History.scala` | `positionHashes` (superko history) and `captures` |

## The rules, as implemented

**Legality.** A placement is legal when the point is empty, is not the ko point, is not suicide,
and does not recreate a position the game has already held. `Chain.capturesUnlessSuicide` answers
capture and suicide together — a capturing placement is never suicide, so the two cannot be asked
separately without producing an answer that is wrong for a recapture.

**Ko.** The simple-ko point is set when a placement captures exactly one stone, stands alone, and
has exactly one liberty. It is cleared by the next action of any kind, including a pass. The
coordinate is carried in the FEN and enforced on parse, which is what makes a game resumed from a
mid-ko FEN behave.

**Superko.** Positional, not situational: the side to move does not enter the position hash, so a
position is forbidden regardless of whose turn recreated it. Probed only on capturing placements,
because a non-capturing placement strictly increases the stone count and so cannot repeat.
`isRepetition` is therefore permanently false — no repeating position is reachable.

**Passing.** An even run of two or more consecutive passes opens dead-stone selection to the player
to move (`Situation.canSelectSquares`). Four consecutive passes end the game outright, with the
stones where they lie.

**Settlement.** The `ss:` action lifts the named stones and ends the game. It is sticky: no later
action makes a settled game unsettled, and both the played and the loaded path refuse an action
after one.

**Scoring.** Chinese area scoring — stones on the board plus empty regions a single colour
surrounds, with komi to p2. A region both colours touch, or no colour touches, scores for nobody;
that rule covers seki without special handling. Scores are carried in tenths of a point, because
komi is routinely a half point. Komi is per *game*, arriving in the FEN, not per variant.

## Adding a board size

Outside `variant/`, adding a size touches two places:

- `Board.BoardSize` — a new `case object` and an entry in `BoardSize.all`. The neighbour table and
  the valid-point list derive themselves from width and height.
- `FEN.variant` — the row-count to variant match. A go FEN names only its board size, so this is
  how a FEN with no variant attached is read; it is unambiguous only while each size has exactly one
  variant.

Plus the usual `Go*` variant file with fresh `id`/`perfId` values. Go variant ids 3, 5, 6, 7 and
perfIds 503/504/505 are retired and must never be reused.

Two things that used to need editing and no longer do: `Forsyth.validate` derives the acceptable
sizes from `Board.BoardSize.all`, and the position hash is one table over the whole 19x19 point
space rather than a table per size.

The `Pos`/`File` alphabet tops out at 19 files, which is the ceiling for any new size.

## Where it differed from joansala

joansala was retired in 2026-07 ([ADR 0001](adr/0001-pure-scala-go-engine.md)). Where the two
disagreed the rules won, so a stored joansala-era game can validate or score differently on replay.
This table is the checklist for a downstream stored-game audit; every row still holds.

| | joansala (retired) | today |
|---|---|---|
| Empty region bordered by no colour | all white's | nobody's |
| Raw FEN captures and pass count | hardcoded `0` | the real counts |
| Ko point and handicap scores in FEN | did not round-trip | reproduced, and a ko coordinate is accepted on parse and enforced |
| A move repeating a position | offered, then ended the game as a repetition | refused, so `isRepetition` is permanently false |
| Board row with a run of ten or more | reader split the digits and truncated overruns | multi-digit runs round-trip, overruns are rejected |
| A finished game | still listed legal actions | lists none |
| An unchecked replay of an illegal move | replayed it blindly | rejected, now as `Validated.invalid` rather than a throw |
| Superko rule | none enforced | positional, not situational |

The ko field of an emitted FEN carries the live simple-ko coordinate (`-` otherwise); joansala only
ever wrote `-`, so stored FENs never carry a coordinate and parse unchanged.

## Behaviours kept that look like bugs

Four, each preserved because stored games were written under them. The reason sits at each code
site, and [the refactor write-up](go-refactor.md) states what fixing each one would take.

| | where |
|---|---|
| An `ss:` records one capture more than the stones it lifts | `Replay.settlementCaptureCount` |
| A settlement records those captures on the two loaders that fold action strings (`Replay.gameFromUciStrings`, `pgn.Reader`), and none on the two that replay a `Uci` list (`Replay.apply(List[Uci], …)`, `Replay.situationsFromUci`) or on a game played live | `Replay.withSettlementCaptures` vs `Variant.boardAfterSelectSquares` |
| An off-board `ss:` key is ignored, where an off-board drop key is an error | `Replay.gameWithActionWhileValid` |
| A game p2 settled renders its full-move number with a `1` concatenated on, not added | `Forsyth.fullMovePart` |

## Replaying a game

One path. `Replay.gameWithActionWhileValid` folds the action strings into games one ply at a time,
and `Replay.apply`, `gameFromUciStrings` and `gameWithUciWhileValid` all go through it. `pgn.Reader`
keeps its own fold because it has to report a partial replay, but it builds every action through
the same `Replay.replayDrop` / `replayPass` / `replaySelectSquares` helpers, so legality is decided
in exactly one place: `Situation.drop` → `Variant.drop`.

A game resumed from a FEN counts only the actions it is handed: the FEN's own fields are the sole
authority for everything before the resume point
([ADR 0003](adr/0003-go-rules-in-variant.md), carried forward from ADR 0002).

`Replay.boards` and `Replay.situations` are dead for go — `pgn.Parser.sans` is a stub that refuses
every go action string, so their bodies have never run. Pre-existing, and out of scope for the
refactor that found it; `situationsFromUci` is the working equivalent.

## Benchmarks

`GoRulesBenchmark` (JMH) times replay, placement, legal-drop generation, area scoring and FEN
parse/render across the three sizes; `GoSmokeTiming` answers the same questions in wall-clock
seconds when a JMH run is too slow to be worth it. Both live in `bench/`, which documents how to run
them. Results: [docs/go-speed-results.md](go-speed-results.md).
