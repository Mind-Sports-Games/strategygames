# The pure-Scala Go engine

Go runs on the pure-Scala engine in `src/main/scala/go/engine/`: the variants `go9x9`, `go13x13`
and `go19x19` (ids 1, 2, 4) all build their positions from it. It replaced the external joansala
engine ([ADR 0001](adr/0001-pure-scala-go-engine.md)).

## Layout

`strategygames.go.Api.Position` is the single seam: Board, Situation, Game, Forsyth and Replay all
consume it and none of them knows the engine behind it.

A go FEN names only its board size, so `go.format.FEN.variant` infers the canonical variant of that
size — unambiguous now that each size has exactly one variant. A caller already holding a variant
should pass it (`Api.positionFromVariantNameAndFEN`, `Api.positionFromVariantStartingFenAndMoves`)
rather than infer it back from the FEN.

| | |
|---|---|
| `go/ScalaPosition.scala` | the seam's implementation — UCI strings in, `PieceMap`/FEN/legal actions out |
| `go/engine/GoState.scala` | a position: board, chains, side to move, position-hash history, and the rules |
| `go/engine/GoGame.scala` | a state plus komi, ply count, and whether dead stones have been agreed |
| `go/engine/GoFen.scala` | the FEN dialect |
| `go/engine/Zobrist.scala` | the per-size stone hash tables |
| `go/engine/BulkReplay.scala` | a whole move sequence folded on one mutable scratch state, frozen once |

Below the seam the board is a flat `(N+2)²` byte array with a sentinel border, chains are union-find
with pseudo-liberty counts, and the position hash is maintained incrementally. The public API is
immutable: a move copies the arrays and mutates only the copy, so a published state is never
written again. Scoring is Chinese area scoring, dead stones are settled through the existing `ss:`
action, and the FEN format is unchanged ([ADR 0001](adr/0001-pure-scala-go-engine.md)).

Errors follow one rule per layer: `GoFen` answers outside text with `Either[GoFenError, _]`;
below the seam `GoState`, `Zobrist` and `BulkReplay` fail on caller bugs with `require` /
`IllegalArgumentException`; at and above the `Api.Position` seam failures are `sys.error`.
Every seam entry obeys that rule, batch ones included: an unreadable or off-board action, or an
action offered to a finished game, is rejected before the fold starts and names the ply it sits
at; a move the engine rejects mid-fold arrives as `BulkReplay.IllegalMoveAt` and is rethrown at
the seam with the ply index, the action string and the legal alternatives, the engine failure
kept as its cause.

Adding a board size touches, outside `variant/`: `GoState.supportedSizes`, the
`Zobrist.tableForSize` match and its per-size tables, and `Api.fenRegex` (its row-count
range `{8,18}` also rejects sizes below 9) — plus the usual `Go*Setup` trait with fresh
`id`/`perfId` values (go ids 3, 5, 6, 7 and perfIds 503/504/505 are retired and must not
be reused). The `Pos`/`File` alphabet tops out at 19 files, which is the ceiling for any
new size. (Do not derive the `Zobrist` match from `supportedSizes`: the two objects would
then initialize through each other for zero gain.)

## Replaying a game

`Replay.gameFromUciStrings` replays a whole game in one batch and is the production default.
`Replay.gameFromUciStringsPerPly` plays one action at a time and is retained as the differential
oracle the batch path is measured against — it must never be redefined in terms of the batch path,
or the specs comparing the two prove nothing.

The batch path crosses the seam once. `Api.positionFromVariantStartingFenAndMoves` (and
`positionFromVariantAndMoves`, from a variant's initial position) plans the uci strings into int
moves in a single pass, folds them through `BulkReplay.replay` on a mutable scratch state, and
publishes one final `Position`; `Replay` materializes `Board`/`Situation`/`Game`, the pieceMap, the
score and the FEN once from it. `Api.positionsFromVariantStartingFenAndMoves` serves consumers that
need every intermediate position: it keeps the immutable per-ply fold and drops only the per-ply
string planning and FEN re-parse, forcing no pieceMap.

Two decisions bind both paths. A game resumed from a FEN counts only the actions it is handed: the
FEN's own fields are the sole authority for everything before the resume point, and the synthetic
pass entries `Forsyth.<<@` seeds onto `board.uciMoves` are wrapper bookkeeping that never reaches
the engine and is never a ply ([ADR 0002](adr/0002-go-batch-replay.md)). Pass plies
do not rescore `history.score` — correct by construction rather than a preserved bug, because a pass
shares its predecessor's board array and area scoring reads only that array
([ADR 0002](adr/0002-go-batch-replay.md)).

Four behaviours look like bugs and are kept deliberately; the reason sits at each code site:

| | where |
|---|---|
| An `ss:` records one capture more than the stones it lifts | `Replay.settlementCaptureCount`, both replay paths |
| The interactive `ss:` refreshes neither `scoring` nor `captures` nor `pieces`; the replay `ss:` refreshes all three | `SelectSquares.finalizeAfter` vs `Replay.replaySelectSquares` ([ADR 0002](adr/0002-go-batch-replay.md)) |
| An off-board `ss:` key is ignored, where an off-board drop key is an error | `ScalaPosition.deadStonePoints` |
| An action list that flattens to nothing replays to the initial game, where the per-ply path throws | `Replay.gameFromUciStrings` |

## Where it differed from joansala

Where the two engines disagreed the rules won, and each difference was recorded rather than
smoothed over ([ADR 0001](adr/0001-pure-scala-go-engine.md)). Stored games played under the
retired engine may exhibit any of these on replay — this table is the checklist for a
downstream stored-game audit.

| | joansala (retired) | pure Scala |
|---|---|---|
| Empty region bordered by no colour | all white's | nobody's |
| Raw FEN captures and pass count | hardcoded `0` | the real counts |
| Ko point and handicap scores in FEN | did not round-trip | reproduced, and a ko coordinate is accepted on parse and enforced |
| A move repeating a position | offered, then ended the game as a repetition | refused, so `isRepetition` is permanently false |
| Board row with a run of ten or more | reader split the digits and truncated overruns | multi-digit runs round-trip, overruns are rejected |
| A finished game | still listed legal actions | lists none |
| `makeMovesNoLegalCheck` | replayed an illegal move blindly | still rejects it |
| Superko rule | none enforced | positional, not situational |

The ko field of an emitted FEN carries the live simple-ko coordinate (`-` otherwise); joansala only
ever wrote `-`, so stored FENs never carry a coordinate and parse unchanged.

## Retiring joansala (historical)

Executed 2026-07-25, per [ADR 0001](adr/0001-pure-scala-go-engine.md): the
`com.joansala % go-engine` dependency, the joansala-backed `Api` paths, the transitional
`go9x9Joansala`/`go13x13Joansala`/`go19x19Joansala` variants (ids 5/6/7) and the differential
suite were deleted in a single commit. Any downstream sighting of those ids — or perfIds
503/504/505 — is a bug. The differential baselines and allowlist live in git history.

## Benchmarks

`GoEngineBenchmark` (JMH) times replay, drop application and legal-move generation over the
committed corpus fixtures; `GoLayerBenchmark` times the same game at each layer — engine fold, seam,
wrapper — so a regression can be attributed; `GoSmokeTiming` answers the question in wall-clock
seconds when a JMH run is too slow to be worth it. All live in `bench/`, which documents how to run
them:

```
sbt "bench/Jmh/run -wi 3 -i 3 -f 1 -to 60s strategygames.bench.GoEngineBenchmark"
```

The three size corpora reach no position the superko probe would refuse, so a fourth fixture
(`go-go9x9-superko-long.txt`, keyed `go9x9superko`) carries a 9x9 game that does. It is what
`GoBulkReplayDifferentialSpec` asserts the probe on; corpus coverage alone is not enough for
superko, and any future engine work must keep a fixture that reaches it.

See [`bench/README.md`](../bench/README.md) for the workloads, the longer baseline invocation,
profiling, and the JDK the whole suite must run under.
