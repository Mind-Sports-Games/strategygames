# The pure-Scala Go engine

Go runs on two engines. The variants `go9x9`, `go13x13` and `go19x19` delegate to the external
joansala engine; `go9x9Scala`, `go13x13Scala` and `go19x19Scala` run the pure-Scala engine in
`src/main/scala/go/engine/`. Both families are live so each can serve as the other's oracle in
benchmarks and differential tests ([ADR 0001](adr/0001-parallel-scala-go-variants.md)).

## Layout

`strategygames.go.Api.Position` is the single seam: Board, Situation, Game, Forsyth and Replay all
consume it and none of them knows which engine answers
([ADR 0002](adr/0002-engine-seam-at-api-position.md)). `Api.position` and
`Api.positionFromVariantNameAndFEN` dispatch on `Variant.usesScalaEngine`.

| | |
|---|---|
| `go/ScalaPosition.scala` | the seam's pure-Scala implementation — UCI strings in, `PieceMap`/FEN/legal actions out |
| `go/engine/GoState.scala` | a position: board, chains, side to move, position-hash history, and the rules |
| `go/engine/GoGame.scala` | a state plus komi, ply count, and whether dead stones have been agreed |
| `go/engine/GoFen.scala` | the FEN dialect, shared byte for byte with the joansala variants |
| `go/engine/Zobrist.scala` | the per-size stone hash tables |

Below the seam the board is a flat `(N+2)²` byte array with a sentinel border, chains are union-find
with pseudo-liberty counts, and the position hash is maintained incrementally
([ADR 0003](adr/0003-flat-array-board-union-find-chains.md),
[ADR 0004](adr/0004-incremental-zobrist-positional-superko.md)). The public API is immutable: a move
copies the arrays and mutates only the copy, so a published state is never written again
([ADR 0005](adr/0005-immutable-api-interior-mutability.md)). Scoring is Chinese area scoring, dead
stones are settled through the existing `ss:` action, and the FEN format is unchanged
([ADR 0006](adr/0006-area-scoring-ss-flow-fen-compat.md)).

## Where it differs from joansala

Where the two engines disagree the rules win, and each difference is recorded rather than smoothed
over ([ADR 0007](adr/0007-rules-correct-over-joansala-parity.md)). The differential suite treats
this table as its allowlist: any other mismatch is a bug.

| | joansala | pure Scala | |
|---|---|---|---|
| Empty region bordered by no colour | all white's | nobody's | [0008](adr/0008-empty-region-scores-for-nobody.md) |
| Raw FEN captures and pass count | hardcoded `0` | the real counts | [0009](adr/0009-raw-fen-reports-real-captures-and-passes.md) |
| Ko point and handicap scores in FEN | do not round-trip | reproduced, and a ko coordinate is accepted on parse | [0010](adr/0010-fen-fields-that-do-not-round-trip.md) |
| A move repeating a position | offered, then ends the game as a repetition | refused, so `isRepetition` is permanently false | [0011](adr/0011-superko-forbidden-up-front-isrepetition-false.md) |
| Board row with a run of ten or more | reader splits the digits and truncates overruns | multi-digit runs round-trip, overruns are rejected | [0012](adr/0012-board-rows-multi-digit-runs-reject-overruns.md) |
| A finished game | still lists legal actions | lists none | [0013](adr/0013-no-actions-after-end-no-unchecked-replay.md) |
| `makeMovesNoLegalCheck` | replays an illegal move blindly | still rejects it | [0013](adr/0013-no-actions-after-end-no-unchecked-replay.md) |
| Superko rule | none enforced | positional, not situational | [0014](adr/0014-positional-superko-over-situational.md) |

## Benchmarks

`GoEngineBenchmark` (JMH) times replay, drop application and legal-move generation on both engines
over the same corpus fixtures; `GoSmokeTiming` answers the same question in wall-clock seconds when
a JMH run is too slow to be worth it. Both live in `bench/`, which documents how to run them:

```
sbt "bench/Jmh/run -wi 3 -i 3 -f 1 -to 60s strategygames.bench.GoEngineBenchmark"
```

See [`bench/README.md`](../bench/README.md) for the workloads, the longer baseline invocation,
profiling, and the JDK the whole suite must run under.
