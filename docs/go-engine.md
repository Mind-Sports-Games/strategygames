# The pure-Scala Go engine

Go runs on the pure-Scala engine in `src/main/scala/go/engine/`: the variants `go9x9`, `go13x13`
and `go19x19` (ids 1, 2, 4) all build their positions from it. It replaced the external joansala
engine, which was developed against it as an oracle and then removed
([ADR 0001](adr/0001-parallel-scala-go-variants.md),
[ADR 0015](adr/0015-variant-identity-when-joansala-retires.md)).

## Layout

`strategygames.go.Api.Position` is the single seam: Board, Situation, Game, Forsyth and Replay all
consume it and none of them knows the engine behind it
([ADR 0002](adr/0002-engine-seam-at-api-position.md)).

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

Below the seam the board is a flat `(N+2)²` byte array with a sentinel border, chains are union-find
with pseudo-liberty counts, and the position hash is maintained incrementally
([ADR 0003](adr/0003-flat-array-board-union-find-chains.md),
[ADR 0004](adr/0004-incremental-zobrist-positional-superko.md)). The public API is immutable: a move
copies the arrays and mutates only the copy, so a published state is never written again
([ADR 0005](adr/0005-immutable-api-interior-mutability.md)). Scoring is Chinese area scoring, dead
stones are settled through the existing `ss:` action, and the FEN format is unchanged
([ADR 0006](adr/0006-area-scoring-ss-flow-fen-compat.md)).

Errors follow one rule per layer: `GoFen` answers outside text with `Either[GoFenError, _]`;
below the seam `GoState` and `Zobrist` fail on caller bugs with `require` /
`IllegalArgumentException`; at and above the `Api.Position` seam failures are `sys.error`.

Adding a board size touches, outside `variant/`: `GoState.supportedSizes`, the
`Zobrist.tableForSize` match and its per-size tables, and `Api.fenRegex` (its row-count
range `{8,18}` also rejects sizes below 9) — plus the usual `Go*Setup` trait with fresh
`id`/`perfId` values ([ADR 0001](adr/0001-parallel-scala-go-variants.md)). The `Pos`/`File`
alphabet tops out at 19 files, which is the ceiling for any new size. (Do not derive the
`Zobrist` match from `supportedSizes`: the two objects would then initialize through each
other for zero gain.)

## Where it differed from joansala

Where the two engines disagreed the rules won, and each difference was recorded rather than
smoothed over ([ADR 0007](adr/0007-rules-correct-over-joansala-parity.md)). Stored games played
under the retired engine may exhibit any of these on replay — this table is the checklist for a
downstream stored-game audit ([ADR 0015](adr/0015-variant-identity-when-joansala-retires.md)).

| | joansala (retired) | pure Scala | |
|---|---|---|---|
| Empty region bordered by no colour | all white's | nobody's | [0008](adr/0008-empty-region-scores-for-nobody.md) |
| Raw FEN captures and pass count | hardcoded `0` | the real counts | [0009](adr/0009-raw-fen-reports-real-captures-and-passes.md) |
| Ko point and handicap scores in FEN | did not round-trip | reproduced, and a ko coordinate is accepted on parse and enforced | [0010](adr/0010-fen-fields-that-do-not-round-trip.md) |
| A move repeating a position | offered, then ended the game as a repetition | refused, so `isRepetition` is permanently false | [0011](adr/0011-superko-forbidden-up-front-isrepetition-false.md) |
| Board row with a run of ten or more | reader split the digits and truncated overruns | multi-digit runs round-trip, overruns are rejected | [0012](adr/0012-board-rows-multi-digit-runs-reject-overruns.md) |
| A finished game | still listed legal actions | lists none | [0013](adr/0013-no-actions-after-end-no-unchecked-replay.md) |
| `makeMovesNoLegalCheck` | replayed an illegal move blindly | still rejects it | [0013](adr/0013-no-actions-after-end-no-unchecked-replay.md) |
| Superko rule | none enforced | positional, not situational | [0014](adr/0014-positional-superko-over-situational.md) |

The ko field of an emitted FEN carries the live simple-ko coordinate (`-` otherwise); joansala only
ever wrote `-`, so stored FENs never carry a coordinate and parse unchanged
([ADR 0010](adr/0010-fen-fields-that-do-not-round-trip.md), as amended by 0015).

## Retiring joansala (historical)

Executed 2026-07-25, per [ADR 0015](adr/0015-variant-identity-when-joansala-retires.md): the
`com.joansala % go-engine` dependency, the joansala-backed `Api` paths, the transitional
`go9x9Joansala`/`go13x13Joansala`/`go19x19Joansala` variants (ids 5/6/7) and the differential
suite were deleted in a single commit. Any downstream sighting of those ids — or perfIds
503/504/505 — is a bug. The divergence record lives in ADRs 0007–0014; the differential
baselines and allowlist live in git history.

## Benchmarks

`GoEngineBenchmark` (JMH) times replay, drop application and legal-move generation over the
committed corpus fixtures; `GoSmokeTiming` answers the same question in wall-clock seconds when a
JMH run is too slow to be worth it. Both live in `bench/`, which documents how to run them:

```
sbt "bench/Jmh/run -wi 3 -i 3 -f 1 -to 60s strategygames.bench.GoEngineBenchmark"
```

See [`bench/README.md`](../bench/README.md) for the workloads, the longer baseline invocation,
profiling, and the JDK the whole suite must run under.
