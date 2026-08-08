# Go: idiomatic rules-in-Variant refactor

Design, 2026-08-07. Branch `lakin/go-speed-dive`.

## Problem

`strategygames.go` is the only pure-Scala game logic in the repo shaped like a foreign-engine
binding. Its rules live behind an `Api.Position` seam in `go/Api.scala`, `go/ScalaPosition.scala`
and a six-file `go/engine/` package; `go/variant/Variant.scala` is a thin adapter that asks the
seam questions; `go/Board.scala` carries an engine object (`position: Option[Api.Position]`) and a
replay log (`uciMoves: List[String]`) inside an otherwise immutable value.

That shape is a fossil of the retired joansala Java binding. Every other game logic —
Togyzkumalak, Backgammon, Abalone, Dameo — puts its rules in `variant/Variant.scala` as pure
functions over `Situation`/`Board`/`PieceMap`, and keeps `Board` as `(pieces, history, variant)`
plus game-specific state.

Goal: make `go` look and behave like it was written by hand for strategygames, following the
house patterns, while staying far faster than the joansala engine it replaced.

## Two branches

The work lands as two branches rather than one compromise.

**`lakin/go-idiomatic-refactor` — as close as possible to the existing game logics, correct for
Go.** Every choice is decided by "what would Togyzkumalak or Backgammon do here", and performance
never breaks a tie. Strict `History.score`. No memo in `validDrops`. `PieceMap` end to end. This
branch is the statement of what idiomatic Go in strategygames looks like, and it is the one to
read when asking whether the code fits the house.

**`lakin/go-idiomatic-perf` — branched from the first once it is green and measured, modified as
little as possible to reach good performance.** Every deviation from the first branch must be
justified by a benchmark number, must be the smallest change that moves it, and must be
documented at the point of deviation. A deviation without a measurement behind it does not land.

The pivot between them is the measurement step. The first branch measures and reports; it does not
optimise. What the second branch changes is decided by what that measurement says, not by what
this document guesses.

**The first branch is the deliverable.** Merging depends on it and on nothing else. The second is
a bonus, attempted only once the first is complete, reviewed and documented. No decision on the
first branch may be shaded by what the second might want — if idiom and speed conflict there,
idiom wins and the conflict becomes a note for the second branch.

## Non-goals

- Changing the Go rules. Superko flavour, area scoring, FEN dialect and variant ids stay as
  ADR 0001 decided them.
- Changing externally-visible behaviour. The rewrite is behaviour-preserving, including four
  known quirks (see "Preserved quirks").
- Touching any other game logic beyond the two lines of `src/main/scala/History.scala` that read
  `go.History`.

## Success criteria

1. `go/Api.scala`, `go/ScalaPosition.scala` and `go/engine/` are gone. No replacement package.
2. `go/variant/Variant.scala` reads like `togyzkumalak/variant/Variant.scala`: an abstract class
   whose concrete `def`s are the base ruleset and whose concrete variants are diffs.
3. `go/Board.scala` has no `position`, no `apiPosition`, no `uciMoves`.
4. `go/History.scala` has no `() => Score` thunk.
5. `go/Replay.scala` has the same public shape as `togyzkumalak/Replay.scala` and
   `backgammon/Replay.scala` — no batch/per-ply duality.
6. No `var` or mutable collection escapes a method body anywhere in `go`.
7. `sbt test` and `sbt bench/test` green.
8. Full-game replay is measured against the joansala baseline on every board size and the numbers
   are recorded. This is a reporting criterion, not a gate: no idiom is traded to move it.

---

## Architecture

### State model

`Board` becomes the whole position. Everything the rules need is a field or is derived from one.

```scala
case class Board(
    pieces: PieceMap,                       // Map[Pos, Piece] — house type, unchanged
    history: History,
    variant: Variant,
    pocketData: Option[PocketData] = None,
    komi: Double,                           // per game, from the FEN; no default — see below
    ko: Option[Pos] = None,                 // simple ko point, FEN field 2
    consecutivePasses: Int = 0,             // 0..2, FEN field 8
    deadStonesSelected: Boolean = false     // FEN field 8 == 3
)
```

Precedent: `backgammon.Board` carries `unusedDice` and `cubeData` the same way — FEN-serialised
turn state that the wrapper layer never sees, threaded whole through go objects.

`komi` deliberately has no default. The obvious `komi: Double = variant.komi` does not compile the
way it reads: `strategygames.go.variant` is a package, so `variant` in that position resolves to
the package rather than to the constructor parameter. A constant default would be worse than none,
because it would silently give `Go9x9` 7.5 where its komi is 5.5. So the parameter is required and
`Board.apply(pieces, variant)` passes `variant.komi` explicitly.

`History` loses the thunk and gains nothing:

```scala
case class History(
    lastTurn: List[Uci] = List.empty,
    currentTurn: List[Uci] = List.empty,
    positionHashes: PositionHash = Array.empty,   // superko history, newest first
    halfMoveClock: Int = 0,
    score: Score = Score(0, 0),                   // area score in FEN tenths
    captures: Score = Score(0, 0)                 // prisoners
)
```

Two notes on `positionHashes`. It is the house field for exactly this purpose in every other
logic; go has always declared it and never used it. Go needs *positional superko*: the set of
every position this line has occupied. Storing them newest-first in the standard `Array[Byte]`
layout means the head is the current hash, so the next hash is an O(1) incremental XOR from it,
and the membership test is a linear scan — affordable because the rules only probe superko on
*capturing* placements, of which a position has a handful. Go's `Hash.size` moves from 3 to 8 so
a truncation collision cannot make a legal move illegal; 24 bits over a 300-position game is a
~0.3% collision rate, which is tolerable for chess threefold and is not tolerable for legality.

`Situation` keeps its `case class Situation(board, player)` shape and delegates everything, like
Togyzkumalak's. `canSelectSquares` and `isSubsequentPassWarning` read `board.consecutivePasses`
and `board.deadStonesSelected` instead of scanning a string log.

### Where the rules live

`go/variant/Variant.scala` — the ruleset, as concrete `def`s over `Situation`/`Board`, following
the Togyzkumalak/Backgammon vocabulary exactly:

| member | shape |
| --- | --- |
| `boardAfter(situation, pos): Board` | apply a placement — the canonical "apply the rules" method, same name and signature idea as Togyzkumalak's and Abalone's |
| `boardAfterPass(situation): Board` | apply a pass |
| `boardAfterSelectSquares(situation, squares): Board` | apply a settlement |
| `private def canDrop(situation): Boolean` | Backgammon's `canX`/`validX` pairing |
| `validDrops(situation): List[Drop]` | generate |
| `validPass(situation): Pass` | generate |
| `createSelectSquares(situation, squares): SelectSquares` | generate |
| `drop` / `pass` / `selectSquares` | `Validated[String, _]`, select from the generated list |
| `areaScore(board): Score` | Chinese area scoring, in FEN tenths, komi to P2 |
| `winner` / `specialEnd` / `specialDraw` | the universal trio |
| `valid(board, strict)` | cheap structural invariant, not a FEN regex |
| `pieces` | `initialFen.pieces` |
| `komi`, `boardFenFromHandicap`, `fenFromSetupConfig`, `setupInfo` | unchanged per-variant config |

`go/Chain.scala` — one new file, holding the Go domain noun the rules are written in terms of:
a chain (group) of connected same-colour stones, its liberties, and what a placement does to its
neighbours. Pure functions over `PieceMap` plus the board's neighbour table.

```scala
object Chain {
  def at(board: Board, pos: Pos): Set[Pos]
  def hasLiberty(board: Board, group: Set[Pos]): Boolean
  def capturedBy(board: Board, player: Player, pos: Pos): Set[Pos]
  def isLegalPlacement(board: Board, player: Player, pos: Pos): Boolean
}
```

Rationale for the file: it names the concept the rules are about, keeps
`variant/Variant.scala` near the size of Abalone's (447) and Backgammon's (693) rather than past
900, and is testable on its own. Precedent for a game-specific concept file:
`backgammon/CubeData.scala`, `abalone/BoardType.scala`, `draughts/Piotr.scala`.

Critically there is **one** implementation of capture and legality. The retired engine had two
(`GoState` and `BulkReplay.ScratchGoState`) kept in sync by a differential test; that duplication
is the single worst thing about the current code and it does not come back.

`Board.BoardSize` gains a precomputed neighbour table, alongside the `validPos` list it already
carries:

```scala
sealed abstract class BoardSize(val width: Int, val height: Int) {
  val validPos: List[Pos]                 // existing
  val neighbours: Array[List[Pos]]        // new: cardinal neighbours by Pos.index, clipped to this size
}
```

This is the geometry fact that makes the whole thing fast and it belongs to the size, not the
variant. `Pos` is a 19x19 grid shared by all three sizes, so a 9x9 board must clip — the same
`allByWidth` idea Togyzkumalak uses, precomputed once per singleton.

### Contract constraints the wrapper layer imposes

These are load-bearing and easy to break by accident:

- `go.Situation`'s first field must stay `board` — `strategygames.Situation.Go` does a positional
  single-argument `s.copy(board)`.
- `go.Game`'s seven-argument positional constructor `(situation, actionStrs, clock, plies,
  turnCount, startedAtPly, startedAtTurn)` is called positionally from `strategygames.Game`.
- `go.Board` is only ever `copy`'d with named arguments by the wrapper, so adding and removing
  fields beyond the first four is safe.
- `go.Action` must stay exhaustively `Drop | Pass | SelectSquares`; the wrapper matches on all
  three.
- `go.History` must keep `score: Score` and `captures: Score` as members —
  `strategygames.History.Go` reads both. `scoring` is written in exactly one place outside `go`
  (`src/main/scala/History.scala:233`, as a constant thunk) and that line becomes `score = score`.

### Data flow

Applying one action, e.g. a drop at `pos`:

```
Situation.drop(role, pos)
  -> Variant.drop           -> selects from validDrops
  -> Drop(piece, pos, situationBefore, autoEndTurn)
  -> Drop.after (lazy val)  -> Variant.boardAfter(situationBefore, pos)
       Chain.capturedBy     -> local flood fill over pieces
       pieces - captured + placed
       history.copy(score = areaScore, captures = ..., positionHashes = newHash ++ old)
       ko = ...             -> per the simple-ko rule
  -> Drop.finalizeAfter     -> updates lastTurn/currentTurn
  -> Drop.situationAfter
```

`Drop` drops the `NextBoard`/`ExplicitBoardAfter`/`LazyBoardAfter` indirection entirely. Instead
of a field holding a thunk, `after` becomes a derived `lazy val`:

```scala
case class Drop(piece: Piece, pos: Pos, situationBefore: Situation, autoEndTurn: Boolean, metrics: MoveMetrics = MoveMetrics())
    extends Action(situationBefore) {
  lazy val after: Board = situationBefore.board.variant.boardAfter(situationBefore, pos)
  ...
}
```

This is both simpler and closer to the other logics — their `Move`/`Drop` carry `after: Board` as
a field, ours carries it as a derived value, because for Go the after-board is fully determined by
`(situationBefore, pos)`. It keeps the laziness that makes `validDrops` cheap (361 candidate drops
cost 361 legality checks, not 361 board constructions) without a bespoke thunk wrapper.

`Pass` and `SelectSquares` get the same treatment for symmetry.

### Replay

`go/Replay.scala` collapses to the Togyzkumalak/Backgammon shape. Deleted:
`gameFromUciStringsSlow`, `gameFromUciStringsPerPly`, `gameFromBatchedActions`, `capturesAfter`,
`isSelectSquares`, `uciOf`. `replayPass` and `replaySelectSquares` lose their `apiPosition` and
`uciMoves` parameters and take only `before` plus the action, which also simplifies
`go/format/pgn/Reader.scala`.

The batch machinery existed only because applying one ply through the seam was expensive —
it forced a movegen and a `pieceMap` rebuild per ply. With `boardAfter` a cheap pure function
that does neither, the ordinary recursive replay is fast enough and the duality has no reason to
exist.

### FEN

`go/format/Forsyth.scala` stops doing string surgery on engine output and renders the ten fields
from `Board` + `Situation`, following `togyzkumalak/format/Forsyth.scala`:

- `FEN.pieces: PieceMap` parses the board field (Togyzkumalak has exactly this member).
- `Forsyth.<<@` builds `Situation(Board(pieces = fen.pieces, history = ..., variant, komi, ko,
  consecutivePasses, deadStonesSelected), player)`.
- `Forsyth.boardPart(board)` writes rows with the house `StringBuilder` + `var empty` idiom.
- `Forsyth.>>` composes the ten fields.

The FEN grammar is unchanged: `board[pocket] turn ko p1Score p2Score p1Captures p2Captures komi
passCount fullMove`, scores and komi in tenths, `passCount` a `{0,1,2,3}` enum with 3 meaning
settled, nine-field legacy form still parsed and never emitted.

### The lila-facing API

`Api.scala` disappears. Its members that have no in-repo caller but are part of the published
surface get idiomatic homes:

| was | becomes |
| --- | --- |
| `Api.pieceMapFromFen(key, fen)` | `FEN.pieces` (mirrors `togyzkumalak.format.FEN.pieces`) |
| `Api.initialFen(variantKey)` | `Variant(key).initialFen` |
| `Api.validateFEN(variant, fen)` | `Variant.valid(board, strict)` plus `Forsyth.validate(fen)` |
| `Api.writeBoardFenFromPieceMap(pieceMap, variant)` | `Forsyth.exportBoard` / `Forsyth.boardPart` |
| `Api.removeDeadStones(deadStones, fen, variant)` | `Forsyth.removeDeadStones(variant, fen, squares)` |
| `Api.moveToUci` / `uciToMove` / `moveToPos` / `passMove` | deleted — engine index encoding, no meaning without the engine |
| `Api.Position` and its 25 members | deleted |
| `go.GameResult` | deleted — `Situation.end`/`winner`/`status` already carry this |

The exact lila patch this implies is written up at the end of the work.

---

## Error handling

Unchanged from house style, and simpler than today's three-layer discipline:

- Legality and action selection return `Validated[String, A]` (cats), as in every other logic.
- Absence is `Option`.
- Genuinely impossible states — a corrupt FEN reaching the parser, a replay naming a square that
  does not exist — are `sys.error`, as in `togyzkumalak/Replay.scala` and `Forsyth`.
- The `GoFenError` taxonomy and `Either[GoFenError, _]` go away with the engine. The error
  messages the tests pin (`"Unreadable action ..."`, `"Drop ... names no square of ..."`,
  `"Illegal action ..."`) belonged to the batch planner and go with it; `Forsyth` and `Replay`
  keep house-style messages.

---

## Preserved quirks

Behaviour-preserving means these come across unchanged, each pinned by a test and each flagged in
the final write-up with the fix it would need:

1. `Replay.settlementCaptureCount`'s `+1` — a settlement records one capture more than it lifts.
   Correct in `Board.afterDrop` (where the placed stone cancels it), over-counting for a
   settlement, and already written into every stored game's `History.captures`.
2. The interactive `ss:` and the replay `ss:` refresh different fields of the resulting board.
3. An off-board `ss:` key is ignored; an off-board drop key is an error.
4. `Forsyth.exportBoardFen` field 9 does `String + Int`, i.e. concatenation — a fullMove of 6
   becomes `"61"`. Only reachable when the last turn was a `SelectSquares` and the engine turn is
   `"w"`, which no committed test hits.

Two rulings from ADR 0002 are carried forward verbatim because they are contracts, not bugs:
a resume counts only the actions given, and pass plies do not rescore `history.score`.

---

## Performance

On this branch performance is measured and reported, never traded for. The reference is the
joansala baseline in `docs/go-speed-results.md`: 7,395 / 24,313 / 96,385 µs per full-game replay
at 9x9 / 13x13 / 19x19.

Expected cost changes, all measured rather than assumed:

- Replay gets slower than today's 277 µs at 19x19 and stays far under the bar. Two known costs:
  `history.score` becomes strict, so every drop pays one area-scoring flood fill — the speed-dive
  measured forced area scoring at 18% of production replay time — and captures are found by local
  flood fill over a `Map` rather than by a maintained union-find index.
- `validDrops` gets slower than today's 13.9 µs at 19x19 because legality is computed per
  candidate from the `PieceMap` rather than read off a maintained index. This is the one place a
  memo would pay; it is deliberately not built until a benchmark says it is needed.

The JMH suite is re-pointed at the new code: the seam and engine benchmarks go, `prodReplay`,
`prodValidDropsMidGame` and `applyDrop` stay and become the headline numbers, and
`docs/go-speed-results.md` is regenerated against the same joansala baseline.

Both of these are candidate deviations for the second branch, not for this one. If measurement
shows the strict `history.score` dominates, the note goes in the write-up and the field stays
strict here, because that is what every other logic does.

---

## Testing

Three layers, in the order they get built.

**1. A golden corpus, as scaffolding.** Before anything is deleted, generate from today's engine a
per-ply record — FEN, legal-drop keys, score, captures, end, winner — over the committed
`upstream-go-bench.suite` games, the generated 9x9/13x13/19x19 corpora, and the superko fixture.
Commit it, use it as a differential oracle throughout the rewrite, and remove it at the end. It is
a harness for the move, not a permanent fixture.

**2. Real behavioural tests, which carry the contract.** The engine tests
(`GoStateTest`, `GoGameTest`, `AreaScoreTest`, `GoFenCodecTest`, `UpstreamJoansalaPortedTest`,
`UpstreamGameSuiteTest`) hold the actual Go rules and are currently expressed against `GoState`.
Their facts get re-homed as readable tests against `variant.Variant`, organised by rule rather
than by engine class:

- placement legality: occupancy, single- and multi-stone suicide, simple ko set/cleared/snapback
- positional superko: send-two-return-one, the 34-move triple ko, the constructed
  `BulkReplayTest` case where `ko` is `None` yet the recapture is still refused — this last one is
  the *sole* guard on the superko probe and the corpora do not exercise it
- capture: corner, multi-chain, prisoner accounting
- area scoring: regions touching both colours or neither score for nobody, seki has no special
  handling, dead stones become territory once lifted
- passes and settlement: two passes open selection, a placement re-closes it, only a settlement
  ends the game, an empty settlement is legal, the four-pass auto-settle
- handicap: all nine 9x9 boards, white to move, area scoring counts handicap stones
- FEN: round-trip at all sizes, multi-digit empty runs, live ko coordinate, legacy nine-field form
- the asymmetry that positional-superko history does not survive a FEN round trip

`UpstreamGameSuiteTest`'s stone-conservation check over the 8 real 19x19 games is the highest
value rules net in the repo; the suite file stays and the test is re-expressed against `Variant`.

**3. The tests that already pass and must keep passing untouched:** `GoSituationTest` (475 lines,
the richest surviving net — ko drop counts, superko refusal, pass parity, handicap resume),
`GoReplayTest`, `GoScalaVariantIsometryTest`, `GoFenTest`, `GoBinaryTest`,
`GoScalaWrapperRoundTripTest`, `format/sgf/DumperTest`, `GameToUciStringsTest`.

Tests that die with their subject: `GoApiTest`, `GoBatchEntryTest`, `GoValidDropsLazinessTest`,
`GoLongGameTest`, `EngineMoveEncodingTest`, `BulkReplayTest`, `GoBulkReplayDifferentialSpec`,
`GoSeamBatchReplaySpec`, `GoReplayDefaultSpec`. Every behavioural fact each of them asserts is
listed above and re-homed first; only then are they removed.

`GoValidDropsLazinessTest` needs a replacement that does not implement `Api.Position`: with
`Drop.after` a `lazy val`, laziness is checked by asserting that generating drops does not
construct boards, which a counting `Variant` subclass or a simple allocation assertion can do.

---

## Documentation

- `docs/go-engine.md` rewritten: the seam, the engine file table and the layered error discipline
  go; the rules contract, the joansala divergence table, the board-size checklist and the
  preserved quirks stay, re-pointed at `Variant`/`Chain`.
- `docs/adr/0001-pure-scala-go-engine.md`: superseded in its structural half by a new ADR; its
  rules decisions remain in force and are restated there.
- `docs/adr/0002-go-batch-replay.md`: superseded; its two rulings and the equivalence-suite
  mandate are carried into the new ADR.
- New ADR: rules in `Variant`, one implementation of capture/legality, state on `Board`,
  superko history in `History.positionHashes`, strict `History.score`.
- `docs/go-engine-speed-dive.md` frozen as historical with a header saying so.
- `docs/go-speed-results.md` regenerated.
- `bench/README.md` corrected (it claims the three size corpora are committed; only the superko
  fixture is).
- A final write-up: how the result follows house conventions, what is idiomatic about it, the
  measured performance, the four preserved quirks with proposed fixes, and the lila patch.

---

## Sequencing

Each step ends green and committed.

1. Golden corpus generator and fixture; differential harness. Nothing else changes.
2. Re-home the engine tests' rules facts as `variant.Variant`-level tests that pass against the
   *current* implementation. This is the safety net, built before anything moves.
3. `Board.BoardSize.neighbours`; `go/Chain.scala` with its own unit tests, unused by production.
4. `Variant.boardAfter` / `boardAfterPass` / `boardAfterSelectSquares` + `areaScore`, in terms of
   `Chain`, verified against the engine by the differential harness while both exist.
5. Switch `Board`, `History`, `Situation`, `Drop`, `Pass`, `SelectSquares` to the new state model.
6. Rewrite `Forsyth` and `FEN`; rehome the lila-facing helpers.
7. Simplify `Replay` and `pgn/Reader`.
8. Delete `Api.scala`, `ScalaPosition.scala`, `engine/`, and the tests whose subject is gone.
9. Re-point the benchmarks; measure; regenerate the speed results.
10. Documentation, ADRs, write-up. Remove the golden corpus scaffolding.
