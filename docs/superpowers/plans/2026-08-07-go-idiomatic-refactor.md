# Go Idiomatic Rules-in-Variant Refactor — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Move the Go rules out of the `Api.Position` seam and the `go/engine/` package into `go/variant/Variant.scala`, leaving one immutable, functional, idiomatic Scala implementation of Go that still runs far faster than the retired joansala engine.

**Architecture:** `Board` becomes the whole position — `pieces: Map[Pos, Piece]` plus the state the engine used to hide (`komi`, `ko`, `consecutivePasses`, `deadStonesSelected`). Rules become concrete `def`s on the abstract `Variant`, written in terms of one connectivity primitive in `go/Chain.scala`. Superko history lives in the `History.positionHashes` field every other game logic already uses. The new rules are built and differentially verified against the engine while both exist, then the engine is deleted.

**Tech Stack:** Scala 3.7.4 (`-source:3.0-migration`), cats `Validated`, specs2 `mutable.Specification`, scalafmt 3.4.3 (`maxColumn = 110`, `align.preset = most`), sbt 1.10.1 on JDK 21, JMH via the separate `bench` subproject.

**Design doc:** `docs/superpowers/specs/2026-08-07-go-idiomatic-refactor-design.md` — read it before starting any task.

## Global Constraints

- **No duplication.** There must be exactly one implementation of capture, legality, and scoring when the work is done. The retired engine had two (`GoState` and `BulkReplay.ScratchGoState`); that is the defect being removed, not a pattern to copy.
- **Immutable and functional.** No `var`, `Array` mutation, or mutable collection may escape a method body. Method-local scratch mutation is house style where the algorithm is genuinely sequential (precedent: `abalone/variant/Variant.scala:87-298`, `backgammon/Situation.scala:40-66`, `togyzkumalak/format/Forsyth.scala:83-106`) and must carry a comment saying why it is safe.
- **House style is Togyzkumalak, then Backgammon and Abalone.** Fixed vocabulary that must not be renamed: `boardAfter`, `validMoves`/`validDrops`, `canDrop`, `possibleDrops`, `winner`, `specialEnd`, `specialDraw`, `materialImbalance`, `valid`, `addVariantEffect`, `hasMoveEffects`, `finalizeAfter`, `situationAfter`, `situationBefore`, `withX`, `updateX`, `all`/`byId`/`byKey`/`default`/`orDefault`/`exists`.
- **Overridable configuration is `def`, never `val`** — even for constants. `val`/`lazy val` is for non-overridable derived caches and object-level tables.
- **Errors:** `Validated[String, A]` on the legality path, `Option` for absence, `sys.error` for genuinely impossible states. No `Either`, no exceptions in legality.
- **Zero comments from the implementer.** `lw:implementer` writes no comments at all; naming and structure are the documentation. A separate `lw:documenter` pass adds every comment and doc afterwards.
- **`scalafmt` must be clean.** Run `sbt scalafmtAll` before every commit.
- **Behaviour-preserving.** The four quirks in the design doc's "Preserved quirks" section stay, each pinned by a test.
- **Tree green after every task:** `sbt compile test` and, where the task touches `bench`, `sbt bench/test`.

## Wrapper-layer contracts that must not break

- `go.Situation`'s first field stays `board` (`strategygames.Situation.Go` does positional `s.copy(board)`).
- `go.Game`'s seven-argument positional constructor `(situation, actionStrs, clock, plies, turnCount, startedAtPly, startedAtTurn)` is called positionally from `strategygames.Game:1590`.
- `go.History` keeps `score: Score` and `captures: Score` as members.
- `go.Action` stays exhaustively `Drop | Pass | SelectSquares`.
- `go.Board` is only ever `copy`'d with named arguments by the wrapper, so fields beyond the first four may be added and removed freely.

## File structure

| File | Responsibility after the refactor |
| --- | --- |
| `go/variant/Variant.scala` | The Go ruleset as concrete `def`s over `Situation`/`Board`. Grows from 267 to roughly 450 lines. |
| `go/variant/Go9x9.scala`, `Go13x13.scala`, `Go19x19.scala` | Per-variant config only: ids, perf, `initialFen`, `komi`, handicap board tables. Essentially unchanged. |
| `go/Chain.scala` | **New.** The connectivity primitive: a chain of connected stones, its liberties, what a placement captures, whether a placement is legal. Pure functions over `PieceMap`. |
| `go/Board.scala` | Position state. Gains `komi`, `ko`, `consecutivePasses`, `deadStonesSelected`; loses `uciMoves`, `position`, `apiPosition`, `afterDrop`. `BoardSize` gains a precomputed neighbour table. |
| `go/History.scala` | Action record plus `score`/`captures`/`positionHashes`. Loses the `scoring: () => Score` thunk. |
| `go/Situation.scala` | Thin projection onto `board.variant`. Loses all `Api` use. |
| `go/Drop.scala`, `Pass.scala`, `SelectSquares.scala` | `after` becomes a derived `lazy val`. `NextBoard`/`ExplicitBoardAfter`/`LazyBoardAfter` deleted. |
| `go/Hash.scala` | Position hashing, widened to 64 bits, with an incremental mask accessor for superko. |
| `go/format/FEN.scala` | FEN field accessors plus `pieces: PieceMap`. |
| `go/format/Forsyth.scala` | Renders and parses the ten FEN fields from `Board` directly. Also the new home for `removeDeadStones` and FEN validation. |
| `go/Replay.scala` | Collapses to the Togyzkumalak/Backgammon shape. |
| `go/format/pgn/Reader.scala` | Simplified to match `Replay`'s new signatures. |
| **Deleted** | `go/Api.scala`, `go/ScalaPosition.scala`, `go/engine/` (6 files). |

---

## Task 1: Golden oracle harness

Scaffolding that records today's behaviour so the rewrite can be checked ply by ply. It is removed in Task 13.

**Files:**
- Create: `src/test/scala/go/oracle/GoOracle.scala` — the record format and its codec
- Create: `src/test/scala/go/oracle/GoOracleGenerator.scala` — an object with a `main` that regenerates the fixture
- Create: `src/test/resources/go/oracle.txt` — the committed fixture
- Create: `src/test/scala/go/oracle/GoOracleTest.scala` — replays the fixture and asserts every recorded field

**Interfaces:**
- Produces:
  - `final case class GoOraclePly(fen: String, legalDropKeys: List[String], scoreP1: Int, scoreP2: Int, capturesP1: Int, capturesP2: Int, end: Boolean, winner: Option[String])`
  - `final case class GoOracleGame(variantKey: String, initialFen: Option[String], actionStrs: List[String], plies: List[GoOraclePly])`
  - `object GoOracle { def render(games: List[GoOracleGame]): String; def parse(content: String): List[GoOracleGame]; def load(): List[GoOracleGame] }`
  - `object GoOracleGenerator { def games(): List[GoOracleGame]; def main(args: Array[String]): Unit }`

**Game population** — the fixture must cover, and the generator must produce, all of:
1. The 8 real 19x19 games in `src/test/resources/go/upstream-go-bench.suite`, translated from the upstream file alphabet `abcdefghjklmnopqrst` (no `i`) into this repo's `Pos` alphabet, exactly as `UpstreamGameSuiteTest.engineMoveOf` does today.
2. Deterministic pseudo-random legal walks: 20 games per board size (9x9, 13x13, 19x19), seeded `20260807L`, each ply choosing `validDrops` by index `(seed * 1103515245 + 12345) % candidates.size`, capped at 120 plies for 9x9, 200 for 13x13, 400 for 19x19, then terminated with `pass, pass, ss:`.
3. The superko corpus `bench/src/main/resources/corpus/go-go9x9-superko-long.txt`, copied into the generator as a literal action list so `src/test` does not depend on `bench`.
4. Curated scripts, each named in the fixture:
   - `scriptedNineByNine` from `GoScalaVariantTest` (12 drops, then `pass, pass, ss:i1`)
   - the ko script from `GoApiTest` (`2 59 20 39 22 41 40 21` in engine indices, translated to keys)
   - the 34-move triple-ko script from `GoScalaVariantIsometryTest`
   - all nine 9x9 handicap starting FENs from `Go9x9.boardFenFromHandicap`, each followed by 10 plies
   - a four-pass game with no `ss:`
   - a two-pass, one-drop, two-pass, `ss:` game
   - a `ss:` naming an off-board key (`ss:n4` on 9x9) and one naming nothing (`ss:`)

**Recording:** for each game, replay it through `Replay.gameFromUciStrings` and record one `GoOraclePly` per ply from the resulting situations: `Forsyth.>>(game).value`, `situation.drops.getOrElse(Nil).map(_.key).sorted`, `situation.board.history.score.p1`/`.p2`, `.captures.p1`/`.p2`, `situation.end`, `situation.winner.map(_.name)`.

- [ ] **Step 1: Write `GoOracle` and `GoOracleGenerator`, run the generator, inspect the fixture**

Run: `sbt "Test/runMain strategygames.go.oracle.GoOracleGenerator"`
Expected: `src/test/resources/go/oracle.txt` written. Sanity-check by eye that the 19x19 initial FEN appears and that the superko game's ply count matches the corpus.

- [ ] **Step 2: Write `GoOracleTest` and run it**

The test loads the fixture, replays each game's `actionStrs` through `Replay.gameFromUciStrings`, and asserts every recorded field of every ply. It must fail loudly naming game, ply index and field on mismatch.

Run: `sbt "testOnly strategygames.go.oracle.GoOracleTest"`
Expected: PASS. If it fails, the generator and the test disagree about how to replay — fix that before going further, because every later task depends on this being an exact mirror.

- [ ] **Step 3: Prove the oracle detects a regression**

Temporarily change one character of an expected FEN in the fixture, re-run, confirm FAIL, revert.

Run: `sbt "testOnly strategygames.go.oracle.GoOracleTest"`
Expected: FAIL, then PASS after revert.

- [ ] **Step 4: Full suite green**

Run: `sbt scalafmtAll && sbt compile test`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add src/test/scala/go/oracle src/test/resources/go/oracle.txt
git commit -m "test(go): golden oracle harness for the rules rewrite"
```

---

## Task 2: Re-home the engine tests' rules facts

The engine tests hold the actual Go rules and are written against `GoState`. Re-express every rule they assert as readable tests against `variant.Variant` and `Situation`, organised by rule. These must pass against the **current** implementation — this is the safety net, built before anything moves.

**Files:**
- Create: `src/test/scala/go/GoLegalityTest.scala` — occupancy, suicide, simple ko, snapback
- Create: `src/test/scala/go/GoSuperkoTest.scala` — positional superko
- Create: `src/test/scala/go/GoCaptureTest.scala` — capture and prisoner accounting
- Create: `src/test/scala/go/GoScoringTest.scala` — Chinese area scoring
- Create: `src/test/scala/go/GoEndingTest.scala` — passes, dead-stone selection, game end
- Create: `src/test/scala/go/GoHandicapTest.scala` — handicap setup and scoring
- Create: `src/test/scala/go/GoUpstreamGamesTest.scala` — the 8 real 19x19 games, stone conservation
- Modify: none. Nothing is deleted in this task.

**Interfaces:**
- Consumes: `variant.Go9x9/Go13x13/Go19x19`, `Situation`, `Forsyth.<<@`, `Forsyth.>>`, `Replay.gameFromUciStrings`.
- Produces: nothing consumed by later tasks; these are the acceptance tests every later task must keep green.

**Every fact below must appear as a named test.** They come from `GoStateTest`, `GoGameTest`, `AreaScoreTest`, `GoFenCodecTest`, `UpstreamJoansalaPortedTest`, `UpstreamGameSuiteTest` and `BulkReplayTest`. Express each through the public go API (`Situation.drops`, `Situation.drop`, `Situation.end`, `Situation.winner`, `board.history.score`, `Forsyth.>>`) — never through `Api` or `engine`, so they survive the deletion.

**Legality (`GoLegalityTest`)**
- An occupied point is not offered as a drop, for either colour.
- Single-stone suicide is illegal: a point whose only neighbours are enemy stones, where the placement captures nothing, is absent from `drops` and `Situation.drop` returns `Invalid`.
- Multi-stone chain suicide is illegal.
- A move that captures is never suicide.
- Simple ko: after a single-stone capture, the recapture point is absent from `drops`; after an exchange elsewhere it becomes available and a ko point is recorded on the other square.
- A pass clears the ko point: `GoSituationTest`'s Issue#489 numbers must reproduce — ply 8 has 76 drops, ply 9 after a pass has 77 including `e4`.
- Snapback: when the capturing stone joins a bigger chain, no ko point is recorded and the immediate recapture is legal. `GoSituationTest`'s Issue#490 — ply 16 has 69 drops including `e8`.

**Superko (`GoSuperkoTest`)**
- The send-two-return-one cycle is refused.
- The 34-move triple-ko script from `GoScalaVariantIsometryTest`: valid at `.init`, invalid at full length.
- `g9.validDrops(game_p12.situation).map(_.pos.key)` does not contain `"h9"`, and after a pass `end === false` and `isRepetition === false`.
- **The constructed case from `BulkReplayTest`, which is the sole guard on the superko probe and which no corpus exercises:** build the ko shape `b2 c2 a3 d3 b4 c4 c3` with white to play, play `koCapture = b3`, then `pass`, `pass`; assert the board's `ko` is empty yet `c3` is still absent from `drops`. Add a comment in the documenter pass recording that this test is the only superko guard.
- Superko history does not survive a FEN round trip: a folded position forbids a recapture that the same position reloaded from its own FEN permits. This asymmetry is deliberate; pin it.

**Capture (`GoCaptureTest`)**
- Corner capture works on all three board sizes.
- One stone killing two distinct chains removes both and credits both to the mover.
- Prisoners are credited to the capturing player, are cumulative, survive passes and settlement, and play no part in scoring.
- Capture frees the intersections: after the ported upstream capture case, the drop count is 72.

**Scoring (`GoScoringTest`)**
- Empty board scores 0 / 0 at all sizes.
- One black stone on an empty 9x9 scores the whole board to black.
- Black `a1` next to white `b1` scores 1 / 1 — a region touching both colours belongs to nobody.
- A full black wall on file e with one white stone at g5 scores 45 / 1 — a region touching only white but enclosed against black belongs to nobody.
- A single-point eye belongs to the enclosing player.
- Removing a dead stone hands the surrounded region to the opponent.
- Captured stones become territory once off the board.
- Seki gets no special handling: shared dame score for nobody.
- Komi goes to P2 only, in tenths, and is 5.5 on 9x9 and 7.5 on the other two.
- `GoApiTest`'s three "ongoing but already decided" positions reproduce their exact drop counts and scores: `(2, 361.0, 7.5)`, `(28, 323.0, 45.5)`, `(38, 322.0, 45.5)`.
- The settled 19x19 position from `UpstreamJoansalaPortedTest` scores 322 / 38 area, 322.0 / 44.5 points.

**Ending (`GoEndingTest`)**
- Two passes do not end the game; drops are still legal (82 on 9x9).
- Two passes open dead-stone selection; a placement re-closes it.
- Only a settlement ends the game. An empty `ss:` is a legal settlement and ends it.
- Pass parity: two passes enable `canSelectSquares`, a third does not — `GoSituationTest`'s ply 8/9/10/13 assertions.
- Four consecutive passes end the game with no explicit `ss:`; four passes split by a stone do not.
- `GoReplayTest`'s 13 exact FENs for the scripted 9x9 replay, including that the two passes leave board and scores untouched while the last field increments.
- A settlement keeps the player to move and advances the ply count by one.
- An off-board `ss:` key is ignored (`ss:n4` on 9x9 behaves as `ss:`); an off-board drop key is an error. Pin both.

**Handicap (`GoHandicapTest`)**
- All nine 9x9 handicap boards round-trip through FEN.
- A handicap game starts with white to move.
- `Go9x9.fenFromSetupConfig(4, 55).value === "9/9/2S3S2/9/9/9/2S3S2/9/9[SSSSSSSSSSssssssssss] w - 40 55 0 0 55 0 1"` and `handicap === Some(4)`.
- The setup FEN's cosmetic score fields are replaced by real area scores on the first render: the same position renders `810` where the setup FEN said `40`.
- Handicap stones participate in area scoring normally.
- After a drop, `initialFen` is still the handicap FEN.

**Upstream games (`GoUpstreamGamesTest`)**
- All 8 games in `src/test/resources/go/upstream-go-bench.suite` replay from an empty board with only legal moves.
- Stone conservation per game: `blackOnBoard + capturesByWhite === blackPlaced` and `whiteOnBoard + capturesByBlack === whitePlaced`.
- Each game contains at least one capture. There are exactly 8 games.

- [ ] **Step 1: Write the seven test files**

Each is a specs2 `mutable.Specification`. Follow the style of `src/test/scala/go/GoSituationTest.scala`.

- [ ] **Step 2: Run them and confirm they pass against the current implementation**

Run: `sbt "testOnly strategygames.go.GoLegalityTest strategygames.go.GoSuperkoTest strategygames.go.GoCaptureTest strategygames.go.GoScoringTest strategygames.go.GoEndingTest strategygames.go.GoHandicapTest strategygames.go.GoUpstreamGamesTest"`
Expected: PASS. Any failure means the fact was mis-transcribed from the engine test — go back to the engine test and re-read it. Do not change the implementation in this task.

- [ ] **Step 3: Full suite green**

Run: `sbt scalafmtAll && sbt compile test`
Expected: PASS.

- [ ] **Step 4: Commit**

```bash
git add src/test/scala/go
git commit -m "test(go): re-home the engine suites' rules facts onto the public go api"
```

---

## Task 3: Position state onto Board, thunk off History

Additive. The engine stays the source of truth; the new fields are populated alongside it and asserted to agree.

**Files:**
- Modify: `src/main/scala/go/Board.scala` — add fields
- Modify: `src/main/scala/go/History.scala` — `scoring: () => Score` becomes `score: Score`
- Modify: `src/main/scala/History.scala:233` — `scoring = () => score` becomes `score = score`
- Modify: `src/main/scala/go/format/Forsyth.scala` — populate the new fields on parse
- Modify: `src/main/scala/go/variant/Variant.scala`, `src/main/scala/go/Replay.scala` — maintain the new fields wherever they maintain `uciMoves`
- Create: `src/test/scala/go/GoBoardStateTest.scala`

**Interfaces:**
- Produces:
  - `go.Board` gains `komi: Double = variant.komi`, `ko: Option[Pos] = None`, `consecutivePasses: Int = 0`, `deadStonesSelected: Boolean = false`
  - `go.History.score: Score` replaces `scoring: () => Score`; `History.unscored` is deleted
  - `go.Board.BoardSize` gains `val neighbours: Array[List[Pos]]`, indexed by `Pos.index`, holding the cardinal neighbours clipped to this size

**Behaviour:** none changes. `uciMoves` and `position` stay for now. Everywhere the code appends to `uciMoves` it must also maintain `consecutivePasses`/`deadStonesSelected`, and everywhere it reads `uciMoves` to derive pass state (`Situation.canSelectSquares`, `Situation.isSubsequentPassWarning`, `Forsyth.exportBoardFen` field 8) it must read the new fields instead.

**`History.score` going strict:** `Board.afterDrop` sets `score = <the engine's fenScore>` eagerly. The two ADR 0002 rulings survive: a pass does not rescore, and a game with no drops keeps `Score(0, 0)`.

- [ ] **Step 1: Write `GoBoardStateTest`**

For every game in the golden oracle, replay it and assert at every ply that the new `Board` fields agree with what the engine says: `board.ko` matches the FEN's field 2, `board.consecutivePasses` and `board.deadStonesSelected` together reproduce the FEN's field 8, `board.komi` matches the FEN's field 7 divided by ten, and `board.history.score` equals the engine's `fenScore`.

- [ ] **Step 2: Run it and watch it fail**

Run: `sbt "testOnly strategygames.go.GoBoardStateTest"`
Expected: FAIL — the fields do not exist yet.

- [ ] **Step 3: Add the fields and maintain them**

Add `BoardSize.neighbours` in the same commit — it is inert geometry with no consumer until Task 4, and it belongs to the size, not the variant, alongside the `validPos` list already there.

- [ ] **Step 4: Run the new test, the oracle, and the Task 2 suites**

Run: `sbt "testOnly strategygames.go.*"`
Expected: PASS. `GoOracleTest` passing is the proof that nothing observable changed.

- [ ] **Step 5: Full suite and bench green**

Run: `sbt scalafmtAll && sbt compile test && sbt bench/test`
Expected: PASS.

- [ ] **Step 6: Commit**

```bash
git add -u && git add src/test/scala/go/GoBoardStateTest.scala
git commit -m "refactor(go): carry ko, komi and pass state on Board, drop the score thunk"
```

---

## Task 4: `go/Chain.scala`

The connectivity primitive. Written and tested standalone; nothing in production uses it yet.

**Files:**
- Create: `src/main/scala/go/Chain.scala`
- Create: `src/test/scala/go/GoChainTest.scala`

**Interfaces:**
- Consumes: `Board.BoardSize.neighbours` from Task 3, `PieceMap`, `Pos`, `Piece`, `Player`.
- Produces:
  - `Chain.at(board: Board, pos: Pos): Set[Pos]` — the maximal set of same-colour stones connected to `pos`; empty if `pos` holds no stone
  - `Chain.liberties(board: Board, group: Set[Pos]): Set[Pos]` — the empty points cardinally adjacent to the group
  - `Chain.hasLiberty(board: Board, group: Set[Pos]): Boolean` — must short-circuit on the first liberty found
  - `Chain.capturedBy(board: Board, player: Player, pos: Pos): Set[Pos]` — the enemy stones removed by `player` placing at `pos`; assumes `pos` is empty
  - `Chain.isSuicide(board: Board, player: Player, pos: Pos, captured: Set[Pos]): Boolean` — whether the placement leaves its own chain without liberties, given what it captures

**Rules these must implement,** from the design doc and `docs/go-engine.md`:
- Adjacency is cardinal only, clipped to `board.variant.boardSize`.
- A placement captures every adjacent enemy chain left with zero liberties after the stone is placed. A chain adjacent on more than one side is captured once.
- A placement is suicide when, after removing what it captures, the chain containing the new stone has no liberties. A capturing placement is therefore never suicide.

Method-local mutation is allowed for the flood fills and must carry a justifying comment in the documenter pass.

- [ ] **Step 1: Write `GoChainTest`**

Build boards with `Forsyth.<<@` from literal FENs. Cover: a lone stone's chain is itself; an L-shaped chain of four; a chain spanning a board edge; liberties of a corner stone are two; `hasLiberty` false for a fully surrounded single stone; `capturedBy` for a corner capture, a multi-chain capture, a chain adjacent on two sides captured once, and a placement that captures nothing; `isSuicide` true for single-stone and multi-stone suicide and false when the placement captures. Include one case per board size so the neighbour clipping is exercised — in particular that a stone on file `i` of a 9x9 board has no east neighbour even though `Pos` has one.

- [ ] **Step 2: Run and watch it fail**

Run: `sbt "testOnly strategygames.go.GoChainTest"`
Expected: FAIL — `Chain` does not exist.

- [ ] **Step 3: Implement `Chain`**

- [ ] **Step 4: Run and confirm pass**

Run: `sbt "testOnly strategygames.go.GoChainTest"`
Expected: PASS.

- [ ] **Step 5: Full suite green**

Run: `sbt scalafmtAll && sbt compile test`
Expected: PASS.

- [ ] **Step 6: Commit**

```bash
git add src/main/scala/go/Chain.scala src/test/scala/go/GoChainTest.scala
git commit -m "feat(go): add Chain, the connectivity primitive the rules are written in"
```

---

## Task 5: `Variant.areaScore`

**Files:**
- Modify: `src/main/scala/go/variant/Variant.scala`
- Create: `src/test/scala/go/GoAreaScoreDifferentialTest.scala`

**Interfaces:**
- Produces: `Variant.areaScore(board: Board): Score` — Chinese area scoring in FEN tenths, `Score(blackArea * 10, whiteArea * 10 + komiTenths)` where `komiTenths = Math.round(board.komi * 10).toInt`.

**Rule:** each player scores their stones on the board, plus every empty region bordered by exactly one colour. A region bordered by both colours, or by neither, scores for nobody. No seki adjustment, no dame filling, no dead-stone inference.

- [ ] **Step 1: Write `GoAreaScoreDifferentialTest`**

For every ply of every game in the golden oracle, assert `variant.areaScore(board)` equals the oracle's recorded score. Then assert the hand-built cases from `GoScoringTest` directly against `areaScore`.

- [ ] **Step 2: Run and watch it fail**

Run: `sbt "testOnly strategygames.go.GoAreaScoreDifferentialTest"`
Expected: FAIL.

- [ ] **Step 3: Implement `areaScore`**

Nothing else calls it yet. Use `boardSize.neighbours` for the flood fill.

- [ ] **Step 4: Run and confirm pass**

Run: `sbt "testOnly strategygames.go.GoAreaScoreDifferentialTest strategygames.go.GoScoringTest"`
Expected: PASS.

- [ ] **Step 5: Full suite green, then commit**

```bash
sbt scalafmtAll && sbt compile test
git add -u && git add src/test/scala/go/GoAreaScoreDifferentialTest.scala
git commit -m "feat(go): score area from the board in Variant"
```

---

## Task 6: Superko history in `History.positionHashes`

**Files:**
- Modify: `src/main/scala/go/Hash.scala` — widen to 64 bits, expose an incremental mask
- Modify: `src/main/scala/go/History.scala` — no signature change; `positionHashes` finally gets populated
- Modify: `src/main/scala/go/Board.scala` — maintain `positionHashes`
- Create: `src/test/scala/go/GoPositionHashTest.scala`

**Interfaces:**
- Produces:
  - `go.Hash.size` becomes `8`
  - `go.Hash.mask(piece: Piece, pos: Pos): Long` — the Zobrist mask for one stone, so a placement's hash is the previous hash XOR the placed stone XOR each captured stone
  - `go.Hash.positionHash(board: Board): Long` — a full recompute, for FEN loads and settlements
  - `History.positionHashes` holds 8-byte hashes, **newest first**, so `positionHashes.take(8)` is the current position
  - `History.hasOccurred(hash: Long): Boolean` — linear scan over `positionHashes`

**Rules:** stones only are hashed — not side to move, not ko, not captures, not pass count. A pass does not append a hash. A settlement restarts the history from the settled position. A FEN load starts the history at the loaded position, which is why superko history cannot survive a FEN round trip.

**Why 64 bits:** 24 bits over a 300-position game collides about 0.3% of the time, and here a collision makes a legal move illegal. `strategygames.Hash.size` is unchanged; only `go.Hash.size` moves, and it affects nothing but `strategygames.History.toString`'s grouping.

- [ ] **Step 1: Write `GoPositionHashTest`**

Assert: the empty board hashes to a stable value; the hash is stable under transposition (two move orders reaching the same arrangement hash equal); a placement's incremental hash equals a full recompute of the resulting board, over every ply of every oracle game; a pass leaves the hash and the history length unchanged; a settlement resets the history to length one.

- [ ] **Step 2: Run and watch it fail**

Run: `sbt "testOnly strategygames.go.GoPositionHashTest"`
Expected: FAIL.

- [ ] **Step 3: Implement**

- [ ] **Step 4: Run and confirm pass**

Run: `sbt "testOnly strategygames.go.*"`
Expected: PASS.

- [ ] **Step 5: Full suite green, then commit**

```bash
sbt scalafmtAll && sbt compile test
git add -u && git add src/test/scala/go/GoPositionHashTest.scala
git commit -m "feat(go): keep positional superko history in History.positionHashes"
```

---

## Task 7: `Variant.boardAfter` for a placement

**Files:**
- Modify: `src/main/scala/go/variant/Variant.scala`
- Create: `src/test/scala/go/GoBoardAfterDifferentialTest.scala`

**Interfaces:**
- Produces: `Variant.boardAfter(situation: Situation, pos: Pos): Board`

**What it does,** in one pure expression chain:
- `pieces` = `situation.board.pieces` minus `Chain.capturedBy(...)` plus `pos -> Piece(situation.player, Stone)`
- `history.captures` = incremented for the mover by the number of stones captured
- `history.score` = `areaScore(the new board)`
- `history.positionHashes` = the new hash prepended
- `history.halfMoveClock` = `+ situation.player.fold(0, 1)`, matching `Board.afterDrop` today
- `ko` = `Some(the captured point)` when exactly one stone was captured **and** the placed stone's chain has exactly one stone **and** that chain has exactly one liberty; `None` otherwise
- `consecutivePasses` = `0`
- `komi`, `deadStonesSelected`, `pocketData`, `variant` carried through

- [ ] **Step 1: Write `GoBoardAfterDifferentialTest`**

For every drop ply of every oracle game, assert that `variant.boardAfter(situationBefore, pos)` produces the same `pieces`, `ko`, `consecutivePasses`, `history.score`, `history.captures` and `history.halfMoveClock` as the engine path `board.afterDrop(player, pos)` does at that ply. Compare `pieces` as sets, not by iteration order.

- [ ] **Step 2: Run and watch it fail**

Run: `sbt "testOnly strategygames.go.GoBoardAfterDifferentialTest"`
Expected: FAIL.

- [ ] **Step 3: Implement `boardAfter`**

Nothing calls it yet. `Board.afterDrop` still exists and is still what production uses.

- [ ] **Step 4: Run and confirm pass**

Run: `sbt "testOnly strategygames.go.*"`
Expected: PASS. Any disagreement is a real rules bug in the new code — read `docs/go-engine.md`'s capture and ko sections and `go/engine/GoState.scala`'s `afterPlacement` before changing the test.

- [ ] **Step 5: Full suite green, then commit**

```bash
sbt scalafmtAll && sbt compile test
git add -u && git add src/test/scala/go/GoBoardAfterDifferentialTest.scala
git commit -m "feat(go): apply a placement in Variant.boardAfter"
```

---

## Task 8: Switch generation and legality onto the new rules

The flip. After this task the engine is dead weight, still compiled but no longer consulted for anything except the differential tests.

**Files:**
- Modify: `src/main/scala/go/variant/Variant.scala` — `validDrops`, `validPass`, `createSelectSquares`, `boardAfterPass`, `boardAfterSelectSquares`, `winner`, `specialEnd`, `specialDraw`, `valid`, `pieces`
- Modify: `src/main/scala/go/Situation.scala` — drop `gameEnd`/`gameResult`/`result`, `isRepetition`, `unary_!`, `canSelectSquares`, `isSubsequentPassWarning`
- Modify: `src/main/scala/go/Drop.scala`, `Pass.scala`, `SelectSquares.scala` — `after` becomes a derived `lazy val`; delete `NextBoard`, `ExplicitBoardAfter`, `LazyBoardAfter`
- Modify: `src/main/scala/go/Board.scala` — delete `afterDrop`
- Create: `src/test/scala/go/GoDropLazinessTest.scala` — replaces `GoValidDropsLazinessTest`
- Delete: `src/test/scala/go/GoValidDropsLazinessTest.scala`

**Interfaces:**
- Produces:
  - `Variant.boardAfterPass(situation: Situation): Board` — turn state only: `consecutivePasses + 1`, `ko = None`, everything else carried through, score **not** recomputed
  - `Variant.boardAfterSelectSquares(situation: Situation, squares: List[Pos]): Board` — lift the named on-board stones of either colour, set `deadStonesSelected = true`, restart `positionHashes`, keep the player to move
  - `Variant.canDrop(situation: Situation): Boolean`
  - `Variant.validDrops(situation: Situation): List[Drop]` — every empty on-board point that is not the ko point, whose placement is not suicide, and whose resulting hash has not occurred when the placement captures
  - `Drop.after: Board` as a `lazy val` computed from `situationBefore` and `pos`
  - `Situation.isRepetition: Boolean = false` — superko is refused at generation, so a repetition is unreachable

**Superko probe:** only test history membership when the placement captures. A non-capturing placement cannot recreate an earlier arrangement.

**The four-pass auto-settle** stays: `validPass` turns a pass into a pass plus an empty settlement when the previous three actions were passes.

**Preserved quirk:** the interactive `ss:` and the replay `ss:` refresh different fields of the resulting board. Determine empirically from the oracle which fields each refreshes and reproduce it exactly. Do not tidy it.

- [ ] **Step 1: Write `GoDropLazinessTest`**

`Drop.after` is now a `lazy val`, so laziness is checked without implementing any seam type: subclass `Go19x19` in the test overriding `boardAfter` to increment a counter, generate `validDrops` for a mid-game 19x19 position, assert the counter is `0`, force three drops' `after`, assert the counter is `3`.

- [ ] **Step 2: Run the full go suite and watch the new laziness test fail**

Run: `sbt "testOnly strategygames.go.GoDropLazinessTest"`
Expected: FAIL.

- [ ] **Step 3: Switch generation and legality onto the new rules**

- [ ] **Step 4: Run the whole go suite**

Run: `sbt "testOnly strategygames.go.*"`
Expected: PASS, including `GoOracleTest`, `GoSituationTest`, `GoReplayTest`, `GoScalaVariantIsometryTest` and every Task 2 suite. This is the task where regressions will surface; the oracle names the game, ply and field.

- [ ] **Step 5: Full suite and bench green**

Run: `sbt scalafmtAll && sbt compile test && sbt bench/test`
Expected: PASS.

- [ ] **Step 6: Commit**

```bash
git add -u && git add src/test/scala/go/GoDropLazinessTest.scala
git rm src/test/scala/go/GoValidDropsLazinessTest.scala
git commit -m "feat(go): generate and validate actions from the rules in Variant"
```

---

## Task 9: `Forsyth` and `FEN` off the engine

**Files:**
- Modify: `src/main/scala/go/format/FEN.scala` — add `pieces`
- Modify: `src/main/scala/go/format/Forsyth.scala` — render and parse from `Board`
- Modify: `src/main/scala/go/format/Sgf.scala` — use `FEN.pieces` instead of `Api.pieceMapFromFen`
- Modify: `src/main/scala/go/variant/Variant.scala` — `pieces` and `valid` off the engine
- Create: `src/test/scala/go/GoForsythTest.scala`

**Interfaces:**
- Produces:
  - `FEN.pieces: PieceMap` — parses field 0, mirroring `togyzkumalak.format.FEN.pieces`
  - `Forsyth.boardPart(board: Board): String` — the board field plus the constant pocket literal `[SSSSSSSSSSssssssssss]`
  - `Forsyth.validate(fen: FEN): Boolean` — replaces `Api.validateFEN`, keeping today's regex **and** a structural parse
  - `Forsyth.removeDeadStones(variant: Variant, fen: FEN, squares: List[Pos]): FEN` — replaces `Api.removeDeadStones`
  - `Variant.valid(board, strict)` — a cheap structural invariant, not a FEN regex round trip

**FEN grammar, unchanged:** `board[pocket] turn ko p1Score p2Score p1Captures p2Captures komi passCount fullMove`. Rows top rank down, `S` black, `s` white, decimal multi-digit runs of empties. Scores and komi in tenths. `passCount` is `{0,1,2,3}` with `3` meaning settled. The nine-field legacy form parses and is never emitted. Field 2 is `-` or the live ko coordinate.

**Preserved quirk:** `exportBoardFen` field 9 concatenates rather than adds when the last turn was a `SelectSquares` and the turn is `w`. Reproduce it, and pin it with a test that actually reaches the branch — no committed test does today.

**`Variant.valid` note:** today it round-trips the board through `Forsyth.exportBoard` and a regex, which is why the 10-field regex is the only FEN shape gate. Keep a `Forsyth.validate` that behaves identically for external callers, but make `Variant.valid` the cheap invariant the house uses: every piece is on a point of this board size, and no point holds more than one stone.

- [ ] **Step 1: Write `GoForsythTest`**

Round-trip every oracle FEN: `Forsyth.<<(fen).map(Forsyth.>>) === Some(fen)`. Assert `FEN.pieces` against `Api.pieceMapFromFen` for every oracle FEN while both exist. Cover multi-digit empty runs, the live ko coordinate, the legacy nine-field form re-emitting as ten, all nine 9x9 handicap boards, and the field-9 concatenation branch. Keep `GoFenCodecTest`'s rejection cases as `Forsyth.validate` returning `false`.

- [ ] **Step 2: Run and watch it fail**

Run: `sbt "testOnly strategygames.go.GoForsythTest"`
Expected: FAIL.

- [ ] **Step 3: Rewrite `Forsyth` and `FEN`**

- [ ] **Step 4: Run and confirm pass**

Run: `sbt "testOnly strategygames.go.*" && sbt "testOnly strategygames.format.GoScalaWrapperRoundTripTest" && sbt "testOnly format.sgf.DumperTest"`
Expected: PASS.

- [ ] **Step 5: Full suite and bench green, then commit**

```bash
sbt scalafmtAll && sbt compile test && sbt bench/test
git add -u && git add src/test/scala/go/GoForsythTest.scala
git commit -m "feat(go): render and parse go fens from the board"
```

---

## Task 10: `Replay` and `pgn/Reader`

**Files:**
- Modify: `src/main/scala/go/Replay.scala`
- Modify: `src/main/scala/go/format/pgn/Reader.scala`
- Modify: `src/main/scala/go/Board.scala` — delete `uciMoves`, `position`, `withPosition`, `apiPosition`

**Interfaces:**
- Produces:
  - `Replay.replayDrop(before: Game, role: Role, dest: Pos, endTurn: Boolean): Drop`
  - `Replay.replayPass(before: Game, endTurn: Boolean): Pass`
  - `Replay.replaySelectSquares(before: Game, squares: List[Pos], endTurn: Boolean): SelectSquares`
  - `Replay.gameFromUciStrings(ucis: List[String], activePlayer: Player, initialFen: Option[FEN], variant: Variant): Game`
- Deleted: `gameFromUciStringsSlow`, `gameFromUciStringsPerPly`, `gameFromBatchedActions`, `capturesAfter`, `isSelectSquares`, `uciOf`

Model the file on `togyzkumalak/Replay.scala` and `backgammon/Replay.scala` — same member order, same `var state`/`var errors` idiom inside `gameWithActionWhileValid`, same recursive `Validated` shape with `andThen`.

**Preserved quirks:** `settlementCaptureCount`'s `+1` stays and keeps its explanatory comment. An action list that flattens to nothing still replays to the initial game.

- [ ] **Step 1: Run the existing replay tests to record the baseline**

Run: `sbt "testOnly strategygames.go.GoReplayTest strategygames.go.oracle.GoOracleTest"`
Expected: PASS. These are the acceptance tests for this task; they must still pass afterwards without modification.

- [ ] **Step 2: Rewrite `Replay` and simplify `Reader`; delete the Board fields**

- [ ] **Step 3: Run the go suite and the wrapper round trip**

Run: `sbt "testOnly strategygames.go.*" && sbt "testOnly strategygames.format.*"`
Expected: PASS.

- [ ] **Step 4: Full suite and bench green, then commit**

```bash
sbt scalafmtAll && sbt compile test && sbt bench/test
git add -u
git commit -m "refactor(go): collapse Replay to the house shape, drop the board's replay log"
```

---

## Task 11: Delete the engine

**Files:**
- Delete: `src/main/scala/go/Api.scala`, `src/main/scala/go/ScalaPosition.scala`, `src/main/scala/go/engine/` (6 files)
- Delete: `src/test/scala/go/GoApiTest.scala`, `GoBatchEntryTest.scala`, `GoLongGameTest.scala`, `src/test/scala/go/engine/` (9 files)
- Delete: `bench/src/test/scala/strategygames/bench/GoBulkReplayDifferentialSpec.scala`, `GoSeamBatchReplaySpec.scala`, `GoReplayDefaultSpec.scala`
- Delete: the differential tests from Tasks 5, 6 and 7 that compare against the engine — `GoAreaScoreDifferentialTest`, `GoBoardAfterDifferentialTest`, and the engine-comparison half of `GoPositionHashTest`. The oracle test replaces them.
- Modify: `src/test/scala/go/GoScalaVariantTest.scala` — drop the four `isInstanceOf[ScalaPosition]` assertions and the `Api.*` calls; keep every behavioural fact
- Create: `src/test/scala/go/GoLongGameTest.scala` (rewritten) — 610 plies on 19x19 via `validDrops`, asserting drops remain available

**Before deleting anything, confirm every fact is re-homed.** Work down the list in the design doc's Testing section. `GoApiTest`'s move-index table and `EngineMoveEncodingTest` are the only facts that legitimately vanish — they pin an int encoding that has no meaning without the engine.

- [ ] **Step 1: Grep for any remaining reference**

Run: `grep -rn "Api\.\|ScalaPosition\|go\.engine\|apiPosition\|GameResult" src/main/scala/go src/test/scala/go bench/src`
Expected: no hits in `go`. Hits in `fairysf`/`samurai` are unrelated packages with their own `Api`.

- [ ] **Step 2: Delete, and rewrite `GoLongGameTest` and `GoScalaVariantTest`**

- [ ] **Step 3: Full suite and bench green**

Run: `sbt scalafmtAll && sbt clean compile test && sbt bench/test`
Expected: PASS.

- [ ] **Step 4: Confirm no `var` escapes a method anywhere in go**

Run: `grep -rn "var \|mutable\." src/main/scala/go`
Expected: every hit is inside a method body. Any field-level `var` is a plan violation.

- [ ] **Step 5: Commit**

```bash
git add -A src/main/scala/go src/test/scala/go bench/src
git commit -m "feat(go): remove the Api seam and the engine package"
```

---

## Task 12: Benchmarks and measurement

**Files:**
- Modify: `bench/src/main/scala/strategygames/bench/GoEngineBenchmark.scala` — `legalDrops` off `Api.Position`
- Rewrite: `bench/src/main/scala/strategygames/bench/GoLayerBenchmark.scala` — keep `prodReplay`, `areaScoreMidGame`, `legalDropsMidGame`, `fenParseMidGame`, `fenRenderMidGame` re-pointed at `Variant`/`Forsyth`; delete the seam and engine benchmarks
- Modify: `bench/src/main/scala/strategygames/bench/GoMovegenConsumerBenchmark.scala` — delete the eager replica
- Modify: `bench/src/main/scala/strategygames/bench/GoSmokeTiming.scala` — build positions through `Replay`/`Forsyth`
- Modify: `docs/go-speed-results.md`

- [ ] **Step 1: Re-point the benchmarks; `sbt bench/test` green**

- [ ] **Step 2: Measure**

Run:
```bash
sbt "bench/Jmh/run -wi 3 -w 2s -i 5 -r 2s -f 1 -to 120s -rf json -rff go-jmh.json strategygames.bench.Go.*"
```
Record replay µs/game and `validDrops` µs/call for all three sizes.

- [ ] **Step 3: Check the bar**

Full-game replay must be at least 40x the joansala baseline (7,395 / 24,313 / 96,385 µs for 9x9 / 13x13 / 19x19). If any size misses, profile before optimising and report the finding rather than guessing — the two known suspects are the strict `history.score` flood fill per drop and per-candidate legality in `validDrops`.

- [ ] **Step 4: Regenerate `docs/go-speed-results.md` against the joansala baseline, then commit**

```bash
git add -u
git commit -m "bench(go): re-point the go benchmarks at the rules in Variant"
```

---

## Task 13: Documentation, ADRs, and removing the scaffolding

**Files:**
- Rewrite: `docs/go-engine.md`
- Create: `docs/adr/0003-go-rules-in-variant.md`
- Modify: `docs/adr/0001-pure-scala-go-engine.md`, `docs/adr/0002-go-batch-replay.md` — mark superseded
- Modify: `docs/go-engine-speed-dive.md` — freeze as historical with a header
- Modify: `bench/README.md` — correct the claim that the three size corpora are committed
- Delete: `src/test/scala/go/oracle/`, `src/test/resources/go/oracle.txt`
- Create: the final write-up

**ADR 0003 records:** rules as concrete `def`s on `Variant`; one implementation of capture and legality in `Chain`; position state on `Board`; superko history in `History.positionHashes`; strict `History.score`; and it carries forward verbatim ADR 0002's two rulings — a resume counts only the actions given, and pass plies do not rescore.

**`docs/go-engine.md` keeps:** the rules contract, the joansala divergence table, the board-size checklist (re-pointed away from `GoState.supportedSizes`, `Zobrist.tableForSize` and `Api.fenRegex`), and the four preserved quirks. It loses the seam, the engine file table, and the layered error discipline.

**The write-up covers:** how the result follows house conventions and where it deviates and why; what is idiomatic about it; the measured performance against the joansala baseline; the four preserved quirks each with the fix it would need; and the exact lila patch the removal of `go.Api` requires.

- [ ] **Step 1: Remove the oracle scaffolding, confirm the suite still proves the behaviour**

Run: `sbt scalafmtAll && sbt clean compile test && sbt bench/test`
Expected: PASS. If removing the oracle leaves a behaviour unproven, that is a missing test — add it rather than keeping the oracle.

- [ ] **Step 2: Write the docs, ADRs and write-up**

- [ ] **Step 3: Commit**

```bash
git add -A docs bench/README.md src/test
git commit -m "docs(go): document the rules-in-Variant design, retire the engine adrs"
```
