# Go's rules moved onto `Variant`

Go was the only pure-Scala game logic in this repo shaped like a foreign-engine binding. Its rules
sat below an `Api.Position` seam backed by a six-file `go/engine/` package, and `go.Board` carried an
engine object and a replay log instead of the position. Thirteen tasks moved the rules into
`go/variant/Variant.scala` and a new `go/Chain.scala`, put position state on `Board` as ordinary
fields, derived the area score, kept superko history in the house `positionHashes` field, collapsed
`Replay` from two paths to one, and deleted the seam and the engine — 4,616 lines. Every step was
verified against the engine while it still ran. The decision is
[ADR 0003](adr/0003-go-rules-in-variant.md); the rules contract is
[docs/go-engine.md](go-engine.md).

## What it found

Four separate instances of the same defect: **one behaviour, several entry points, one of them
updated.** They were found by four different tasks, at four different layers, and none of them was
being looked for.

| | the behaviour | the entry points | what diverged |
|---|---|---|---|
| 1 | selecting dead stones | played (`Variant.createSelectSquares`) vs loaded (`Replay.replaySelectSquares`) | the played path never refreshed `board.pieces`, so the lifted stones were still on the board. Measured on a decided 19x19 position: `Score(450,65)` where the truth is `Score(810,55)` |
| 2 | four passes ending the game | played vs loaded | the auto-settlement lived only in `Variant.validPass`, which the loader did not go through. Same four passes: played gave `end=true, winner=P2, 0 drops`; replayed gave `end=false, winner=None, 81 drops` |
| 3 | an action offered after a settlement | batch replay vs per-ply replay | batch refused it, per-ply accepted it |
| 4 | the settlement capture adjustment | `Replay`'s three loaders vs `pgn.Reader` | three loaders added it, the fourth did not — the same game got a different `history.captures` and a different exported FEN depending on how it was loaded |

A fifth was *introduced* during the refactor and caught in review: `Forsyth.validate` and
`Forsyth.<<@`, two gates built in the same task, disagreed about which turn-field strings a FEN may
carry. That one is the tell. The shape is not carelessness; it is what happens when a behaviour has
more than one home, and it recurs even in work whose explicit purpose is removing it.

All four are closed by there being nowhere left to diverge, rather than by four fixes. There is now
one placement path (`Variant.boardAfter`), one settlement path
(`Variant.boardAfterSelectSquares`), one legality decision (`Situation.drop` → `Variant.drop`), and
one helper that pairs a recorded settlement with its capture accounting so a loader cannot take one
without the other (`Replay.addSettlement`). A fifth instance would have to be written on purpose.
That is the argument for the whole exercise, and it is worth more than any line count.

## How it follows the house conventions

The point of the branch was to make go look like the other eight logics. Concretely:

**The `Variant` vocabulary.** `boardAfter` is the placement path, named after
`togyzkumalak.Variant.boardAfter` and `backgammon.Variant.boardAfter`. `canDrop`/`validDrops` pair
the way `validMoves` and its predicate do elsewhere — `canDrop` short-circuits on the first legal
point instead of building a list nobody asked for. `winner`, `specialEnd` and `specialDraw` carry
the game's ending, `valid(board, strict)` is the cheap structural invariant, and the whole file sits
in the same position with the same member order as its neighbours.

**Named transitions on `Board`, after the backgammon precedent.** `passed`, `stonePlaced`, `settled`
and `withKo` are the only writers of `ko`, `consecutivePasses` and `deadStonesSelected`, exactly as
`backgammon.Board` owns `setDice`, `useDie` and `undoUseDie`. Nothing outside `go/Board.scala`
assigns position state, so the fields cannot drift apart.

**`Validated[String, A]` on the legality path.** `Variant.drop`, `pass` and `selectSquares` all
return it, and every loader now reports a refusal rather than throwing. The engine's `GoFenError`
taxonomy died with it by design; `Forsyth.validate` answers `Boolean` and `Forsyth.<<@` answers
`Option`.

**Derived state as `lazy val`s.** `Board.areaScore` sits with `actors`, `posMap`,
`piecesOnBoardCount` and `playerPiecesOnBoardCount`. `def` for overridable configuration — `komi`,
`initialFen`, `perfId` — and `val` for what is fixed per variant.

**Method-local mutation only where the algorithm is genuinely sequential.**
`Replay.gameWithActionWhileValid` folds with a `var state`, which is what the same function does in
togyzkumalak and backgammon; the mutation never escapes the method. `Variant.emptyRegionsOf` came out fully
immutable — a fold over a `(regions, alreadyInARegion)` accumulator — so the mutation that had been
sanctioned for it was not needed.

**The score split.** `Variant.areaScore(board)` is the rule; `Board.areaScore` is the memo. That is
the same split `materialImbalance` already had.

## What is idiomatic about it, and the one deviation

The result reads as go rules. `isPlayable` is four clauses in the order a person would check them.
`koPointAfter` is the three conditions of simple ko and nothing else. `areaScore` is stones plus
sole-bordered empty regions plus komi. A reader who knows go can now audit the rules without
learning a private engine dialect first, which was not true before.

`Chain` is the piece that was not obvious in advance. Pulling connectivity out as its own file meant
capture and legality could be answered by one call — `capturesUnlessSuicide` — instead of two that a
caller might combine wrongly, and it meant exactly one flood fill exists in the go package. That
mattered: without it, area scoring would have grown a second, hand-written fill, and a second fill
that gets the frontier bookkeeping wrong produces *identical results* while walking large regions
repeatedly. No assertion on the output can tell the two apart. The sharing is the mitigation; there
is no test standing behind it, and the note on `Chain.Stones.grownFrom` says so.

**The one deviation worth seeing: `Drop.after` is a derived `lazy val`, not a constructor field.**
Every other logic takes it as a constructor parameter — `chess.Move`, `togyzkumalak.Move` and
`backgammon.Move` all declare `after: Board`. For go it is fully determined by
`(situationBefore, pos)` — there is nothing a caller could supply that `Variant.boardAfter` cannot
work out — so passing it in only creates the possibility of passing in something else. It is also
simpler than what it replaced: the pre-refactor code carried a `NextBoard`/`LazyBoardAfter` wrapper
whose whole job was deferring the same computation. Keeping it lazy is what makes `validDrops`
affordable, since a client asking for the legal points never forces a single after-board. The
tradeoff is real and worth naming: an illegal placement now fails from inside a `lazy val`, so
`Chain.requireVacant`'s `IllegalArgumentException` surfaces at the first read of `after` rather than
at construction. Every path that builds a `Drop` checks legality first, which is why that is
acceptable rather than merely tolerated.

## What it costs

[docs/go-speed-results.md](go-speed-results.md) holds the tables and is the record. Every ratio
crossing to joansala or to the deleted seam is cross-machine and indicative, because all three
references are deleted code and no same-run denominator can be produced again.

**Against joansala**, full-game replay through `go.Replay` is 9.3x / 16.4x / 29.7x faster
(9x9 / 13x13 / 19x19), and a single placement — `Variant.boardAfter` — is 42x faster at 19x19.

**Against the seam this branch deleted**, replay at 19x19 is 11.7x slower than its batch path and
1.5x slower than its per-ply path. The batch path folded a whole game on one mutable scratch state
and published once; an immutable design that materialises a `Board` per ply does not reach that, and
was never going to. Legal-drop generation is the one workload untouched by any of this: 17–21x the
seam's *lazy* list, and 1.2–2.7x faster than its *eager* one.

One design decision changed mid-branch on measurement, and it is the largest single number here. The
area score was first a strict field on `History`, copying togyzkumalak's *shape* without its
*reasoning* — togyzkumalak's score is accumulated captures, go's is a full-board flood fill, so the
copy ran a flood fill on every placement whether or not anyone wanted the number. Deriving it on
`Board` instead was worth **3.4x / 5.2x / 9.5x** on replay and **2.9x / 5.0x / 8.4x** on allocation,
and took `applyDrop` at 19x19 from 59.9 µs to 1.44 µs. Per ply, replay is now 6.6 / 7.4 / 8.1 µs —
nearly flat across board sizes, which is what it should look like once the one O(board area) term
per ply is gone.

What is left, with `areaScore` out of the profile, is the cost the design knowingly accepted:

- **`PieceMap = Map[Pos, Piece]` and `Set[Pos]`.** `Pos` is a case class, so every liberty test and
  every region step is a boxed structural hash and an equality against an immutable `HashMap` or
  `HashSet`. The deleted engine indexed a byte array by `Pos.index`. That representation difference
  — not the flood fill's asymptotics — is the constant factor under chain walking, FEN parsing and
  movegen alike, and it is the single named root cause in the profile.
- **`History.hasOccurred`'s superko scan**, an O(plies) linear scan per ply, newly visible at ~4%. It
  was always there; `areaScore` dwarfed it.
- **On the wrapper path only**, `strategygames.History.Go` takes the score as a strict parameter, so
  reading *any* history field on a ply forces that ply's flood fill — 2.8x / 3.7x / 4.7x on a
  consumer that reads a score every ply. Closing it needs either a by-name parameter (costing
  `History.Go` its `case class`, making go the only non-case-class of nine) or a board-carrying
  `History.Go` (which lila's `History.apply` factory cannot construct). Both are larger than this
  branch sanctioned.

Nothing else measured here was acted on. A follow-up branch takes the numbers as its input.

## The four preserved quirks

Each is behaviour that stored games were written under, so each is preserved rather than fixed. The
reason sits at the code site; here is what fixing each one would take.

**1. A settlement records one capture more than it lifts.**
`Replay.settlementCaptureCount` is `stonesBefore - stonesAfter + 1`. *Fix:* delete the `+ 1`. What
it needs is a decision about the `history.captures` of every settled go game already in the
database.

**2. A settlement records captures when the game is loaded, and none when it is played.**
`Replay.withSettlementCaptures` adds the count; `Variant.boardAfterSelectSquares`, which the live
path uses, does not. *Fix:* move the adjustment into `boardAfterSelectSquares` so both paths agree.
Same blocker as quirk 1, and the same decision resolves both — this is the divergence, quirk 1 is
its arithmetic.

**3. An off-board `ss:` key is ignored, where an off-board drop key is an error.**
The settlement branch of `Replay.gameWithActionWhileValid` uses `flatMap(Pos.fromKey)`, which drops
a key the board size has no point for; the drop branch above it errors on the same input. *Fix:* a
`traverse` instead of a `flatMap`. What it needs first is a sweep of the stored records, because any
game already carrying a stray key becomes unloadable the moment it is refused.

**4. A game p2 settled renders its full-move number with a `1` concatenated, not added.**
`Forsyth.fullMovePart` produces `"231"` for full move 23. Almost certainly a `+` that should have
been arithmetic. *Fix:* make it arithmetic. What it needs is a decision about the FENs already
written; nothing reads the field back for anything that matters, because a settled game is over.

## Behaviour changes

Three, all forced, all with evidence.

**1. A live settlement now refreshes `board.pieces`.** It did not; the lifted stones stayed on the
board and only the engine's `pieceMap` was correct. The replay path was already right. Once the
engine is deleted `board.pieces` is the only answer, so the paths had to merge and the only correct
merge is the fresh one. Three findings during the refactor traced back to this one defect
independently, and it was quantified at the ply that decides a game: area scoring a live-settled
board gave `Score(450,65)` where the truth is `Score(810,55)` — it was counting dead stones as
alive.

**2. Four passes now end the game the same way whether it is played or replayed.** Verified against
pre-rewrite code, empty 9x9, four passes:

```
PLAYED    end=true   winner=Some(P2)  drops=None      fen=… 55 3 3
REPLAYED  end=false  winner=None      drops=Some(81)  fen=… 55 2 3
```

The auto-settlement lived only in `Variant.validPass`, which the replay path did not go through.
Unification adopted the played behaviour, so the replay path is what changed. The golden-oracle
fixture had recorded the *replayed* value, because its generator used the replay path — so the
fixture had to be corrected. Worth stating plainly: a golden corpus records what the code did, bugs
included. It behaved exactly as designed here — it refused to go green silently, named the game and
the ply, and forced a decision.

**3. A go position before its first stone now reports the area score of the position it is in,
rather than `Score(0, 0)`.** This supersedes a clause of ADR 0002, on evidence: **91 of 16,207
recorded plies change**, fields 5 and 6 only, every one moving from `0|0` to that ply's own FEN
score. 85 of them are ply 0; the other 6 are the drop-less prefixes of the two games curated for the
clause. No FEN, digest, drop, capture, end or winner value moves anywhere. The clause described an
evaluation schedule rather than a rule of go, and the tree already contradicted it — the exported FEN
read the position while `history.score` read zero at exactly those plies. Deriving the score ends
that disagreement rather than creating one. `Score(0, 0)` was never "no score" in any case: a
handicap game has stones and a real area score from ply 0.

## The lila patch

Two of these are source-breaking beyond the removal of `go.Api`.

### `go.Api` is gone

| Old (`strategygames.go.Api`) | New | Difference |
|---|---|---|
| `pieceMapFromFen(variantKey, fen): PieceMap` | `FEN(fen).pieces` | takes no variant and never throws. The old call required `variant.boardSize.height == fen.gameSize` and `sys.error`ed otherwise; that check now lives in `Forsyth.<<@`, which returns `None`. A caller relying on the throw as validation should call `Forsyth.validate` or `Forsyth.<<@` |
| `initialFen(variantKey): FEN` | `Variant(key).map(_.initialFen): Option[FEN]` | `Option` instead of `sys.error` on an unknown key. `Variant.orDefault(key).initialFen` for a total function. Values identical |
| `validateFEN(variant, fen: String)` / `validateFEN(fen: String)` | `Forsyth.validate(fen: FEN): Boolean` | takes a `FEN`, not a `String`. The `variant` overload always ignored its variant. Same regex, plus a structural read. Verified equal on 16,207 recorded FENs and all ten refusal cases. **Unchanged quirk:** the legacy nine-field form is still rejected by `validate` while `Forsyth.<<@` still reads it |
| `writeBoardFenFromPieceMap(pieceMap, variant)` | `Forsyth.boardRows(variant, pieces)` | argument order flipped to put the variant first. Output byte-identical. Still no pocket literal — `Forsyth.boardPart(board)` is the one that appends it |
| `removeDeadStones(deadStones, fen: String, variant)` | `Forsyth.removeDeadStones(variant, fen: FEN, squares)` | argument order, and `FEN` rather than `String` in and out. Behaviour preserved exactly, including that every field after the board is carried through untouched |
| `moveToUci`, `uciToMove`, `moveToPos`, `passMove` | **deleted, no replacement** | they encode the engine's integer point index and mean nothing without it. Use `Pos.fromKey` / `Pos.at` / `Uci` |
| `Api.Position.initialFen` | `Replay.setup` | the starting FEN is a property of the game record, which lives in lila, not of the position. No other logic exposes a per-position initial FEN |
| `Api.stonePocketData` | `PocketData.init` | identical value, already existed |

### `FEN.variant` is `Option[Variant]`

```scala
def variant: Option[Variant]   // was: Variant, sys.error on any other row count
```

A call site needs `.getOrElse(…)` or a `flatMap`. `Forsyth.<<` and `<<<` already returned `Option`,
so a go FEN whose board is no go size is now refused rather than throwing from inside a function
that returns an `Option`.

### The `goHistory` implicit is gone

```scala
implicit def goHistory(h: go.History): History   // removed
```

A `go.History` alone no longer determines a score, so the conversion cannot be written honestly. It
had no callers in this repo. Alongside it:

- **`strategygames.History.Go` gains a second parameter**, `areaScore: Score`. Any
  `case History.Go(h) =>` becomes `case History.Go(h, _) =>`.
- **`go.History.score` is gone.** Read it from the board, or from `strategygames.History.score`,
  which is unchanged in type and meaning.
- `History.apply(lib, …)` is unchanged in signature and behaviour, including its `score` argument.
- `strategygames.History.score` remains `val score: Score` on the parent, so no other logic's readers
  are affected.

### Behaviour a lila upgrade absorbs

The three changes above, plus: `Forsyth.<<@(variant, fen)` now returns `None` for a FEN it cannot
read, where it previously returned `Some` unconditionally or threw on a bad turn field. Callers that
matched on `Some` and never handled `None` will now see the empty case.

## Known limitations

**`Replay.boards` and `Replay.situations` are dead for go.** Both go through `pgn.Parser.sans`,
which is a stub answering `Not implemented iterable moves` for every go action string, so their
bodies have never executed. This is pre-existing and was deliberately left alone: their callers are
in the wrapper layer, so deleting them is an API change with a blast radius outside `go`, and
implementing `Parser.sans` is feature work. Neither belongs in a refactor whose contract is
behaviour preservation. `Replay.plyAtFen` had the same problem and *was* deleted, because it
provably never worked *and* nothing depended on its result — the wrapper now answers
`Validated.invalid` directly. The inconsistency is deliberate and wants its own decision.

**`GoSituationTest`'s stacked assertions overstate their coverage.** Several examples assert
multiple facts in sequence, where only the last failure is reported. Two reviewers flagged it
independently. Pre-existing and out of scope here, but real: an example that appears to pin four
things pins one loudly and three quietly.

**The golden-oracle corpus is gone, and what is lost is breadth.** The 85-game, 16,207-ply fixture
built in task 1 was migration scaffolding and was deleted with the engine it existed to verify
against. Every *rule* it carried was moved into a durable suite first — 13x13 gained four
independent pins, the ko sequence was widened to all three board sizes, and the position-hash and
board-state sweeps were re-homed onto scripted games covering capture, ko, pass and settlement. Two
facts died with the fixture on purpose: the `boardAfter` sweep, because `Drop.after` *is*
`boardAfter` and every drop test in the suite exercises it, and the 15,919-placement count, which is
a cardinality of the corpus and means nothing without it. What no longer exists is coverage across
85 real games; what remains is coverage of every rule, on positions chosen to reach it. Say it that
way rather than implying the coverage is unchanged.

**Every ratio against joansala or the deleted seam is cross-machine.** All three references are
deleted code, so no same-run denominator can be produced. A follow-up branch that wants a controlled
baseline has to pick a current number and re-measure on one box.
