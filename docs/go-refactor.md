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
| 4 | the settlement capture adjustment | `Replay`'s three `gameWithActionWhileValid` entry points vs `pgn.Reader` | three added it, the fourth did not — the same game got a different `history.captures` and a different exported FEN depending on how it was loaded. The `Uci`-list loaders were never in this group and still are not: see preserved quirk 2 |

Two more were *introduced* during the refactor and caught in review, and those are the tell. The
first: `Forsyth.validate` and `Forsyth.<<@`, two gates built in the same task, disagreed about which
turn-field strings a FEN may carry. The second: the task that reinstated "an action offered after a
settlement is illegal" put the guard on `Replay.gameWithActionWhileValid` and not on
`pgn.Reader.makeReplayWithActionStrs`, so a pass or a second `ss:` after a settlement loaded fine on
one loader and threw on the other — row 3 above, reopened by the fix for row 3, and caught only by
the whole-branch review. The shape is not carelessness; it is what happens when a behaviour has more
than one home, and it recurs even in work whose explicit purpose is removing it.

The four are closed by there being nowhere left to diverge, rather than by four fixes. There is now
one placement path (`Variant.boardAfter`), one settlement path (`Variant.boardAfterSelectSquares`),
one legality decision per action kind — `Replay.replayDrop`, `replayPass` and `replaySelectSquares`
all route through `Situation` to `Variant.drop` / `pass` / `selectSquares`, so no loader builds an
action without asking whether it is legal — and one helper that pairs a recorded settlement with its
capture accounting so an action-string loader cannot take one without the other
(`Replay.addSettlement`).

What the two introduced instances say about "a fifth would have to be written on purpose" is that it
would not, and an earlier draft of this document claimed otherwise. Both were written by accident, in
tasks whose subject was the defect itself. The structural claim worth making is the narrow one: the
*rules* have one home each now, so a divergence has to be introduced above them, in a loader. That is
the layer `GoPostSettlementTest` watches — every loader, every action kind, one refusal — because
that is the layer where this kept happening.

## How it follows the house conventions

The point of the branch was to make go look like the other eight logics. Concretely:

**The `Variant` vocabulary.** `boardAfter` is the placement path, named after
`togyzkumalak.Variant.boardAfter` and `backgammon.Variant.boardAfter`. `canDrop`/`validDrops` pair
the way `validMoves` and its predicate do elsewhere — `canDrop` short-circuits on the first legal
point instead of building a list nobody asked for. `winner`, `specialEnd` and `specialDraw` carry
the game's ending, `valid(board, strict)` is the cheap structural invariant, and the whole file sits
in the same position with the same member order as its neighbours.

**Named transitions on `Board`, after the backgammon precedent.** `passed`, `stonePlaced`, `settled`
and `withKo` are the only writers of `ko`, `consecutivePasses` and `deadStonesSelected` on the action
path, exactly as `backgammon.Board` owns `setDice`, `useDie` and `undoUseDie`. No rule advances
position state by any other route, so the fields cannot drift apart as a game is played. The one
place outside `go/Board.scala` that sets them is `Forsyth.<<@`, which builds a fresh board from a
FEN's own ko, pass-count and settlement fields — construction rather than transition, and the only
way a resumed game gets a position state at all.

**`Validated[String, A]` on the legality path.** `Variant.drop`, `pass` and `selectSquares` all
return it, and every path that decides legality goes through one of the three. The engine's
`GoFenError` taxonomy died with it by design; `Forsyth.validate` answers `Boolean` and `Forsyth.<<@`
answers `Option`.

**How a refusal reaches a caller, though, is not uniform, and an earlier draft of this document said
it was.** The `Uci` loaders — `Replay.apply(List[Uci], …)` and `situationsFromUci` — hand back
`Validated.invalid`. The action-string loaders throw: `Replay.replayDrop`, `replayPass` and
`replaySelectSquares` each end in `.valueOr(error => sys.error(…))`, and
`gameWithActionWhileValid` also `sys.error`s for an action offered to a finished game and for an
unreadable drop, which is how a throw gets past `pgn.Reader`'s own `Result.Incomplete` channel. That
is preserved behaviour, not something this branch introduced, and the suite reflects it — the
loader tests wrap in `Try`. **A lila integrator calling `Replay.gameFromUciStrings`,
`Replay.apply(actionStrs, …)`, `gameWithUciWhileValid` or `pgn.Reader` needs a `Try`, not a
`fold`.**

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

**Against joansala, on the path lila runs.** lila replays go through the wrapper, so the wrapper
rows are the ones that describe production: at 19x19, **30.1x faster if the consumer never reads a
score and 2.9x if it reads one every ply**. Which of the two is live is not known here and cannot be
settled from this repo — it depends on whether lila touches a history field per ply, and the same
library code costs 10.3x more if it does. Go is a scoring game whose score is usually on screen, so
the pessimistic number may be the real one.

Both figures are ahead of the tables and are arithmetic rather than a measurement: `879d0977`'s
measured post-fix 19x19 numbers (3205.6 ± 25.3 and 32947.3 ± 335.8 µs) over the same joansala
baseline the tables use, which is a different machine again. `docs/go-speed-results.md` still records
11.3x and 2.4x in its own table because that run predates `879d0977`, which deferred the wrapper's
`PieceMap` and took 5.1 ms off `wrapperReplay` and 5.5 ms off `wrapperReplayReadingEveryScore` at
19x19. **The tables have not been re-run**; the post-fix pair comes from a separate paired run whose
baselines reproduce them within a few percent.

**Against joansala, on the go package alone**, full-game replay through `go.Replay` is 9.3x / 16.4x
/ 29.7x faster, and a single placement — `Variant.boardAfter` — is 42x faster at 19x19. That
measures the rules, not the site, and `879d0977` did not touch it — the change is three lines in
`strategygames/Board.scala`. The two figures have since converged: on the score-unread path the
wrapper reaches 30.1x against the go package's 29.7x, so quoting either now says the same thing.
Quoting 29.7x still leaves out the score-reading consumer, which is where the remaining wrapper cost
lives.

**Against the seam this branch deleted**, replay at 19x19 is 11.7x slower than its batch path and
1.5x slower than its per-ply path. The batch path folded a whole game on one mutable scratch state
and published once; an immutable design that materialises a `Board` per ply does not reach that, and
was never going to. Legal-drop generation is the one workload untouched by any of this, and it goes
both ways: it costs 17–21x what the seam's *lazy* list cost, and 1.2–2.7x less than its *eager* one.

One design decision changed mid-branch on measurement, and it is the largest single number here. The
area score was first a strict field on `History`, copying togyzkumalak's *shape* without its
*reasoning* — togyzkumalak's score is accumulated captures, go's is a full-board flood fill, so the
copy ran a flood fill on every placement whether or not anyone wanted the number. Deriving it on
`Board` instead was worth **3.4x / 5.2x / 9.5x** on replay and **2.9x / 5.0x / 8.4x** on allocation,
and took `applyDrop` at 19x19 from 59.9 µs to 1.44 µs. Per ply, replay is now 6.6 / 7.4 / 8.1 µs —
nearly flat across board sizes, which is what it should look like once the one O(board area) term
per ply is gone.

One wrapper cost the tables name has since been paid off, and it is the reason the production figures
above are ahead of them. **`strategygames.Board` took its `pieces` as a strict constructor argument**,
so every `Board.Go(b)` rebuilt the whole go piece map into wrapper `Pos` and `Piece` whether or not
anything read it, once per ply on the wrapper replay path — ~27% of named samples in `wrapperReplay`
at 19x19, with per-ply overhead scaling with board area (3.4 / 6.3 / 13.1 µs). `879d0977` takes it by
name into a `lazy val`, the shape `5ed78cb1` had already used for the wrapped history on the same
class, and removes 5.1 ms from a 19x19 wrapper replay — 2.59x on the row that reads no score. All
nine logics gained it; `Board` is a `sealed abstract class` rather than a `case class`, so the
by-name parameter cost no subclass anything.

That closes the wrapper penalty on the score-unread path entirely rather than narrowing it. At 19x19
`wrapperReplay` measures 3205.6 ± 25.3 µs against the go package's own `replay` at 3243.2 ± 156.3 —
the same work, within noise, one wrapper `Game` per ply and all. The ~5.25 ms the tables charged to
the wrapper was this single map rebuild.

What is left is the cost the design knowingly accepted. The first is on the wrapper path, and it
leads because that is the path lila runs:

- **`strategygames.History.Go` takes the score as a strict parameter**, so reading *any* history
  field on a ply forces that ply's flood fill — 10.3x at 19x19 on a consumer that reads a score every
  ply, once the `pieces` cost is out of the way, and with that cost gone it is the only thing
  separating the two wrapper rows. Closing it needs either a by-name parameter (costing
  `History.Go` its `case class`, making go the only non-case-class of nine) or a board-carrying
  `History.Go` (which lila's `History.apply` factory cannot construct). `Board` had the first option
  cheaply because it is not a `case class`; `History.Go` does not. Both are larger than this branch
  sanctioned.
- **`PieceMap = Map[Pos, Piece]` and `Set[Pos]`, in the go package.** `Pos` is a case class, so every
  liberty test and every region step is a boxed structural hash and an equality against an immutable
  `HashMap` or `HashSet`. The deleted engine indexed a byte array by `Pos.index`. That representation
  difference — not the flood fill's asymptotics — is the constant factor under chain walking, FEN
  parsing and movegen alike, and it is the single named root cause in the go-package profile.
- **`History.hasOccurred`'s superko scan**, an O(plies) linear scan per ply, newly visible at ~4%. It
  was always there; `areaScore` dwarfed it.

Nothing else measured here was acted on. A follow-up branch takes the numbers as its input, and
`History.Go`'s strict score is what it should take first — it is the last item standing between a
wrapper consumer and the 10.3x, and the only wrapper item left at all.

That branch will live in `bench`, which no root task compiles: `sbt test` at the root does not
aggregate it, so a rename in `src/main/scala` can leave every benchmark uncompilable with nothing
red. `7c69c51d` did exactly that, and no go benchmark could run until `8904ce51`. Compile `bench`
explicitly before believing a measurement is available to take; `bench/README.md` carries the
verification steps.

## The four preserved quirks

Each is behaviour that stored games were written under, so each is preserved rather than fixed. The
reason sits at the code site; here is what fixing each one would take.

**1. A settlement records one capture more than it lifts.**
`Replay.settlementCaptureCount` is `stonesBefore - stonesAfter + 1`. *Fix:* delete the `+ 1`. What
it needs is a decision about the `history.captures` of every settled go game already in the
database.

**2. A settlement records captures on the loaders that fold action strings, and nowhere else.** Not
a played-versus-loaded split, which is how it was described until the whole-branch review measured
it. Four entry points can be measured directly:

| entry point | `history.captures` after `s@a1 s@e5 pass pass ss:a1` on `Go9x9` |
|---|---|
| `Replay.gameFromUciStrings` | `Score(2,0)` — the one stone lifted, plus one |
| `pgn.Reader.replayResultFromActionStrs` | `Score(2,0)` |
| `Replay.apply(List[Uci], …)` | `Score(0,0)` |
| `Replay.situationsFromUci` | `Score(0,0)` |

The first two go through `Replay.withSettlementCaptures`, which adds the count. The last two hand
each `Uci` to a `Situation` and so run the *played* path — `Variant.boardAfterSelectSquares`, which
does not. A game played through the site records `Score(0,0)` too, so the split is by mechanism, not
by played versus loaded.

The action-string group has two more members than the table shows, and both add the count the same
way, through `Replay.gameWithActionWhileValid`. `gameWithUciWhileValid` returns the adjusted game for
every ply. `Replay.apply(actionStrs, …)` computes the adjustment and then throws it away, because its
`.state` is the game after the *first* action rather than the last — see the known issue below —
which is why neither is in the measured table: one has no single state to quote and the other quotes
the wrong one. `GoSettlementCaptureTest` names the four that can be pinned. *Fix:* move the
adjustment into
`boardAfterSelectSquares` so every path agrees. Same blocker as quirk 1, and the same decision
resolves both — this is the divergence, quirk 1 is its arithmetic.

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

Three parts, in the order they matter: one thing lila has to start doing, the source-breaking
changes, and a set of values that move without any compile error.

### Required first: go games must now carry `positionHashes`

Before this branch, nothing in go ever wrote `positionHashes` —
`git grep positionHashes 237fbdf1 -- src/main/scala/go` returns the field declaration and nothing
else. It is now the superko history, and go legality is read from it
(`Variant.recreatesAnEarlierPosition` → `History.hasOccurred`).

**If lila persists and restores go history through `History.apply(GameLogic.Go(), positionHashes =
…)` and does not carry the array, every restored go game gets an empty superko history and the live
game will permit placements that a straight-through replay refuses.** That is the same defect this
branch spent a task closing inside `Replay`, relocated to the lila boundary where nothing in this
repo can catch it. Go's `repetitionEnabled` is `false` and the field was always empty, so a lila
persistence layer that quietly drops it is the likely status quo rather than an unlikely accident.
Check it before anything else in this patch.

Two consequences that follow from the same change:

- **`go.Hash.size` is 8 bytes per entry, where `strategygames.Hash.size` is 3.** Go is the only
  logic whose entries are that wide. `strategygames.History.toString` groups `positionHashes` by the
  generic size (`History.scala:56`), so that debug rendering is now meaningless for go. Cosmetic, and
  it is the only generic consumer in the repo.
- A 400-ply 19x19 game carries about 3.2 KB of position history where it carried none.

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
| `Api.stonePocketData` | `PocketData.init` | listed for completeness, not for lila: it was `private[go]`, so no call site outside the package could exist. Same pockets, one wrapper less — it was `Some(PocketData.init)` |

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
  which is unchanged in type. Its *meaning* on the go path is not unchanged — see below.
- `History.apply(lib, …)` is unchanged in signature and behaviour, including its `score` argument.
- `strategygames.History.score` remains `val score: Score` on the parent, so no other logic's readers
  are affected.

### A go `Board` can no longer produce a whole `FEN`

```scala
go.format.Forsyth.exportBoardFen(board: Board): FEN   // removed
go.Board.playerToMove: Player                          // removed
go.Board.withPlayerToMove(player: Player): Board       // removed
```

The removal is the point rather than a side effect: `go.format.FEN`'s accessors index by full-FEN
field positions, so a `FEN`-typed projection of a bare board would be a lie in the type. Build a
`Game` and call `Forsyth.>>` for a FEN; take the player from the `Situation` that has one.
`fairysf` and `samurai` keep their own `exportBoardFen` — only go's is gone. The runtime half of the
same change is the first entry below, and it is the one that will not announce itself.

### Values that move with no compile error

These are the ones to grep for. Nothing here fails to build; the numbers simply differ.

**`Forsyth.exportBoard(GameLogic.Go(), board)` returns eight fields where it returned ten, and
`Forsyth.boardAndPlayer(GameLogic.Go(), situation)` returns nine where it returned eleven.** This is
the most dangerous item in the patch: a caller gets a shorter string at runtime and the compiler
says nothing.

What it returns now is the board part with its pocket literal, then the fields that belong to the
board — ko point, both area scores, both capture counts, komi in tenths, pass count. Gone from it are
the turn symbol, which used to sit between the board part and the ko point, and the trailing
full-move number. `boardAndPlayer` still appends the player letter on the end, as it does for every
logic.

**Anything that wanted the whole FEN should call `Forsyth.>>(game)`**, which is unchanged and emits
the same ten-field string byte for byte. A 478-line corpus — three initial positions, seven scripted
games at every ply, nine handicap games, the eight upstream 19x19 games, each recorded as
`>>(game)`, `>>(situation)`, `>>(!situation)` and a reload round trip — was dumped before and after
the change and diffed identical. It was chosen to reach the three places the derivation moved: the
turn field, the inversion `!situation`, and the settled full-move quirk.

The result is **not a FEN**, and treating it as one fails quietly. `go.format.FEN`'s accessors index
by full-FEN field positions, so on a board-only string every field after the board is read one
position early — `player1Score` returns p2's score, `ko` reads a score field — and the two fields the
string no longer carries return their defaults: `fenPassCount` answers `0` whatever the real pass run
is, `fullMove` and `ply` answer `None`. Only `board`, `pieces` and `gameSize` still read correctly.
`boardAndPlayer` was never a valid go FEN either, before or after: the letter it appends is
`Player.letter`, which is `'w'` for P1, while a go FEN's turn field spells P1 as `"b"`.

Why it changed: every other game logic already returns board-only state from `exportBoard` —
`togyzkumalak` returns board plus score, `backgammon` board plus pockets plus dice — and go returning
a whole FEN derived from a bare `Board` was the last shape inherited from the deleted `Api.Position`
seam. It was also the only reason the two `Board` accessors above existed, and the only reason go's
`Situation.unary_!` rewrote the board's ply counter instead of being `copy(player = !player)` as it
is in all eight sibling logics. Removing the anomaly is what let all three go.

One consequence sits behind that deletion: `>>` now takes the turn symbol from
`game.situation.player`, where it used to derive it from `board.history.halfMoveClock`. Inside the
library the two cannot disagree — every construction site was checked — but a caller that builds
`strategygames.Situation(GameLogic.Go(), board, player)` with a mismatched pair now gets its own
player exported rather than the board's. The new answer is the more honest one; the point is that the
invariant is no longer enforced anywhere, and the one test that asserted it went with the member it
read.

**`go.History.halfMoveClock` now counts plies, where it counted something close to completed turns.**
Old (`237fbdf1:go/Board.scala`, `afterDrop`): `halfMoveClock + player.fold(0, 1)`, so it incremented
on P2's drop only, and not at all for a played pass or settlement — the old `Variant.validPass` and
`createSelectSquares` rebuilt the board with `board.copy(…)` and never touched history. New
(`Variant.afterOnePly`): `+ 1` on every ply, and set from `fen.ply` on parse. The field roughly
doubles. The change is forced by what reads it: with the engine gone, `Forsyth.fullMovePart` computes
the FEN's full-move number as `halfMoveClock / 2 + 1`, and that field has to keep the value it always
had. Two further derivations existed when this was written and no longer do —
`go.Board.playerToMove` and a private `Forsyth.playerToMove`, both
`Player.fromTurnCount(halfMoveClock)`, which between them produced the FEN's turn field. `>>` takes
the turn from `game.situation.player` now, and both went with `go.format.Forsyth.exportBoardFen`,
which is the entry above. Either way the *exported FEN* is unaffected. But
`strategygames.History.halfMoveClock` is a pass-through field, so any lila code
keyed on the raw value for a go game (a clock, a move counter, a stored-game migration) reads a
different number for the same game, with no compile error and no test in this repo that could
notice.

**`strategygames.Board.Go.withHistory` and `.copy(history)` discard the score they are handed.**
`case History.Go(h, _) => Go(b.withHistory(h))` (`Board.scala:253-281`) drops the second field on the
floor, and `Board.Go(b).history.score` is unconditionally `b.areaScore`. Before, a score installed
through `History.apply(GameLogic.Go(), score = …)` travelled inside `go.History.scoring` and survived
`withHistory`. For go the derived value is the truthful one and that is the point of the change, but
the consequence is that `strategygames.History.score` is now **unsettable** on the go path: a caller
that restores a persisted score and reads it back gets the position's own area score instead, with
no error. Nothing in this repo pins it.

**`Forsyth.<<@(variant, fen)` now returns `None` for a FEN it cannot read**, where it previously
returned `Some` unconditionally or threw on a bad turn field. Callers that matched on `Some` and
never handled `None` will now see the empty case.

## Known limitations

**`Replay.boards` and `Replay.situations` are dead for go.** Both go through `pgn.Parser.sans`,
which is a stub answering `Not implemented iterable moves` for every go action string, so their
bodies have never executed. This is pre-existing and was deliberately left alone: their callers are
in the wrapper layer, so deleting them is an API change with a blast radius outside `go`, and
implementing `Parser.sans` is feature work. Neither belongs in a refactor whose contract is
behaviour preservation. `Replay.plyAtFen` had the same problem and *was* deleted, because it
provably never worked *and* nothing depended on its result — the wrapper now answers
`Validated.invalid` directly. The inconsistency is deliberate and wants its own decision.

**`Ecopening.fromGame` can never resolve an opening for go**, for the same reason: it calls
`Replay.boards`, so the `Parser.sans` stub refuses every input before a board is built. Pre-existing
and not caused by this branch — verified against the tree before the change as well. The opening
lookup itself works; `Ecopening.matchChronoBoards` keys on `Forsyth.exportBoard(board)` and
`GoEcopeningTest` pins that a real played game resolves to the right ECO on all three sizes. Only the
`fromGame` entry is dead, and it now has a test named for the truth, so whoever implements
`Parser.sans` is told to come back and check the keys.

**`Replay.apply(actionStrs, …).state` is the game after the *first* action, not the last.**
`gameWithActions.reverse.lastOption` (`go/Replay.scala:81`) takes the last element of a reversed
chronological list. Measured: on `s@a1 s@e5 pass pass ss:a1` it returns the captures as they stood
after `s@a1`. The `actions` list and the `setup` are correct; only `state` is wrong, and only for a
game of more than one action. Pre-existing and not go's — the identical expression appears in
`backgammon`, `dameo`, `fairysf`, `abalone`, `samurai` and `togyzkumalak`, seven game logics in all.
Out of scope for this branch and it wants its own ticket. Two things bound the blast radius, and
neither makes it safe: `strategygames.Replay.apply` exposes only the `List[Uci]` form, which builds
its state through `addAction` and is correct, and nothing in `src/main` calls the action-string form.
A lila caller reaching a game logic's `Replay` directly is the exposure.

**A class-initialisation deadlock in `abalone/BoardType.scala` can hang a test run indefinitely.**
Observed once on this branch: a full suite run sat for 45 minutes with no output and no failure.
Unrelated to go, pre-existing, and wants its own ticket. The cycle is in the bytecode:
`HexBoardType.<init>` reads `BoardType$.MODULE$` to evaluate the defaulted `norm: Norm = N6`
constructor parameter, which Scala hosts on the companion; `BoardType$`'s static initialiser
evaluates `all = List(Hex5, Hex6)`, which reads `Hex5$.MODULE$` and `Hex6$.MODULE$`; and `Hex5$`'s
static initialiser calls `HexBoardType.<init>`. A single thread does not hang on that, because the
JVM's class-initialisation lock is re-entrant; two specs2 threads entering from opposite ends each
hold the lock the other is waiting on, and there is no timeout. *Fix:* break the cycle — pass `norm`
explicitly from `HexBoardType` so the constructor never touches the companion, or move `all` off the
companion. Worth checking while fixing it, though it has not been observed: the same cycle entered
from `Hex5$` first reaches `BoardType$`'s `all = List(Hex5, Hex6)` before `Hex5$.MODULE$` has been
assigned.

**A board standing at three consecutive passes does not survive a FEN round trip.**
`Forsyth.passCount` caps the rendered pass count at 2 (`consecutivePasses min highestPassCount`) and
the parse maps 1→1, 2→2 and anything else→0, so three renders as 2 and reloads as 2. Measured, `Go9x9`,
`pass pass pass`: the source board has `consecutivePasses = 3` and `canSelectSquares = false`; its
own exported FEN reloads to `consecutivePasses = 2` and `canSelectSquares = true`, and the resumed
game needs **two** further passes to settle where the source board needed one. Preserved — the old
field-8 renderer capped at 2 as well and the old `<<@` seeded exactly two synthetic passes — so it is
out of scope for a behaviour-preserving refactor, but the stakes went up: behaviour change 2 makes
passing the only way a game ends without a settlement. *Fix:* render and parse the real count. What it
needs is a decision about the FENs already written, which is the same blocker as the four quirks.
`GoBoardStateTest.resumptions` stops at two passes; adding `(List("pass","pass","pass"), "pass")` to
that list turns the asymmetry red.

**Not a limitation, though it was recorded as one: stacked assertions.** Two reviewers flagged
independently that examples asserting several facts one after another report only the last, so an
example appearing to pin four things pins one. Both were wrong.
`org.specs2.mutable.Specification` mixes
in thrown expectations: an example whose first statement is `1 === 2` and whose last is `3 === 3`
fails, and the reported failure is `1 != 2` at the *first* statement's line — measured on this
project, this specs2 version. Every stacked assertion in `GoSituationTest`, `GoChainTest` and
`GoBoardStateTest` is a real assertion. What is genuinely lost is only that a later failure is not
reported in the same run as an earlier one. This entry is kept as a retraction rather than deleted,
because the claim was raised twice and would otherwise be raised a third time.

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
