# `strategygames.go` — house conformance review

An independent read of `src/main/scala/go/` against the four comparator logics, commissioned to
answer one question: does the refactored go package read as code that belongs in this repository?
The reviewer worked from the code and from `dev`, and was told not to use `docs/go-refactor.md` as
evidence for anything.

## Reading this document

The review below is reproduced **verbatim**. It was an independent assessment and its text is not
edited. Everything added afterwards appears in a blockquote marked **Since the review**, and nothing
else in this file was touched.

**When it was taken.** The review states its baseline as `lakin/go-idiomatic-refactor` @ `e266d1c7`.
That commit is no longer reachable from the branch — the branch's fixup commits were folded and the
history rewritten. Its subject survives as `34b36e5e docs(go): retrue the performance claims to the
re-measured tables`, and the two trees differ by exactly one line, in
`src/main/scala/go/format/Forsyth.scala`: `e266d1c7` fails loudly on a FEN with no move number,
`34b36e5e` falls back to `0`. The loud failure was restored later by `8992dbd2`. Treat `34b36e5e` as
the baseline.

**What has landed since.** Thirteen commits, of which six change code:

| commit | |
|---|---|
| `32207199` | refuse an action after a settlement on every loader — `pgn.Reader` had two branches that built actions without asking legality |
| `0ce6ed71` | restore the board-size check `Sgf.render` lost; reword the superko-restriction note; record the three-pass FEN round trip |
| `7c69c51d` | export board-only state from `Forsyth.exportBoard` |
| `8904ce51` | follow that change through the go benchmark |
| `b79b20bc` | defer the wrapped piece map on `strategygames.Board` |
| `8992dbd2` | restore the loud failure on a FEN with no move number |

The remaining seven (`19524a4e`, `d8144795`, `20996659`, `42842a98`, `f8ab958c`, `5aa9153a`,
`be190b35`) are documentation.

**Where it stands.** Of the twelve residue items, **one is fixed** — R1, the one the review named as
most worth fixing, closed by `7c69c51d`. The other eleven are open, verified open against
`be190b35`. All five justified deviations still hold; JD5 has widened.

---

# `strategygames.go` — house conformance review

Independent read of `src/main/scala/go/` against `src/main/scala/togyzkumalak/`,
`src/main/scala/backgammon/`, `src/main/scala/abalone/` and `src/main/scala/dameo/`, plus the
shared façade files the refactor touched. Verified against the code and against `dev` (the
pre-refactor state); `docs/go-refactor.md` was not used as evidence for any claim below.
`sbt compile` succeeds on `lakin/go-idiomatic-refactor` @ `e266d1c7`.

Counts: **21 matches, 5 justified deviations, 12 residue items.**

---

## 1. Matches

Where go now follows the house, with comparator and evidence.

**M1 — `Variant` preamble, member for member.**
`go/variant/Variant.scala:15-47` is `togyzkumalak/variant/Variant.scala:15-47`: same
`private[variant]` constructor (`id`, `key`, `name`, `standardInitialPosition`, `boardSize`),
then `exotic`, `baseVariant`, `fenVariant`, `variableInitialFen`, `hasAnalysisBoard`,
`hasFishnet`, `p1IsBetterVariant`, `blindModeVariant`, `materialImbalanceVariant`,
`dropsVariant`, `onlyDropsVariant`, `hasGameScore`, `canOfferDraw`, `repetitionEnabled`,
`perfId`, `perfIcon`, `initialFen`, `pieces`, `startPlayer` — in that order, all `def`.

**M2 — Overridable configuration is `def`, not `val`.**
`Variant.komi: Double = 7.5` (`go/variant/Variant.scala:49`) overridden by
`Go9x9.scala:24`; `initialFen` overridden at `Go9x9.scala:21`, `Go13x13.scala:20`,
`Go19x19.scala:23`. Same convention as `togyzkumalak/variant/Variant.scala:51,57`
(`initialStoneCount`, `usesTuzdik`) and `Bestemshe.scala`'s overrides. The only `val`s on
the go `Variant` are `roles` and the singleton-identity members, exactly as togyzkumalak.

**M3 — `Variant` companion.**
`go/variant/Variant.scala:386-420` vs `togyzkumalak/variant/Variant.scala:240-269`:
`all`/`byId`/`byKey`/`default`/`apply(id)`/`apply(key)`/`orDefault` ×2/`byName`/`exists`/
`openingSensibleVariants`/`divisionSensibleVariants`, and the same reference-equality
`equals` + `hashCode = id` at `go/variant/Variant.scala:377-379`.

**M4 — `Validated[String, A]` on every legality entry point.**
`Variant.drop`/`pass`/`selectSquares` (`go/variant/Variant.scala:176-198`) mirror
`togyzkumalak/variant/Variant.scala:172-181` and `backgammon/variant/Variant.scala`'s
`drop`/`lift`/`diceRoll`; `Situation.drop`/`pass`/`selectSquares`
(`go/Situation.scala:52-58`) delegate to the variant exactly as
`togyzkumalak/Situation.scala:48-52` and `backgammon/Situation.scala:375-386`.
`Option` is used for absence throughout (`Board.ko: Option[Pos]`,
`Variant.koPointAfter: Option[Pos]`, `Chain.capturesUnlessSuicide: Option[Set[Pos]]`).

**M5 — Position state as ordinary `Board` fields, in the house slot.**
`go/Board.scala:12-20` appends `komi`, `ko`, `consecutivePasses`, `deadStonesSelected` after
`pocketData`, precisely where `backgammon/Board.scala:13-15` appends `unusedDice` and
`cubeData`.

**M6 — Named board transitions rather than ad-hoc `copy`.**
`Board.passed`/`settled`/`stonePlaced`/`withKo`/`withHistoryStartingHere`
(`go/Board.scala:68-83`) are the same idiom as `backgammon/Board.scala:27-45`
(`setDice`, `useDie`, `undoUseDie`, `initialiseCube`).

**M7 — `Board` boilerplate block is line-for-line house.**
`apply(at)`, `apply(file, rank)`, `actors`, `posMap`, `piecesOnBoardCount`,
`playerPiecesOnBoardCount`, `withHistory`, `updateHistory`, `withVariant`, `withPocketData`
×3, `ensurePocketData`, `situationOf`, `valid`, `materialImbalance`, `toString`
(`go/Board.scala:23-97`) against `togyzkumalak/Board.scala:13-64` and
`backgammon/Board.scala:98-134`. Companion `apply`/`init`/`BoardSize` sealed abstract class +
case objects (`go/Board.scala:100-171`) matches `togyzkumalak/Board.scala:67-107`, including
the retained `// def empty(variant: Variant)` comment.

**M8 — `History` shape.**
`go/History.scala:15-21` is `togyzkumalak/History.scala:7-14` with one slot substituted:
`lastTurn`, `currentTurn`, `positionHashes`, `halfMoveClock`, plus the one genuinely
accumulated field (`captures: Score` where togyzkumalak has `score: Score`). The three
derived `lazy val`s `lastAction`/`recentTurn`/`recentTurnUciString`
(`go/History.scala:23-29`) are character-identical to `togyzkumalak/History.scala:16-22`.

**M9 — Superko rides the existing repetition field.**
`positionHashes: PositionHash` (`go/History.scala:18`) with `positionCount`, `positionAt`,
`currentPosition`, `hasOccurred`, `afterPosition`, `startingAtPosition`
(`go/History.scala:31-43`). Nothing new was invented: the hash comes from go's existing
polyglot table via a new `Hash.mask` (`go/Hash.scala:47`), and `PositionHash` is the
package alias every logic already uses (`go/package.scala:13`).

**M10 — Action types.**
`Drop` (`go/Drop.scala:6-39`), `Pass` (`Pass.scala:6-33`), `SelectSquares`
(`SelectSquares.scala:6-34`) all carry `situationBefore`, `autoEndTurn`,
`metrics: MoveMetrics = MoveMetrics()` and expose `situationAfter`, `finalizeAfter`,
`applyVariantEffect`, `player`, `withMetrics`, `toUci`, `toString` in that order. The
`finalizeAfter` body — the `lastTurn`/`currentTurn` swap keyed on `autoEndTurn` — is
identical to `togyzkumalak/Move.scala:22-27`.

**M11 — `Action` base class matches the togyzkumalak form.**
`go/Action.scala:6-19` is `togyzkumalak/Action.scala` plus `applyVariantEffect`: a single
`situationBefore` parameter, not the three-parameter
`(situationBefore, after, metrics)` form of abalone/dameo/backgammon. This is the form that
makes a derived `after` possible without disturbing the house shape (see JD2).

**M12 — `Replay` follows togyzkumalak step for step.**
`go/Replay.scala:21-368` vs `togyzkumalak/Replay.scala:14-325`: `case class Replay(setup,
actions, state)` with `chronoPlies`/`chronoActions`/`addAction`; companion with
`apply(game)`, the five-argument `apply` returning `Reader.Result`, the `goAction` /
`togyzkumalakMove` guard with the same `// TODO: because this is primarily used in a
Validation context` comment, `actionStrsWithEndTurn`, `combineActionStrsWithEndTurn`,
`gameWithActionWhileValid`, `gameWithUciWhileValid`, `recursiveSituations`,
`recursiveSituationsFromUci`, `recursiveReplayFromUci`, `initialFenToSituation`, `boards`,
`situations`, `boardsFromUci`, `situationsFromUci`, `apply(ucis, …)`, `makeGame`. The
collapse from two replay paths to one left the house shape intact.

**M13 — Local mutation is confined exactly where the house confines it.**
The only `var`s in the entire go package are `go/Replay.scala:181-182` — the same two, in
the same function, as `togyzkumalak/Replay.scala:114-115`. Nothing mutable escapes a method
body anywhere in `src/main/scala/go/`.

**M14 — The flood fill is a `@tailrec` recursion, not a `while`/`var` loop.**
`Chain.grownFrom` (`go/Chain.scala:98-110`) and `Variant.emptyRegionsOf`'s `foldLeft`
(`go/variant/Variant.scala:315-327`) are purely functional. This is *stricter* than the
house tolerates — `abalone/variant/Variant.scala:75-140` uses nested `while` loops over
seven `var`s for the same class of problem.

**M15 — Case classes with `copy`, no escaping state.**
`Chain.Stones` is a `private case class` whose mutators are `copy`-based
(`go/Chain.scala:62-67`); every `Board` transition is a `copy`. No `enum`, no opaque types,
no `given`/`using`, no `extension` anywhere in `src/main/scala/go/` (verified by grep) —
Scala 3 written in the 2.13 dialect, matching all four comparators.

**M16 — Shared types reused rather than reinvented.**
`strategygames.Score` for both `History.captures` (`go/History.scala:20`) and
`Variant.areaScore` (`go/variant/Variant.scala:296`), including its `add`
(`go/variant/Variant.scala:247`); `MoveMetrics`, `GameMessage`, `Player`, `Status` all
imported, none shadowed.

**M17 — Engine-era bespoke abstractions genuinely deleted, not renamed.**
`dev`'s `trait NextBoard` / `ExplicitBoardAfter` / `LazyBoardAfter` (dev
`go/Drop.scala:6-15`) collapsed to a plain `lazy val after` (`go/Drop.scala:18`);
`Board.uciMoves`, `Board.position: Option[Api.Position]` and `Board.apiPosition` (dev
`go/Board.scala:14-15,54-60`) are gone; `Variant.validPass`'s two `return` statements (dev
`go/variant/Variant.scala:98-124`) are now a single expression
(`go/variant/Variant.scala:127-133`). Grep for `Api`, `apiPosition`, `uciMoves`,
`GameResult` across `src/main/scala/go/` returns nothing.

**M18 — `Situation` skeleton.**
`go/Situation.scala:27-81` against `togyzkumalak/Situation.scala:16-71`: `history`, the
`checkMate`/`staleMate` stubs with the inherited comment, `private def variantEnd`, `end`,
`winner`, `playable(strict)`, the three-branch `lazy val status`, `withVariant`, `unary_!`,
and `object Situation { def apply(variant) }`.

**M19 — `Forsyth` object layout, and it is more functional than the comparators.**
`initial`, `<<@`, `<<`, `SituationPlus` (same `turnCount`/`plies` bodies, same
`// when we get a multiaction variant` comment), `<<<@`, `<<<`, the three `>>` overloads,
`exportBoard`, `boardPart`, `boardAndPlayer` ×2 — `go/format/Forsyth.scala:32-209` vs
`togyzkumalak/format/Forsyth.scala:15-113`. go's `renderedRank` is a pure `foldLeft`
(`go/format/Forsyth.scala:161-174`) where `togyzkumalak/format/Forsyth.scala:84-105` and
`backgammon/format/Forsyth.scala:91-113` still use a `StringBuilder` and `var empty`.

**M20 — `FEN` value class.**
`final case class FEN(value: String) extends AnyVal` with `clean` and named field-index
members on the companion (`go/format/FEN.scala:7,99-107`) matches
`togyzkumalak/format/FEN.scala:6,84-90`.

**M21 — Everything the refactor did not need to touch is still house-shaped.**
`Actor` is the four-field data-only stub (`go/Actor.scala:3-7`), matching
`togyzkumalak/Actor.scala`, `backgammon/Actor.scala`, `abalone/Actor.scala` — go correctly
did *not* follow chess/dameo, whose `Actor` carries move generation.
`go/package.scala` matches `togyzkumalak/package.scala`. `Uci`, `UciDump`, `UciCharPair`,
`Visual`, `Sgf`, `pgn/{Binary,Dumper,Parser,parsingModel,Reader}` keep their house shapes,
including the `Validated.invalid("Not implemented…")` `Parser` stubs togyzkumalak also
carries. `Pos`/`File`/`Rank`/`Piece`/`Role` changed only by introducing and using
`File.count` / `Rank.count` (`go/File.scala:49`, `go/Rank.scala:68`).

> **Since the review — matches.** The twenty-one matches were not re-audited item by item; the
> three the later commits could have disturbed were.
>
> - **M13 holds.** `grep -rn '\bvar \b' src/main/scala/go/` still returns exactly two hits, both in
>   `Replay.gameWithActionWhileValid` (now `go/Replay.scala:197-198`).
> - **M17 holds.** `grep -rn '\bApi\b\|apiPosition\|uciMoves' src/main/scala/go/` still returns
>   nothing.
> - **M18 and M19 were changed for the better by `7c69c51d`.** `Situation.unary_!` is now
>   `copy(player = !player)` (`go/Situation.scala:74`), which is what every comparator has, and
>   `Forsyth.exportBoard` replaced `exportBoardFen`. See R1.
> - **M8 line numbers have drifted** throughout, since the go files have been edited; the claims
>   were checked against content, not against the cited lines.

---

## 2. Justified deviations

**JD1 — `go/Chain.scala`, a file no other logic has. Justified.**
A *chain* is the standard Go term for a maximally-connected set of same-colour stones, and
the file's public surface is entirely rules vocabulary: `at`, `liberties`, `hasLiberty`,
`capturedBy`, `capturesUnlessSuicide` (`go/Chain.scala:13-50`). `variant.Variant` is written
in those words — `isPlayable` reads
`Chain.capturesUnlessSuicide(…)` (`go/variant/Variant.scala:103-104`), `boardAfter` reads
`Chain.capturedBy` (`:239`), `koPointAfter` reads `Chain.at`/`Chain.liberties`
(`:272-274`). It is not a utility bag: there is no `GoUtils`-shaped grab of unrelated
helpers, and the one general-purpose function (`regionFrom`, `:18`) is deliberately
`private[go]`. Precedent for a domain-noun helper file alongside the standard set exists in
`backgammon/CubeData.scala`, `abalone/BoardType.scala`, `abalone/norm/Norm.scala`.
Smallest that works? Very nearly. The one blemish: `regionFrom` is used by
`Variant.emptyRegionsOf` (`go/variant/Variant.scala:322`) to walk *empty* regions, which are
not chains — so the file's single non-chain export lives under a chain name. Sharing one
flood fill instead of writing a second is clearly right (and the comment at
`Chain.scala:92-97` explains why), so this is a naming wrinkle, not residue.

> **Since the review — JD1 unchanged.** `regionFrom` is still `private[go]`
> (`go/Chain.scala:18`), still the single non-chain export, and still called by
> `Variant.emptyRegionsOf` (`go/variant/Variant.scala:327`). The naming wrinkle stands.

**JD2 — `Drop.after` derived as a `lazy val`. Justified, and it is a house improvement.**
`go/Drop.scala:18`, `Pass.scala:12`, `SelectSquares.scala:13`. Go enumerates up to 361
candidate placements per `validDrops` (`go/variant/Variant.scala:83-95`); building an
after-board for each eagerly means 361 flood fills for a call whose only consumer is a list
of legal points. Two things make this justified rather than a departure: (a) it is not new —
`dev` already had it, via a bespoke `NextBoard`/`LazyBoardAfter` thunk-holder pair (dev
`go/Drop.scala:6-15`), and the refactor replaced that with the plain Scala idiom; (b) the
repo already solves the same problem on the same path, in backgammon, with
`lazy val lazySituationAfter` (`backgammon/Drop.scala:28`). This is the smallest form
available: because `togyzkumalak/Action.scala` — the model — already takes only
`situationBefore`, go needed no change to the shared `Action` shape. Applying it uniformly
to all three action types is right; three action types with three different `after`
conventions would read worse than one consistent departure.

> **Since the review — JD2 unchanged.** All three `lazy val after` declarations are still in
> place: `go/Drop.scala:18`, `go/Pass.scala:12`, `go/SelectSquares.scala:13`.

**JD3 — Four position fields on `Board`. Justified.**
`go/Board.scala:17-20`. Each is genuinely per-position and none is derivable from the
stones: `komi` arrives in the FEN and varies per game (handicap), `ko` is a one-ply
prohibition that must survive a FEN round trip (`go/variant/Variant.scala:266-269`),
`consecutivePasses` is what the four-pass rule (`:139-140`) and `Situation.canSelectSquares`
(`go/Situation.scala:61`) read, `deadStonesSelected` is the terminal flag. Precedent is
`backgammon/Board.scala:14-15`. Four is more than anyone else carries, but Go has more
per-position state than anyone else — this is the rules, not the shape. Smallest that works:
essentially yes. `consecutivePasses` + `deadStonesSelected` could be folded into one phase
value, but that would be *less* like the house, which uses flat fields with defaults.

> **Since the review — JD3 unchanged.** The four fields are still at `go/Board.scala:17-20`.
> `0ce6ed71` added a related known limitation rather than a change: a board standing at three
> consecutive passes does not survive a FEN round trip, because `Forsyth.passCount` caps the
> rendered count at 2. Recorded under "Known limitations" in `docs/go-refactor.md`. It bears on
> `consecutivePasses`, but it is a FEN-encoding fault, not an argument against the field.

**JD4 — `Board.areaScore` as a `lazy val` rather than a `History` field. The argument holds,
and it landed in the right place.**
The test that settles it: is the value reconstructible from the position alone? Area score is
stones-on-board + solely-enclosed empty points + komi (`go/variant/Variant.scala:296-308`) —
every input is on the `Board`. `togyzkumalak.History.score`
(`togyzkumalak/History.scala:11`) is *not* reconstructible: it is a running capture total.
Go's true analogue of that is `History.captures`, and it correctly stayed on `History`
(`go/History.scala:20`). The placement is the house's own derived-state block —
`areaScore` at `go/Board.scala:49` sits directly beneath `actors`, `posMap`,
`piecesOnBoardCount`, `playerPiecesOnBoardCount` (`:26-35`), which are `lazy val`s in
togyzkumalak and backgammon too. The rule-on-`Variant` / memo-on-`Board` split does mirror
`materialImbalance` (`go/variant/Variant.scala:341`, `go/Board.scala:95`), with one honest
caveat: `materialImbalance` is a plain `def` in every logic, so the *memoisation* is go's own
addition, justified by measured cost rather than by precedent. Two consequences worth
holding onto, both acknowledged in the code: the wrapper's `History.score` for a stoneless go
position now reports komi rather than `Score(0,0)` (`go/Board.scala:46-47`), and the memo is
per-`Board`, so a caller that reconstructs boards gets no reuse.

> **Since the review — JD4 unchanged.** `lazy val areaScore` is still at `go/Board.scala:49`,
> still beneath the derived-state block, and the komi-for-a-stoneless-position consequence is
> still stated in the comment above it.

**JD5 — `strategygames.Board` takes its history by name. Acceptable and idiomatic, but the
cost is real and the second-order asymmetry is the worse half.**
`src/main/scala/Board.scala:7,10` turns `val history: History` into
`wrappedHistory: => History` plus `lazy val history`. By-name-into-`lazy val` is a standard
2.13-dialect idiom, `History` wrappers are pure constructions, and nothing observable
changes for the other eight logics. It is also close to forced: `History.Go` must be handed a
`Score`, case-class parameters cannot be by-name, and promoting the abstract
`History.score` from a constructor `val` to an overridable `def` would touch all nine
`extends History(score = …)` sites. So of the available options this is the smallest.

The part that is *not* forced, and that a reader pays for, is downstream:
`History.Go(h: go.History, areaScore: Score)` (`src/main/scala/History.scala:108`) is now
the only two-field case among nine, which produced `History.Go(h, _)` patterns at four sites
in `src/main/scala/Board.scala:256-279` and the deletion of the `implicit def goHistory`
conversion that all eight siblings still have (`src/main/scala/History.scala:146-153`).
Anyone reading `History.scala` now has to know go's scoring story to parse the file. That
is a defensible price for the measured win, but it should be recorded as a price, not as a
neutral refactor.

> **Since the review — JD5 has widened, and one leg of the argument weakened.** `b79b20bc` added
> a *second* by-name constructor parameter, `wrappedPieces: => PieceMap`, held in
> `lazy val pieces` (`src/main/scala/Board.scala:6,13`). Its justification is the same shape as
> the history one and its measurement is in the commit message: a 19x19 wrapper replay went
> 8300.2 → 3205.6 us/op with nothing reading `pieces` at all.
>
> This cuts both ways. The idiom is no longer a go-shaped exception on the façade — two of
> `Board`'s parameters are now by-name and the deferral benefits all nine logics — so the
> "go alone forced this" framing is weaker than when the review was written. But the second-order
> cost the review called the worse half is untouched and still go-only:
> `History.Go(h: go.History, areaScore: Score)` is still the sole two-field case
> (`src/main/scala/History.scala:108`), the four `History.Go(h, _)` patterns are still at
> `src/main/scala/Board.scala:258,263,276,281`, and go is still the one logic without an
> `implicit def …History` conversion (`src/main/scala/History.scala:146-153`). The review's
> instruction — record it as a price — stands.

---

## 3. Residue

Ordered by what is worth fixing.

**R1 — `Forsyth.exportBoardFen` still exports a whole FEN from the `Board` alone, including
the turn field. This is the last engine-era shape in the package, and it is the cause of
three separate house deviations.**

`go/format/Forsyth.scala:129-145` builds all ten fields from `board`, deriving the player
from `board.history.halfMoveClock` (`:185-186`); `>>(game: Game)` at `:124` is therefore
`exportBoardFen(game.situation.board)` and ignores `game.situation.player`, `game.plies` and
`game.turnCount` entirely. Every comparator does the opposite — `exportBoard(board)` renders
board-only state and `>>(game)` splices in the player and full-move from the `Game`:
`togyzkumalak/format/Forsyth.scala:69-81`, `backgammon/format/Forsyth.scala:79-89`,
`abalone/format/Forsyth.scala:63-80`. The go signature is inherited verbatim from the engine
era, where it was `board.apiPosition.fen` patched field-by-field (dev
`go/format/Forsyth.scala:77-116`); the engine went, the shape stayed.

Two further deviations exist only to serve it:

- `Board.playerToMove` / `Board.withPlayerToMove` (`go/Board.scala:85-89`) — nothing else in
  `src/main` calls them. `withPlayerToMove` expresses "the other player is to move" by
  adding ±1 to `halfMoveClock`, a ply counter, which then feeds `fullMovePart`
  (`go/format/Forsyth.scala:194-195`).
- `Situation.unary_!` (`go/Situation.scala:74`) is consequently the only one of nine that
  touches the board. Every comparator is `copy(player = !player)`
  (`togyzkumalak/Situation.scala:64`, `backgammon/Situation.scala:401`,
  `abalone/Situation.scala`, `dameo/Situation.scala`). In go, `!situation` silently moves the
  game's ply count.

What the house would do: `exportBoardFen(board)` renders fields 1 and 3-9 only; `>>(game)`
supplies the turn from `game.situation.player` and the full move from `game.fullTurnCount`;
`boardAndPlayer` is unchanged; `Board.playerToMove` and `withPlayerToMove` are deleted;
`unary_!` becomes `copy(player = !player)`. Size: **medium** — roughly a 30-line rewrite of
`exportBoardFen`/`>>`, two `Board` members and one `Situation` line deleted, and a check of
the two façade call sites that hand over a bare `Board`
(`src/main/scala/format/Forsyth.scala:242,268`), which would then receive a board-only
string, which is what they already receive for every other logic. This is the single change
that removes three deviations at once.

> **Since the review — R1 is FIXED**, by `7c69c51d refactor(go): export board-only state from
> Forsyth.exportBoard`, with `8904ce51` following it through the go benchmark. Verified against
> `be190b35`:
>
> - `exportBoardFen` is gone. `Forsyth.exportBoard(board): String`
>   (`src/main/scala/go/format/Forsyth.scala:135`) returns `boardPart` plus a seven-element
>   `positionPart` — ko point, both area scores, both capture counts, komi in tenths, pass count.
>   Eight fields. No turn symbol, no full-move number.
> - `>>(game)` (`:124-133`) takes the player from `game.situation.player` and splices it between
>   the board part and the position part, the way the comparators do.
> - `Board.playerToMove` and `Board.withPlayerToMove` are deleted — `src/main/scala/go/Board.scala`
>   no longer contains either.
> - `Situation.unary_!` is `copy(player = !player)` (`src/main/scala/go/Situation.scala:74`), so go
>   is no longer the odd one of nine.
>
> **One half of the prescription was not taken, and it is worth knowing.** The review asked for the
> full move to come from `game.fullTurnCount`. It still comes from the board:
> `fullMovePart(board, playerToMove)` (`:194-202`) computes `board.history.halfMoveClock / 2 + 1`.
> The `Game` supplies the player and nothing else. The full-move field therefore still depends on
> `halfMoveClock` being right, which was the second-order complaint behind `withPlayerToMove` — the
> writer of that dependency is gone, the reader is not.
>
> This is a lila-visible runtime change with no compile error on the caller side:
> `Forsyth.exportBoard(GameLogic.Go(), board)` returns eight fields where it returned ten, and
> `boardAndPlayer` nine where it returned eleven. `docs/go-refactor.md` documents it under
> "Values that move with no compile error" and calls it the most dangerous item in the patch.

**R2 — `Situation.dropsAsDrops` / `drops` / `dropsByRole` are `def`s that each re-enumerate
every legal placement.**
`go/Situation.scala:15-19`. `backgammon/Situation.scala:24` makes the same member a
`lazy val`; `togyzkumalak/Situation.scala:12` does the same for `moves`. go's own file
already uses `lazy val` for `status` (`:42`) and `gameMessage` (`:66`), so this is
inconsistent even internally. It bites because `Variant.possibleDrops` and
`possibleDropsByRole` each call `validDrops` independently
(`go/variant/Variant.scala:200-212` — byte-identical to
`backgammon/variant/Variant.scala:541-553`), so a caller asking for both `drops` and
`dropsAsDrops` runs the full 361-point legality sweep twice, and go's sweep is the most
expensive in the repo (a flood fill per candidate point). What the house would do:
`lazy val dropsAsDrops`, with `drops` and `dropsByRole` derived from it. Size: **three
lines.** This is the clearest missed `lazy val` in the package and it is in the file the
brief flagged as under-reviewed.

> **Since the review — R2 open, unchanged.** All three are still `def`s
> (`src/main/scala/go/Situation.scala:15-19`), and `possibleDrops` / `possibleDropsByRole` still
> call `validDrops` independently (`src/main/scala/go/variant/Variant.scala:205-217`).

**R3 — `Forsyth.validate` and its regex are an orphaned engine artefact.**
`go/format/Forsyth.scala:28-30,63-65`. On `dev`, `variant.valid(board, strict)` was
`Api.validateFEN(Forsyth.exportBoard(board))` (dev `go/variant/Variant.scala:212`) — the FEN
grammar *was* the structural check. The refactor correctly narrowed `valid` to
`board.pieces.keys.forall(boardSize.onBoard)` (`go/variant/Variant.scala:364-365`) and said
why, then kept a hand-reimplemented FEN grammar checker that nothing in `src/main` calls.
Its only callers are tests (`src/test/scala/go/GoForsythTest.scala:40,105-134`), and it is
not reachable through `strategygames.format.Forsyth`, which has no `validate`. No other
pure-Scala logic has a FEN validator at all — only the two engine-bound ones do
(`fairysf/Api.scala:297`, `samurai/Api.scala:258`), and both are called from `variant.valid`.
The *live* half of that block, `describes` (`:67-75`), is legitimately used by `<<@` and
must stay. What the house would do: delete `validate` and `tenFieldShape`, keep `describes`,
and let `<<@` returning `None` be the answer to "is this FEN acceptable" — which several of
the tests already assert (`GoForsythTest.scala:131-134`). Size: **small**, ~10 lines plus a
test rewrite.

> **Since the review — R3 open, unchanged.** `validate` and `tenFieldShape` are still at
> `src/main/scala/go/format/Forsyth.scala:28-30,63-65`, `describes` is still live and still
> `private`, and `validate`'s only callers are still the seven sites in
> `src/test/scala/go/GoForsythTest.scala`.

**R4 — Magic-number policy is split-brained across `Forsyth.scala` and `FEN.scala`, and one
constant is duplicated across packages.**
`go/format/Forsyth.scala:16-24` names nine private constants, three of which are noise by
house standards (`decimalBase = 10`, `digitAppendedBySettlement = 1`,
`firstCountedField = 3`). Meanwhile `go/format/FEN.scala:51-59` reads the *same* FEN with
bare literals (`intFromFen(3)` … `intFromFen(7)`, `lift(0)` at `:84`), and its companion
names only `playerIndex` and `koIndex` (`:103-105`). Worse:
`Forsyth.fenTenths = 10` (`go/format/Forsyth.scala:20`) and
`Variant.fenTenthsPerPoint = 10` (`go/variant/Variant.scala:388`) are two private names for
one FEN convention in two files, and `Variant.fenFromSetupConfig`
(`go/variant/Variant.scala:51-60`) then uses a bare `10` for the same thing plus a literal
`"[SSSSSSSSSSssssssssss]"` that duplicates `Forsyth.pocket` (`:18`). No comparator names
constants at this density — `togyzkumalak/format/Forsyth.scala` and
`backgammon/format/Forsyth.scala` use bare literals throughout. What the house would do:
one convention, applied to both files; FEN field indices on the `FEN` companion where
`playerIndex` already lives; one shared tenths-per-point; `fenFromSetupConfig` using
`Forsyth.pocket` instead of a second copy of the string. Size: **small**, ~20 lines across
three files, no behaviour change.

> **Since the review — R4 open, unchanged.** Every cited site is still as described:
> nine constants at `go/format/Forsyth.scala:16-24`; bare `intFromFen(3)`…`intFromFen(7)` at
> `go/format/FEN.scala:51-59` and `lift(0)` at `:84`; `Forsyth.fenTenths = 10` at
> `go/format/Forsyth.scala:20` beside `Variant.fenTenthsPerPoint = 10` at
> `go/variant/Variant.scala:393`; and `fenFromSetupConfig` (`go/variant/Variant.scala:51-60`)
> still holding both a bare `10` and its own copy of `"[SSSSSSSSSSssssssssss]"`.

**R5 — `strategygames.Replay.plyAtFen` is now a hard-coded failure, for go and only for go.**
`src/main/scala/Replay.scala:732-733` returns
`Validated.invalid("plyAtFen is not implemented for go")` where the other eight delegate.
`go.Replay.plyAtFen` was deleted; on `dev` it was structurally identical to
`togyzkumalak/Replay.scala:280-316`. Both are equally dead in practice — both are gated
behind a `Parser.sans` stub that always fails — but the refactor's own reasoning is
inconsistent: `go.Replay.situations`/`boards` were *kept*, with a `TODO(go)` explaining that
deleting them is an API change because the callers live in the wrapper layer
(`go/Replay.scala:316-320`). That argument applies verbatim to `plyAtFen` and here reached
the opposite conclusion. What the house would do: restore `plyAtFen` as the togyzkumalak copy
carrying the same TODO, and restore the one-line delegation. Size: **small**, ~35 lines
restored plus one wrapper line.

> **Since the review — R5 open, unchanged in code, and now argued for in prose.**
> `src/main/scala/Replay.scala:733` still returns `Validated.invalid("plyAtFen is not implemented
> for go")` while the other eight delegate, and `go.Replay.situations` still carries its `TODO(go)`
> (`src/main/scala/go/Replay.scala:329-333`). `docs/go-refactor.md` now states the position under
> "Known limitations": the two were treated differently because `plyAtFen` provably never worked
> *and* nothing depended on its result, and it closes with "The inconsistency is deliberate and
> wants its own decision." That answers the review's charge of inconsistent reasoning; it does not
> close the item.

**R6 — Comment density is 5-20× the comparators, and a distinct subset of it is changelog.**
Measured (comment lines / total): `go/variant/Variant.scala` 92/420 = 22%,
`go/Chain.scala` 33/114 = 29%, `go/Board.scala` 24/171 = 14%, `go/Replay.scala` 31/368 = 8%.
Comparators: `togyzkumalak/variant/Variant.scala` 10/269 = 3.7%,
`backgammon/variant/Variant.scala` 19/693 = 2.7%, `abalone/variant/Variant.scala`
18/447 = 4.0%, `dameo/variant/Variant.scala` 2/153 = 1.3%, `togyzkumalak/Board.scala`
1/107 = 0.9%, `backgammon/Board.scala` 4/169 = 2.4%. Much of go's prose is good domain
writing the house simply lacks, and I would not strip it wholesale. But one subset is
narration about the refactoring process rather than about the code, and it is the clearest
residue of *how* the file was made rather than of what it does:
`go/variant/Variant.scala:137-138` ("before this refactor only the played path knew about
it"), `go/Board.scala:47` ("That superseded a clause of ADR 0002; ADR 0003 says so"),
`go/Chain.scala:96-97` ("The sharing is the mitigation; there is no test standing behind
this"), `go/Replay.scala:126,150` ("preserved rather than fixed"), `go/Drop.scala:13-16`.
No comparator file refers to its own history anywhere. What the house would do: keep the
rules explanations, move the process narration to `docs/`. Size: **small**, prose only.

> **Since the review — R6 open. Re-measured on `be190b35`:** `go/variant/Variant.scala` 97/425 =
> 23%, `go/Chain.scala` 33/114 = 29%, `go/Board.scala` 24/165 = 15%, `go/Replay.scala` 37/384 =
> 10%. Every ratio is unchanged or slightly up.
>
> Of the five cited passages, three survive word for word — `go/variant/Variant.scala:143`,
> `go/Board.scala:47`, `go/Chain.scala:97`. The `go/Replay.scala` pair was rewritten by `19524a4e`
> and `0ce6ed71`, but the replacement is the same kind of prose: `go/Replay.scala:166` still says
> "Preserved rather than fixed", and the settlement docstring above it now ends with "it is how
> `pgn.Reader` came to disagree with the rest of `Replay` about the same game" — a sentence about
> this branch's own defect history. `go/format/Forsyth.scala:191` carries a third "preserved rather
> than fixed". The comment at `go/Drop.scala:14-17` is unchanged and is design rationale rather
> than history, which the reviewer may have counted differently.

**R7 — A comment claims an invariant the code does not enforce.**
`go/Board.scala:66-67` states that every writer of `ko`, `consecutivePasses` and
`deadStonesSelected` goes through one of the three transitions and that the fields "are
never assigned from outside this file". They are: `Forsyth.<<@` sets all three through the
case-class constructor (`go/format/Forsyth.scala:49-56`), and `withKo`
(`go/Board.scala:83`) is a public fourth writer of `ko`, called from `Variant.boardAfter`
(`go/variant/Variant.scala:243`). A claim like this is worse than no comment, because it
invites a reader to reason from a guarantee that does not hold. Fix: soften the sentence to
what is true (the transitions are the *intended* path). Size: **one line.**

> **Since the review — R7 open, unchanged, and every leg of it re-verified.** The sentence still
> reads "…are never assigned from outside this file and cannot drift apart"
> (`src/main/scala/go/Board.scala:65-67`). `Forsyth.<<@` still sets all three through the
> constructor (`src/main/scala/go/format/Forsyth.scala:50-56`), and `withKo`
> (`src/main/scala/go/Board.scala:83`) is still public and still called from `Variant.boardAfter`
> (`src/main/scala/go/variant/Variant.scala:248`). The comment is still false.

**R8 — `Chain.requireVacant` uses `require`; the repo does not.**
`go/Chain.scala:56-57` is the only `require` or `assert` in `go`, `togyzkumalak`,
`backgammon`, `abalone` or `dameo` (verified by grep). The house marker for a genuinely
impossible state is `sys.error` — 12 sites in go alone, e.g. `go/Replay.scala:100,113,198`.
The distinction matters at the boundary: `require` throws `IllegalArgumentException`, and
the wrapper layer's error handling is written against the `RuntimeException` that `sys.error`
produces. Fix: **one line.**

> **Since the review — R8 open, unchanged.** `src/main/scala/go/Chain.scala:57` is still the only
> `require` or `assert` in `src/main/scala/go/`.

**R9 — `Variant.validMoves(situation) = None` and the commented-out `move` survive.**
`go/variant/Variant.scala:78`, complete with its inherited `// just remove this?`. No
callers (the only other mention is a commented-out line at `go/Situation.scala:11`), no
declared return type, and it returns `None.type` where every comparator returns
`Map[Pos, List[Move]]` (`togyzkumalak/variant/Variant.scala:145`,
`abalone/variant/Variant.scala:62`). The commented-out ten-line `move` implementation
immediately below (`go/variant/Variant.scala:165-174`) is the same. Both are pre-existing,
but they sit inside the block this refactor rewrote and are the two things in the file a
reader most obviously trips over. Fix: delete both, ~12 lines.

> **Since the review — R9 open, unchanged.** `validMoves` with its `// just remove this?` is at
> `src/main/scala/go/variant/Variant.scala:78`; the commented-out `move` is at `:170-179`.

**R10 — `FEN.invertPlayer` is now dead in `src/main`.**
`go/format/FEN.scala:44-47` was called only by the engine-era `Situation.unary_!` (dev
`go/Situation.scala:85-99`); its only remaining callers are tests
(`src/test/scala/go/GoFenTest.scala:19-115`). `fairysf/format/FEN.scala:17` and
`samurai/Situation.scala:65` still use theirs, so the method is not out of place in the
abstract — it is simply no longer wired to anything. Worth one deliberate decision (keep for
lila, or delete with its tests) rather than an oversight. Size: **trivial.**

> **Since the review — R10 open, unchanged.** `go/format/FEN.scala:44-47` still has no caller in
> `src/main`; the only hits are `src/test/scala/go/GoFenTest.scala`. `fairysf` and `samurai` still
> use theirs from `src/main`.

**R11 (minor) — `BoardSize.neighbours` is built by mutating an `Array` in place.**
`go/Board.scala:129-133` allocates with `Array.fill` and then assigns inside a `foreach`.
Array lookup tables have clear precedent (`draughts/Pos.scala:250`,
`draughts/variant/Variant.scala:518`, `go/Hash.scala`), and it is a `val` on a case object so
nothing escapes — but `Array.tabulate` expresses it without the mutation, and go's own
`Hash.bytesOf` (`go/Hash.scala:53`) already uses `Array.tabulate`. Size: **one line.**

> **Since the review — R11 open, unchanged.** `Array.fill` followed by an in-place `foreach`
> assignment is still at `src/main/scala/go/Board.scala:123-127`.

**R12 (minor) — `komi` is a `Double` that only ever holds tenths.**
`FEN.komi` divides an Int by `10.0` (`go/format/FEN.scala:59`), `Board.komi: Double` carries
it (`go/Board.scala:17`), and both `Variant.areaScore` (`go/variant/Variant.scala:306`) and
`Forsyth.komiTenths` (`go/format/Forsyth.scala:179`) `Math.round` it back into tenths. The
house keeps scores as `Int` (`strategygames.Score`). The `Double` is pre-existing at the FEN
boundary, but the refactor moved komi onto `Board` and could have moved it as tenths at the
same time. Fix touches the variant defaults and `setupInfo`'s `.replace(".0","")` — probably
not worth doing on its own, but worth knowing it is a float where the repo uses ints.

> **Since the review — R12 open, unchanged.** `FEN.komi` still divides by `10.0`
> (`go/format/FEN.scala:59`), `Board.komi` is still a `Double` (`go/Board.scala:17`), and both
> `Variant.areaScore` (`go/variant/Variant.scala:311`) and `Forsyth.komiTenths`
> (`go/format/Forsyth.scala:182`) still `Math.round` it back to tenths.

---

## Answer to the owner's question

Yes — with the residue list attached. The core of `strategygames.go` now reads as idiomatic
functional Scala that belongs in this repository: `variant/Variant.scala`, `Board.scala`,
`History.scala`, the three action types and `Replay.scala` follow togyzkumalak's skeleton
member for member, the legality path is `Validated[String, A]` throughout, absence is
`Option`, derived state is `lazy val`, configuration is `def`, and the only two `var`s in the
package are the same two the house keeps in the same function of every `Replay`. In two
places go is stricter than the house rather than looser — `Chain.grownFrom` is a `@tailrec`
recursion where `abalone/variant/Variant.scala` uses nested `while` loops over seven `var`s,
and go's FEN rank renderer is a `foldLeft` where togyzkumalak's and backgammon's are still
`StringBuilder` plus `var`. The engine is genuinely gone: `NextBoard`/`LazyBoardAfter`,
`Board.uciMoves`, `Board.position` and every `Api` reference are deleted, not renamed. Of the
five deviations you already knew about, all five hold up, though JD5 (the by-name history on
the shared façade) costs more than it looks: the `History.Go` arity change ripples to four
pattern matches in `Board.scala` and cost go the `implicit` conversion its eight siblings
still have. The residue is concentrated exactly where you predicted — `Situation.scala` and
`format/` — and it is mostly small: an un-memoised `dropsAsDrops`, an orphaned FEN validator,
a split-brained constants policy, a deleted `plyAtFen`, and comment prose that narrates the
refactor rather than the rules. **The single thing most worth fixing is `Forsyth.exportBoardFen`
(`src/main/scala/go/format/Forsyth.scala:124-145,185-186`): it is the last engine-era
signature in the package — a whole ten-field FEN, turn included, derived from a bare `Board`
where every other logic renders board-only state and lets `>>(game)` supply the player — and
it is the sole reason `Board.playerToMove`/`withPlayerToMove` exist and the sole reason go's
`Situation.unary_!` is the only one of nine that reaches into the board and nudges the game's
ply counter.**

> **Since the review.** The paragraph's closing recommendation was taken: `7c69c51d` made
> `exportBoard` board-only, deleted `Board.playerToMove` and `withPlayerToMove`, and returned
> `Situation.unary_!` to `copy(player = !player)`. One residue item of twelve is closed; the other
> eleven stand as written. The item-by-item status is in the blockquotes above.
