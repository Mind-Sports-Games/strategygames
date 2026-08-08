# 0003 — Go's rules live on `Variant`, like every other game logic

**Status:** Accepted (2026-08-08). Supersedes the structural half of
[0001](0001-pure-scala-go-engine.md) and the whole of [0002](0002-go-batch-replay.md), carrying
one ruling forward from each.

## Context

Go was the only pure-Scala game logic in this repo shaped like a foreign-engine binding. Its rules
lived below an `Api.Position` seam backed by a six-file `go/engine/` package, and `go.Board` carried
an engine object plus a replay log instead of the position itself. Nothing forced that shape —
[0001](0001-pure-scala-go-engine.md) inherited it from joansala, which really was foreign, and kept
it because the seam already existed and every consumer already sat behind it.

The cost was structural rather than aesthetic. State that the wrapper types owned in every other
logic was reachable two ways, one authoritative and one stale; rules were implemented once per
entry point rather than once; and a new maintainer had to learn a private engine dialect before
they could read a rule. The refactor found four separate instances of the same defect shape —
several entry points for one behaviour, one of them updated — which is a count high enough to be a
property of the shape rather than of anyone's carelessness.

## Decision

**Rules are concrete `def`s on `go/variant/Variant.scala`.** `boardAfter`, `validDrops`, `canDrop`,
`drop`, `boardAfterPass`, `boardAfterSelectSquares`, `winner`, `specialEnd`, `specialDraw`,
`areaScore`, `valid` — the same vocabulary `togyzkumalak` and `backgammon` use, in the same file
position. The three go variants override board size, komi and handicap layout and nothing else.

**One implementation of capture and legality, in `go/Chain.scala`.** `Chain` owns connectivity:
chains, liberties, and what a placement captures. `capturesUnlessSuicide` answers capture and
suicide together, returning `Option[Set[Pos]]`, so "captures something and is also suicide" cannot
be spelled. One flood fill (`Stones.grownFrom`) serves both chain-finding and empty-region-finding.

**Position state is ordinary fields on `go/Board.scala`** — `pieces`, `ko`, `consecutivePasses`,
`deadStonesSelected`, `komi` — written only through named transitions (`passed`, `stonePlaced`,
`settled`, `withKo`), after the precedent `backgammon.Board` sets. Komi is per game, not per
variant, because it arrives in the FEN.

**Superko history is `History.positionHashes`**, the house field every logic already carries for
repetition. Positional superko, probed only on capturing placements. `Board.settled` restarts it,
which is the only truncation go performs.

**The area score is derived on `Board`**, as `lazy val areaScore: Score = variant.areaScore(this)`,
sitting with the other derived state (`actors`, `posMap`, `playerPiecesOnBoardCount`). The rule
stays on the variant, the memo on the board — the same split as `materialImbalance`.
`History.captures` stays on `History` because it is genuinely accumulated and no single position can
reconstruct it.

**The `Api.Position` seam and `go/engine/` are deleted**, along with the second replay path. 4,616
lines removed.

### Carried forward from the superseded ADRs

- From **0001**, unchanged and still binding: positional rather than situational superko; Chinese
  area scoring; the FEN dialect including the live ko coordinate; rules-correct over bug-for-bug
  parity with joansala, with the divergence table in
  [docs/go-engine.md](../go-engine.md) as the audit checklist; and the retirement of go variant ids
  3, 5, 6, 7 and perfIds 503/504/505. What 0001 no longer describes is the *implementation*: the
  byte array, the union-find chains, the incremental engine hash and the seam are gone.
- From **0002**, unchanged: **a game resumed from a FEN counts only the actions it is handed.** The
  FEN's own fields are the sole authority for everything before the resume point.
- From **0002**, strengthened: **a pass cannot change the score.** Under a derived score this holds
  structurally rather than by scheduling discipline — a passed board carries the same stones and the
  same komi, so it computes the same number. There is no longer a pass path that scores, so there is
  nothing left to accidentally add rescoring to.
- From **0002**, **superseded on evidence: a drop-less game no longer keeps `Score(0, 0)`.** That
  clause described an evaluation schedule ("scoring evaluates once from the last scoring-bearing
  state") and what it produced before any such state existed — not a fact about go. It was already
  contradicted inside the tree: the exported FEN reads the position, so at exactly those plies the
  FEN said `0 55` while `history.score` said `0 0`. Measured scope of the change: **91 of 16,207
  recorded plies**, fields 5 and 6 only, every one moving from `0|0` to that ply's own FEN score;
  85 of them are ply 0, and the other 6 are the drop-less prefixes of the two games curated for the
  clause. No FEN, digest, drop, capture, end or winner value moves anywhere. Note `Score(0, 0)` was
  never "no score" in any case — a handicap game has stones and a real area score from ply 0.

## Alternatives

- **Keep the seam, tidy behind it.** Rejected: the seam is what makes state reachable two ways and
  invites a second implementation per entry point. Tidying below it would have left all four
  instances of the defect shape possible.
- **Keep the engine's representation, move only the API.** Rejected: a `Board` holding a byte array
  is not readable as a go position, and the wrapper layer would still need a projection, which is
  where the stale `pieces` came from.
- **A `Score` field on `History`, kept strict.** Tried, and the first version of this refactor
  shipped it — it is the shape `togyzkumalak` uses. Rejected on measurement: togyzkumalak's score is
  accumulated captures and go's is a full-board flood fill, so copying the shape without the
  reasoning cost a flood fill per placement whether or not anyone wanted the number. That is
  roughly half to four-fifths of a full-game replay.
- **Situational superko.** Rejected in 0001 and still rejected; positional forbids a strict superset
  of cycles and pairs correctly with area scoring.

## Consequences

- One place to read a go rule, and one place to change it. `go/Replay.scala` is 342 lines against
  togyzkumalak's 325, down from 572.
- **Two behaviour changes**, both forced and both stated in
  [docs/go-refactor.md](../go-refactor.md) with their evidence: a live settlement now refreshes
  `board.pieces` (it did not, and the stale board mis-scored a decided game
  `Score(450,65)` where the truth is `Score(810,55)`), and four passes now end the game the same way
  whether it is played or replayed (they did not, and the two paths returned different games).
- **Source-breaking for lila**, in two places beyond the deleted `Api` helpers:
  `FEN.variant` is `Option[Variant]`, and the `goHistory` implicit is gone. The full patch is in
  [docs/go-refactor.md](../go-refactor.md).
- **Slower than the seam it replaced** — 37–111x on replay, 18–20x on movegen — and roughly 3x
  faster than joansala. Tables in [docs/go-speed-results.md](../go-speed-results.md). Every number
  is reported, none was traded for; a follow-up branch takes the measurements as its input.
- The four preserved quirks stay preserved, each documented at its code site. They are behaviour
  stored games were written under, not decisions this ADR reopens.
