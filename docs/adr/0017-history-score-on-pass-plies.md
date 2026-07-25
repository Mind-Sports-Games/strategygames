# 0017 — `history.score` is not rescored on pass plies

**Status:** Accepted (2026-07-25)

## Context

`go.Replay.replayPass` carries `history.scoring` forward unchanged; only the drop path
(`Board.afterDrop`) and the replay `ss:` path (`Replay.replaySelectSquares`) write it. The
interactive pass path (`variant.Variant.validPass`) behaves the same way: `board.copy` on a
pass preserves the existing `History`. A pass-terminated game therefore carries the scoring
thunk installed at the last drop. RESULTS-E4 recorded this as a quirk reproduced
"bug-for-bug"; RESULTS-E1 changed the mechanism (`scoring: () => Score` thunk, evaluated
lazily) without changing the pass-ply behaviour. The question: is the carried-forward score
wrong, and must the batch replay path (orders 06–08) rescore on passes?

### Invariance analysis

The carried-forward score is not stale — it is provably the score of the final position:

- `GoState.afterPass` (`go/engine/GoState.scala`) constructs the successor state passing
  the **same `board` array reference** — no clone, no stone change. Only `playerTurn`,
  `consecutivePasses`, `simpleKoMove`, and `capturedMovesOnLastPlacement` change.
- `GoState.areaScore` reads only `board` (flood fill over stones; ADR 0006, 0008).
- Komi enters via `GoGame.p2FenScore = areaScore.white * 10 + komiTenths`; `GoGame.play`
  never changes `komi`.

Hence for every state `s`: `areaScore(afterPass(s)) == areaScore(s)` — by reference
identity of the board array, not merely by value — and `fenScore` is invariant under any
number of passes. Whenever at least one drop (or replay `ss:`) precedes the passes, the
thunk installed there evaluates to exactly the final position's score. "Keep the last
score" and "rescore at the end" are the same function on that domain.

The two coincide everywhere except the **zero-drop seed case**: a game with no drop since
construction (all-pass game, or a FEN resume followed only by passes — `Forsyth.<<@` builds
`History` with the default `History.unscored`) reports `Score(0, 0)`, whereas a fresh
rescore would report at least `(0, komiTenths)` plus any stones on a resumed board.

### Consumers

- **The wrapper FEN never reads `history.score`.** `Forsyth.exportBoardFen` takes the
  engine FEN (`GoFen.render`, fields 3–4 from the current `GoGame`'s `areaScore` + komi)
  and patches only turn, captures, pass count, and full-move. FEN score fields are
  position-fresh on every ply, including passes and dead-stone selection.
- **Nothing in `go/` reads `history.score`.** `Variant.winner` / `specialDraw` use
  `apiPosition.fenScore` directly.
- **The only reader is the wrapper** `strategygames.History.Go`, which surfaces
  `score = h.score` (forcing the E1 thunk once) for lila.
- Retention (E1 interaction): after passes, `history.scoring` still closes over the last
  drop's `ScalaPosition` while `board.position` has advanced. Cheap by construction —
  `afterPass` shares every array with its predecessor.

Related, out of scope here: the interactive `ss:` path (`createSelectSquares` +
`SelectSquares.finalizeAfter`) refreshes neither `scoring` nor `captures` nor `pieces`,
while the replay `ss:` path refreshes all three (with the spurious captures `+1` recorded at
`Replay.settlementCaptureCount`). After an `ss:` that removes stones the score genuinely changes, and
only the replay path records it in `History`; the FEN stays correct on both paths because
its score fields are position-derived.

## Decision

Pass plies do not rescore, and this is correct by construction — not a preserved bug. The
per-ply path keeps carrying the thunk forward unchanged (`replayPass` and `validPass` stay
as they are), and the batch replay path reproduces the same observable value: evaluate the
score once from the last scoring-bearing state (last drop or replay `ss:`), or preserve
`History.unscored` (`Score(0, 0)`) when no such action exists in the replayed sequence.

Because of the invariance theorem, a batch implementation may equivalently evaluate from
the final post-pass state whenever a drop or `ss:` precedes the passes; the zero-drop seed
case is the only place the choice is observable, and there `unscored` must be preserved.

## Alternatives

- **B — rescore on (game-ending) passes.** Rejected: for every game containing a drop it is
  a provable no-op bought with code churn in two production paths; its only observable
  effect is changing the zero-drop seed case from `Score(0, 0)` to a komi-bearing value,
  which shifts wrapper-visible values and golden expectations for no consumer that needs
  it — the FEN, the winner logic, and the dead-stone-selection flow all read the position,
  not `history.score`.

## Consequences

- Orders 06–08 (batch engine core, seam entry, wrapper default) implement: batch
  `history.scoring = () => fenScore(state after the last drop/ss:)`, or `History.unscored`
  if none. E4's "reference to the last scoring `GoGame`" trick is the blessed shape.
- Equivalence tests must assert, on pass-terminated games, `history.score` equal between
  batch and per-ply paths, covering (i) a game whose last drop precedes several passes,
  (ii) a zero-drop all-pass game asserting `Score(0, 0)`, and may additionally assert the
  theorem itself: `history.score == apiPosition.fenScore` whenever a drop occurred.
- Adding `scoring = ...` to `replayPass` or `validPass` is foreclosed unless this ADR is
  superseded.
- The zero-drop seed value `Score(0, 0)` is now a recorded contract, not an accident; a
  future decision to score all-pass games must supersede this ADR and migrate downstream
  expectations.
- The invariance argument is pinned here so it is not re-derived per order: pass shares the
  board array; `areaScore` is a function of the board array; komi is constant per game.
