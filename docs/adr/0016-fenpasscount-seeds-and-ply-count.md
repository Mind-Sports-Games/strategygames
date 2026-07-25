# 0016 — Resumes from a fenPassCount FEN count only the actions given; seeds are never plies

**Status:** Accepted (2026-07-25)

## Context

A go game resumed from a FEN whose `passCount` field is 1–3 carries its pass state twice:

- The engine parses it directly: `GoFen.parse` sets `consecutivePasses` (clamped to 0–2),
  `deadStonesSelected` (`passCount >= 3`) and `plyCount` (from the full-move field) — the
  raw FEN states the real pass count by 0009.
- The wrapper seeds it symbolically: `Forsyth.<<@` puts synthetic entries on
  `board.uciMoves` (`List("pass")`, `List("pass", "pass")`, `List("ss:")`), because three
  wrapper readers derive pass state from the uciMoves tail rather than the engine:
  `Situation.canSelectSquares` / `isSubsequentPassWarning`, `Forsyth.exportBoardFen`'s
  passCount field, and `Board.apiPosition`'s position-less fallback.

Until the E1 wrapper fixes, production's two action paths disagreed about what to do with
that duplication. The drop path (`Board.afterDrop`) played one action onto the position in
hand. The pass/`ss:` path (`Replay.gameWithActionWhileValid`'s old `getApiPosition`)
rebuilt the position as `positionFromVariantStartingFenAndMoves(variant, resumeFen,
uciMoves)` — parsing the pass state out of the FEN *and then replaying the seeds on top of
it*. Every seeded entry became an extra engine ply.

Reproduced against the current engine (9x9, both conventions still reachable through
`Api`; A = one action on the parsed position, B = seeds re-replayed):

| resume FEN (tail) | new action | A raw FEN tail | B raw FEN tail |
|---|---|---|---|
| `b … 55 2 2` | `ss:` | `b … 55 3 2` | `b … 55 3 3` — full-move inflated |
| `w … 55 1 1` | `pass` | `b … 55 2 2` | `w … 55 2 2` — turn a parity off |

So under the old pass path a resumed-mid-pass game rendered a FEN whose full-move field —
and, with an odd seed count, whose turn field — depended on which *kind* of action came
last. The double-counted `consecutivePasses` (parsed 1 + seed 1 + played 1 = 3 where 2 is
true) stayed invisible only because every reader clamps: `GoGame.fenPassCount` is
`min(_, 2)`, `inDeadStoneSelectionPhase` is `>= 2`.

Meanwhile the wrapper's own `Game.plies` always counted actions-given: parsed from the
FEN's full-move via `Forsyth.<<<@`, incremented once per applied action. The old pass path
put the engine at odds with the wrapper it was feeding.

E1 already moved the per-ply pass path to one-action-on-the-position-in-hand. The E4/E2
batch path plays exactly the actions it is given. This ADR fixes which convention is the
contract both must satisfy.

## Decision

**Actions-given semantics.** For a game resumed from a FEN:

- The resume FEN's own fields are the sole authority for everything before the resume
  point — pass state, turn, and ply count enter the engine once, through `GoFen.parse`.
- `GoGame.plyCount`, `Game.plies`/`turnCount`, the engine turn, and therefore the rendered
  FEN's full-move and turn fields advance only for actions actually applied after the
  resume point.
- The seeded `board.uciMoves` entries are wrapper bookkeeping only. They continue to ride
  `board.uciMoves` so `canSelectSquares`, `exportBoardFen` and the position-less fallback
  see pass state, but they are never handed to the engine as moves and never counted as
  plies — by any path.
- `consecutivePasses` is single-counted: the parsed count plus passes actually played.

## Alternatives

- **Seeds are plies** (the old pass-path convention) — counts the same FEN information
  twice, disagrees with the drop path, with `Game.plies`, and with itself (turn parity off
  by the seed count). Making it *consistent* would mean adding the inflation to the drop
  path and the wrapper to match a bug. Rejected; E1's analysis already recorded the
  single-counted behaviour as the more correct of the two.
- **Drop the seeds entirely and derive wrapper pass state from the engine** — cleaner, but
  it rewires three readers and is not needed to make the paths agree. Out of scope here;
  nothing in this decision forecloses it later.

## Consequences

- Every game whose FEN carries `passCount` 0 — the variant initial FENs, all three
  corpora, every in-repo fixture — is untouched, byte-for-byte.
- A resumed-mid-pass game now renders the same FEN whichever action type came last. FENs
  the old pass path rendered from such games can differ (full-move, and turn on odd seed
  counts); this is treated as a bug fix, not a migration — go has no FromPosition support,
  and no in-repo fixture or corpus carries `passCount` 1–3.
- Single-counted `consecutivePasses` is now the contract: resume from `passCount 1` plus
  one played pass gives `consecutivePasses` 2 and `canSelectSquares` — the clamps in
  `GoGame.fenPassCount` and `inDeadStoneSelectionPhase` are thresholds again, not
  camouflage for a double count.
- `passCount 3` parses to an ended game (`deadStonesSelected`), and 0013 forbids offering
  it further actions — the `"ss:"` seed exists only for the wrapper readers, and no path
  may replay it.
- What the batch-path orders must enforce: the batch entry point receives only the real
  actions, never the seeds; and the per-ply/batch equivalence tests must include resumed
  FENs with `passCount` 1 and 2 followed by a pass, by a drop, and by an `ss:`, asserting
  both paths agree on `Game.plies`/`turnCount`, the full rendered wrapper FEN (turn,
  passCount, full-move), `canSelectSquares`, and the raw engine FEN — the drop case pinned
  absolutely at `fenPassCount == 0`, since a placement resets a seeded pass count. The
  corpora alone cannot catch a regression here — they all start at `passCount` 0.
