# Go speed dive: results

The go rules live as concrete `def`s on `go/variant/Variant.scala`, with connectivity in
`go/Chain.scala` and position state on `go/Board.scala`, where the area score is a derived
`lazy val` alongside `actors`, `posMap` and `piecesOnBoardCount`. The `Api.Position` seam and the
`go/engine/` package are gone, and there is one replay path.

Measured **2026-08-08** at `dd250768` with `GoRulesBenchmark` in `bench/`. These numbers replace both
the 2026-07-25 table (which measured the engine-backed seam) and an intermediate table taken earlier
today, before the area score moved off `go.History` onto `go.Board`.

**Two commits have landed since, and they move rows here.** `a3dc35a2` defers the wrapper's piece
map, which changes both `wrapperReplay` rows and the `wrapperReplay` allocation row; `c3ad23ac`
changes what `fenRenderMidGame` renders. Each affected row is marked, and the two sections at the
end say what a re-run has to account for. Every other row still stands.

## Protocol and machine

```
git worktree add /tmp/bench-run HEAD && cd /tmp/bench-run
sbt "bench/Jmh/run -wi 5 -w 2s -i 10 -r 2s -f 1 -to 120s -rf json -rff go-jmh-final.json \
  strategygames.bench.GoRulesBenchmark"
```

JMH 1.37, `avgt`, µs/op ± 99.9% CI. AMD Ryzen 9 8945HS (8 cores / 16 threads), 60 GB RAM, NixOS,
OpenJDK 21.0.12+2. Corpora: go9x9 120 turns, go13x13 200, go19x19 400 — generated on first use from
the fixed seed, not committed. All 30 configurations completed; error bars are under 2% on 24 of 30
rows and under 6% on all of them.

**The run was taken in a dedicated git worktree, and that is load-bearing.** Two earlier attempts in
the shared checkout were **discarded for contamination**, not lost: a concurrent `sbt` in the same
directory made JMH's forked JVMs fail with
`Could not find or load main class org.openjdk.jmh.runner.ForkedMain`, and JMH drops those
configurations from the console table *and* the `-rff` JSON while sbt still exits 0. Those runs
produced 27 and 22 of 30 rows respectively, with visibly inflated error bars on the survivors.
Anyone who finds their JSON should treat it as rejected. See `bench/README.md`, "Benchmark from a
separate worktree", for the trap and the verification commands.

**Cross-machine caveat, and it matters.** The joansala baseline (7,395 / 24,313 / 96,385 µs) and
every "removed" column below were measured on a *different* box — 32 cores, JDK 25.0.2, load average
5.6–7.5 — per the protocol line in `docs/go-engine-speed-dive.md`. joansala, the engine and the seam
are all deleted, so no same-run denominator can ever exist again. Every ratio crossing those runs is
indicative, not a controlled measurement.

## Full-game replay — two paths, and whether the score is read

These four numbers are the headline, and they are four rather than one because the paths no longer
cost the same thing. Say which one you are quoting.

| µs/op | go9x9 (120 plies) | go13x13 (200) | go19x19 (400) |
| --- | --- | --- | --- |
| `replay` — go package, score unread | 793.2 ± 6.0 | 1485.6 ± 10.4 | 3243.2 ± 156.3 |
| `replayReadingFinalScore` — go package, score read once | 860.3 ± 21.7 | 1532.1 ± 8.6 | 3441.6 ± 37.6 |
| **`wrapperReplay`** — wrapper, one `Game` per ply, score unread — *pre-`a3dc35a2`* | **1205.7 ± 8.3** | **2750.2 ± 85.3** | **8494.6 ± 477.8** |
| **`wrapperReplayReadingEveryScore`** — wrapper, score read every ply — *pre-`a3dc35a2`* | **3326.3 ± 87.5** | **10135.1 ± 216.1** | **39874.5 ± 1560.1** |

**lila runs go through the wrapper.** The two bold rows are therefore the production figures, and
the two go-package rows measure the rules on their own. Quote a wrapper number when the question is
what the site will feel, and a go-package number only when the question is what the rules cost.

**Both wrapper rows are stale, and by a large margin.** `a3dc35a2` landed after this run and took
the wrapper's `PieceMap` rebuild by name into a `lazy val`. It reports its own before/after pair at
19x19, measured together:

```
wrapperReplay                   8300.2 -> 4630.5 us/op   (1.79x)
wrapperReplayReadingEveryScore 38413.9 -> 34773.7 us/op
```

Its baselines are a different run from the table above, which is why they are not the table's 8494.6
and 39874.5 — take the 3.7 ms it removes from each row, not the absolute pair, as the comparable
quantity. The two go-package rows are untouched: `a3dc35a2` is one line in
`strategygames/Board.scala` and the go package never sees it. So the gap between the pairs is now
about 3.7 ms narrower at 19x19 than this table shows. **The table has not been re-run.**

Against the joansala baseline, with the same cross-machine caveat every ratio here carries, the
post-`a3dc35a2` production figures at 19x19 are about **21x joansala if nothing reads the score and
2.8x if something reads one every ply**, where the table below still says 11.3x and 2.4x.

**Which of the two wrapper numbers is live is not known here**, and the fix widened the question
rather than narrowing it: the spread was 4.6x on that run before `a3dc35a2` and is 7.5x after, since
removing a fixed per-ply cost shrinks the cheaper path proportionally more. It is decided entirely by
whether the caller touches a history field per ply. Nothing in this repo can answer which lila does;
the question has to go to the integration. Go is a scoring game and its score is normally on screen,
so the pessimistic figure may well be the live one — that is a reason to find out, not an answer.

- **`go.Replay.gameFromUciStrings`** returns one final `go.Game` and never scores unless something
  reads `Board.areaScore`. Reading it once at the end costs one flood fill: +67 / +47 / +198 µs.
- **`strategygames.Replay.gameWithUciWhileValid`** materialises one wrapper `Game` per ply. Since
  `1d658123` the wrapped history is by-name into a `lazy val`, and since `a3dc35a2` the wrapped piece
  map is too, so a consumer that reads neither pays for neither. Reading `history.score` on every ply
  cost **2.8x / 3.7x / 4.7x** on this run and **7.5x** at 19x19 after `a3dc35a2`, because `History.Go`
  takes the score as a strict parameter — so reading *any* history field forces that ply's flood
  fill.
- A consumer reading the score on the last ply only lands just above the `wrapperReplay` row. Which
  pattern lila actually uses is a question for the integration, not something measured here.

### Against the joansala baseline

| ratio vs joansala | go9x9 | go13x13 | go19x19 |
| --- | --- | --- | --- |
| `replay` | 9.3x faster | 16.4x faster | 29.7x faster |
| `replayReadingFinalScore` | 8.6x faster | 15.9x faster | 28.0x faster |
| **`wrapperReplay`** | **6.1x faster** | **8.8x faster** | **11.3x faster** |
| **`wrapperReplayReadingEveryScore`** | **2.2x faster** | **2.4x faster** | **2.4x faster** |

The bold pair is what production gets. Anyone quoting 29.7x as the speed-up go received is quoting
the rules in isolation. Both bold rows predate `a3dc35a2` and now understate the wrapper by about
3.7 ms per 19x19 game; see above.

### Against the removed engine, and against this branch before the score moved

| µs/op, 19x19 | value | vs `replay` |
| --- | --- | --- |
| engine batch path (removed) | 277.6 | 11.7x faster than now |
| engine per-ply path (removed) | 2127.3 | 1.5x faster than now |
| **this branch, score strict on `History`** | **30690.3** | **9.5x slower than now** |
| this branch now | 3243.2 | — |

Moving the area score onto `Board` as a `lazy val` bought **3.4x / 5.2x / 9.5x** on the go-package
path. Per ply, replay is now 6.6 / 7.4 / 8.1 µs — close to flat across board sizes, which is what it
should look like once the one O(board area) term per ply is gone.

## Single-shot costs (µs per call, mid-game position)

| µs/op | go9x9 | go13x13 | go19x19 | removed reference @19x19 |
| --- | --- | --- | --- | --- |
| `applyDrop` | 2.29 ± 0.02 | 2.28 ± 0.01 | **1.44 ± 0.02** | 60.9 joansala, ~6 seam |
| `validDropsMidGame` | 33.5 ± 0.3 | 97.9 ± 0.6 | 294.6 ± 3.3 | 13.9 seam (lazy), 794.0 seam (eager) |
| `dropsByRoleMidGame` | 33.2 ± 0.3 | 99.4 ± 0.5 | 296.8 ± 4.3 | — |
| `areaScoreMidGame` | 11.0 ± 0.1 | 27.2 ± 0.8 | 62.9 ± 0.8 | 4.3 engine |
| `fenParseMidGame` | 28.2 ± 0.1 | 49.8 ± 0.4 | 103.8 ± 5.6 | 5.9 engine |
| `fenRenderMidGame` — *rendered a ten-field FEN; see below* | 13.6 ± 0.1 | 32.6 ± 0.3 | 80.4 ± 1.1 | 7.8 engine |

`applyDrop` = `Variant.boardAfter`, which no longer scores: **1.44 µs at 19x19 against 59.9 µs
before the change**, a 42x drop, and 42x faster than joansala's own 60.9 µs. It is now cheaper at
19x19 than at 9x9 because the cost is the placed stone's chain walk, not the board.

`validDropsMidGame` is the one workload the change did not touch — it never scored. It costs 17–21x
what the seam's lazy list cost, and 1.2–2.7x less than the seam's eager one.

## Allocation per full-game replay

`-prof gc`, `-wi 3 -i 5`, same worktree.

| B/op | go9x9 | go13x13 | go19x19 |
| --- | --- | --- | --- |
| `replay` | 2,304,140 | 4,112,378 | 9,758,786 |
| `wrapperReplay` — *pre-`a3dc35a2`* | 5,317,980 | 12,836,084 | 44,432,706 |
| `replay`, score strict (this branch, earlier) | 6,701,569 | 20,722,629 | 82,142,796 |
| engine batch path (removed) | 174,249 | 282,548 | 566,081 |

The score change cut go-package replay allocation **2.9x / 5.0x / 8.4x**. The `wrapperReplay` row
predates `a3dc35a2` and is the one row here that moved with it — a deferred `PieceMap` is a wrapper
map per ply not allocated. It has not been re-measured, so no post-fix figure is quoted.

## Where the time goes now

`areaScore` has dropped out of the go-package replay profile completely. `-prof stack:lines=4` on
`replay` at 19x19, named RUNNABLE samples:

| share | frame |
| --- | --- |
| 8.0% | `List.filter` in `Chain.Stones.grownFrom` ← `regionFrom` ← `chainAt` |
| 5.2% | `Statics.anyHash` → `HashSet.contains` — the `reached` set test inside `grownFrom` |
| 4.4% | `List.length` / `SeqOps.size` |
| 3.9% | `IterableOnceOps.exists` → **`History.hasOccurred`** — the superko scan |
| 3.5% | `BoxesRunTime.equals2` → `BitmapIndexedMapNode.get` — `PieceMap` lookups |
| 3.3% | `HashSet.concat` / `BitmapIndexedSetNode.updated` — region accumulation |
| 1.7% | `Variant.boardAfter` via `Drop.after`'s `lazy val` |

The go-package path is now **chain flood fill plus `Set[Pos]`/`Map[Pos, Piece]` hashing**, with
`History.hasOccurred`'s O(plies)-per-ply superko scan newly visible at ~4% — it was always there,
but `areaScore` dwarfed it.

The wrapper path's overhead over the go-package path was 3.4 / 6.3 / 13.1 µs per ply, scaling with
board area, and the profile named one cause:

| share | frame |
| --- | --- |
| 13.1% | `HashMap.map` ← `Board$.Go$superArg$1` ← **`Board$Go.<init>`** |
| 9.6% | `HashMapBuilder.update` ← the same `map` |
| 1.9% + 1.4% + 1.2% | more builder and iterator frames under the same `map` |

That was one line: `strategygames.Board.Go` rebuilds the whole `PieceMap` as
`b.pieces.map { case (pos, piece) => (Pos.Go(pos), (Piece.Go(piece), 1)) }`, and `pieces` was a
**strict** constructor argument, so every wrapper `Board` — one per ply — paid a full board scan.
~27% of named wrapper samples sat under it. **`a3dc35a2` closed this**, deferring `pieces` by name
into a `lazy val` exactly as `1d658123` had done for the wrapped history; this profile and the
per-ply overhead beside it both predate it.

## The wrapper path, in the order it was taken

Both items are on the wrapper path, which is why they lead: that is the path lila runs. Neither was
actionable before the owner confirmed the path, because nobody knew which numbers described
production.

**1. `strategygames.Board` rebuilding the whole `PieceMap` per construction — closed by
`a3dc35a2`.** It was a strict constructor argument, so every `Board.Go(b)` scanned the go piece map
into wrapper `Pos` and `Piece` whether or not anything read it, and `gameWithUciWhileValid`
materialises one wrapper `Board` per ply. It was ~27% of named samples in `wrapperReplay` at 19x19,
with per-ply wrapper overhead scaling with board area (3.4 / 6.3 / 13.1 µs). Taking `pieces` by name
into a `lazy val` — the shape `1d658123` had already used for the wrapped history on the same class —
removed 3.7 ms from a 19x19 wrapper replay, 1.79x on the row that reads no score. All nine logics
gained it.

**2. `History.Go` holding the area score strictly** — still open. Reading *any* history field on a
wrapper board forces that ply's flood fill, which is the 7.5x between the two wrapper rows. Closing
it needs a by-name parameter, costing `History.Go` its `case class`, or a board-carrying `History.Go`
that lila's `History.apply` factory cannot construct; `docs/go-refactor.md` records what each costs.
The `pieces` deferral was cheap because `Board` is a `sealed abstract class` rather than a `case
class`, so a by-name parameter cost no subclass anything. `History.Go` does not have that out.

## What a re-run needs to know

`c3ad23ac` changed what `fenRenderMidGame` measures. It rendered a whole ten-field FEN through
`exportBoardFen` when the table above was taken; it now renders the eight-field board-only string
through `exportBoard`. The row is not comparable across that commit. `GoMidGame.fenOf` moved with it,
from `exportBoardFen(board)` to `Forsyth.>>(game)`, which is byte-identical, so the mid-game fixture
is unchanged and every other row still measures what it measured.

The same commit is why the go benchmarks could not have been re-run in between: `b6ba4a67` left
`GoRulesBenchmark` uncompilable, and `bench` is not built by `sbt test` at the root, so nothing
caught it.

## What this is not

This is the idiomatic branch, so most of the above is a reported finding and an input to the
follow-up performance branch rather than something acted on. Three changes did land, each because it
was a misreading of the house idiom rather than a considered trade:

- **`History.score` strict → `Board.areaScore` derived.** Togyzkumalak's strict `history.score` is an
  accumulated capture count, essentially free, whereas go's is a flood fill, and the house pattern
  for derived state is a `lazy val` on `Board`.
- **`1d658123`, the wrapped history by name**, and **`a3dc35a2`, the wrapped piece map by name.** Both
  were strict constructor arguments building a wrapper view nothing on the replay path reads. Both
  are one line, and both apply to all nine logics.

Everything else here is untouched, including the remaining item the profile names:
`History.Go`'s strict score.
