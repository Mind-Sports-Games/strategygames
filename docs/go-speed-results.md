# Go speed dive: results

The go rules live as concrete `def`s on `go/variant/Variant.scala`, with connectivity in
`go/Chain.scala` and position state on `go/Board.scala`, where the area score is a derived
`lazy val` alongside `actors`, `posMap` and `piecesOnBoardCount`. The `Api.Position` seam and the
`go/engine/` package are gone, and there is one replay path.

Measured **2026-08-08** at `d714b3b9` with `GoRulesBenchmark` in `bench/`. These numbers replace both
the 2026-07-25 table (which measured the engine-backed seam) and an intermediate table taken earlier
today, before the area score moved off `go.History` onto `go.Board`.

**Two commits have landed since, and they move rows here.** `879d0977` defers the wrapper's piece
map, which changes both `wrapperReplay` rows and the `wrapperReplay` allocation row; `8904ce51`
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
| **`wrapperReplay`** — wrapper, one `Game` per ply, score unread — *pre-`879d0977`* | **1205.7 ± 8.3** | **2750.2 ± 85.3** | **8494.6 ± 477.8** |
| **`wrapperReplayReadingEveryScore`** — wrapper, score read every ply — *pre-`879d0977`* | **3326.3 ± 87.5** | **10135.1 ± 216.1** | **39874.5 ± 1560.1** |

**lila runs go through the wrapper.** The two bold rows are therefore the production figures, and
the two go-package rows measure the rules on their own. Quote a wrapper number when the question is
what the site will feel, and a go-package number only when the question is what the rules cost.

**Both wrapper rows are stale, and by a large margin.** `879d0977` landed after this run and took
the wrapper's `PieceMap` rebuild by name into a `lazy val`. It carries its own before/after run: two
`git worktree` checkouts at `8904ce51` and `879d0977`, differing by `Board.scala` alone, same
protocol as above, 6/6 results and no `ForkedMain` in either log.

| µs/op | go9x9 | go13x13 | go19x19 |
| --- | --- | --- | --- |
| `wrapperReplay` before | 1216.9 ± 9.0 | 2789.5 ± 42.8 | 8300.2 ± 60.8 |
| `wrapperReplay` after | 830.9 ± 7.2 | 1509.0 ± 16.0 | **3205.6 ± 25.3** |
| | 1.46x | 1.85x | **2.59x** |
| `wrapperReplayReadingEveryScore` before | 3124.8 ± 35.6 | 9582.7 ± 74.6 | 38413.9 ± 852.3 |
| `wrapperReplayReadingEveryScore` after | 2750.5 ± 81.1 | 8302.8 ± 26.2 | **32947.3 ± 335.8** |
| | 1.14x | 1.15x | **1.17x** |

That is a different run from the table above, which is why its baselines are 8300.2 and 38413.9
rather than the table's 8494.6 and 39874.5 — within a few percent, so the two runs are comparable,
but no row here belongs in that table. At 19x19 the change removes 5.1 ms from `wrapperReplay` and
5.5 ms from `wrapperReplayReadingEveryScore`: the same per-ply map rebuild, gone from both. The two
go-package rows are untouched — `879d0977` is three lines in `strategygames/Board.scala` and the go
package never sees it. **The table has not been re-run.**

**The wrapper's overhead on a replay that reads no score is now indistinguishable from zero.** At
19x19 `wrapperReplay` is 3205.6 ± 25.3 against the go package's own `replay` at 3243.2 ± 156.3 — the
wrapper row is nominally the lower of the two and the intervals overlap. At 9x9 and 13x13 the
wrapper costs 0.31 and 0.12 µs per ply more than the go package. The 5.25 ms this document
attributed to wrapper overhead at 19x19 (8494.6 against 3243.2) was almost entirely this one map
rebuild. That comparison crosses two runs, and its whole warrant is that the paired run's baselines
reproduce this table's within a few percent.

Against the joansala baseline, with the same cross-machine caveat every ratio here carries, the
post-`879d0977` production figures at 19x19 are **30.1x joansala if nothing reads the score and 2.9x
if something reads one every ply** (96385/3205.6 and 96385/32947.3), where the table below still says
11.3x and 2.4x.

**Which of the two wrapper numbers is live is not known here**, and the fix widened the question
rather than narrowing it: on that paired run the spread between the two rows was 4.6x before
`879d0977` and is **10.3x** after, since removing a fixed per-ply cost shrinks the cheaper path
proportionally more. It is decided entirely by whether the caller touches a history field per ply.
Nothing in this repo can answer which lila does; the question has to go to the integration. Go is a
scoring game and its score is normally on screen, so the pessimistic figure may well be the live one
— that is a reason to find out, not an answer.

- **`go.Replay.gameFromUciStrings`** returns one final `go.Game` and never scores unless something
  reads `Board.areaScore`. Reading it once at the end costs one flood fill: +67 / +47 / +198 µs.
- **`strategygames.Replay.gameWithUciWhileValid`** materialises one wrapper `Game` per ply. Since
  `5ed78cb1` the wrapped history is by-name into a `lazy val`, and since `879d0977` the wrapped piece
  map is too, so a consumer that reads neither pays for neither. Reading `history.score` on every ply
  cost **2.8x / 3.7x / 4.7x** on this run and **3.3x / 5.5x / 10.3x** on the paired run after
  `879d0977`, because `History.Go` takes the score as a strict parameter — so reading *any* history
  field forces that ply's flood fill.
- A consumer reading the score on the last ply only lands just above the `wrapperReplay` row. Which
  pattern lila actually uses is a question for the integration, not something measured here.

### Against the joansala baseline

| ratio vs joansala | go9x9 | go13x13 | go19x19 |
| --- | --- | --- | --- |
| `replay` | 9.3x faster | 16.4x faster | 29.7x faster |
| `replayReadingFinalScore` | 8.6x faster | 15.9x faster | 28.0x faster |
| `wrapperReplay` — *pre-`879d0977`* | 6.1x faster | 8.8x faster | 11.3x faster |
| `wrapperReplayReadingEveryScore` — *pre-`879d0977`* | 2.2x faster | 2.4x faster | 2.4x faster |
| **`wrapperReplay`** — *post-`879d0977`* | **8.9x faster** | **16.1x faster** | **30.1x faster** |
| **`wrapperReplayReadingEveryScore`** — *post-`879d0977`* | **2.7x faster** | **2.9x faster** | **2.9x faster** |

The bold pair is what production gets, and both rows have to be stated: which one describes lila
depends on whether it reads a history field per ply. They are arithmetic over the paired run above
rather than rows of this table, so they carry a second caveat on top of the cross-machine one — the
numerator and denominator come from different runs as well as different boxes.

Anyone quoting 29.7x as the speed-up go received is quoting the rules in isolation, though after
`879d0977` the score-unread wrapper row reaches 30.1x, level with the go-package row, because the
wrapper no longer costs anything measurable on that path.

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
| `wrapperReplay` — *pre-`879d0977`* | 5,317,980 | 12,836,084 | 44,432,706 |
| `replay`, score strict (this branch, earlier) | 6,701,569 | 20,722,629 | 82,142,796 |
| engine batch path (removed) | 174,249 | 282,548 | 566,081 |

The score change cut go-package replay allocation **2.9x / 5.0x / 8.4x**. The `wrapperReplay` row
predates `879d0977` and is the one row here that moved with it — a deferred `PieceMap` is a wrapper
map per ply not allocated. `879d0977`'s paired run measured time only, with no `-prof gc`, so no
post-fix allocation figure exists to quote.

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
~27% of named wrapper samples sat under it. **`879d0977` closed this**, deferring `pieces` by name
into a `lazy val` exactly as `5ed78cb1` had done for the wrapped history; this profile and the
per-ply overhead beside it both predate it, and the overhead it names has since gone to
approximately zero.

## The wrapper path, in the order it was taken

Both items are on the wrapper path, which is why they lead: that is the path lila runs. Neither was
actionable before the owner confirmed the path, because nobody knew which numbers described
production.

**1. `strategygames.Board` rebuilding the whole `PieceMap` per construction — closed by
`879d0977`.** It was a strict constructor argument, so every `Board.Go(b)` scanned the go piece map
into wrapper `Pos` and `Piece` whether or not anything read it, and `gameWithUciWhileValid`
materialises one wrapper `Board` per ply. It was ~27% of named samples in `wrapperReplay` at 19x19,
with per-ply wrapper overhead scaling with board area (3.4 / 6.3 / 13.1 µs). Taking `pieces` by name
into a `lazy val` — the shape `5ed78cb1` had already used for the wrapped history on the same class —
removed 5.1 ms from a 19x19 wrapper replay, 2.59x on the row that reads no score, and took the
wrapper's overhead on that row to within noise of the go package. All nine logics gained it.

**2. `History.Go` holding the area score strictly** — the one item still open, and the whole of what
is left. Reading *any* history field on a wrapper board forces that ply's flood fill, which is the
10.3x between the two wrapper rows: with the piece map deferred, nothing else measurable separates
them. Closing it needs a by-name parameter, costing `History.Go` its `case class`, or a
board-carrying `History.Go` that lila's `History.apply` factory cannot construct;
`docs/go-refactor.md` records what each costs. The `pieces` deferral was cheap because `Board` is a
`sealed abstract class` rather than a `case class`, so a by-name parameter cost no subclass anything.
`History.Go` does not have that out.

## What a re-run needs to know

`8904ce51` changed what `fenRenderMidGame` measures. It rendered a whole ten-field FEN through
`exportBoardFen` when the table above was taken; it now renders the eight-field board-only string
through `exportBoard`. The row is not comparable across that commit. `GoMidGame.fenOf` moved with it,
from `exportBoardFen(board)` to `Forsyth.>>(game)`, which is byte-identical, so the mid-game fixture
is unchanged and every other row still measures what it measured.

The same commit is why the go benchmarks could not have been re-run in between. `7c69c51d` renamed
`Forsyth.exportBoardFen` to `exportBoard` and narrowed its return type, leaving two call sites in
`GoRulesBenchmark` on the old name; `bench` is aggregated by no root task, so `sbt test` never
compiled it and nothing failed. Every go benchmark on this branch was unrunnable from `7c69c51d`
until `8904ce51` fixed it. `bench/README.md` carries the standing hazard; a branch that lives in
`bench` should compile it explicitly before trusting that a benchmark exists to run.

One figure in this document was written before it was measured — `879d0977`'s "after" pair, invented
and then corrected by its own author's re-check, not by review, and re-instated once by a rebase that
dropped the correcting amend. On a branch whose output is almost entirely measurement, the only thing
that catches an unmeasured number is tracing it to a JMH JSON before it is written down.

## What this is not

This is the idiomatic branch, so most of the above is a reported finding and an input to the
follow-up performance branch rather than something acted on. Three changes did land, each because it
was a misreading of the house idiom rather than a considered trade:

- **`History.score` strict → `Board.areaScore` derived.** Togyzkumalak's strict `history.score` is an
  accumulated capture count, essentially free, whereas go's is a flood fill, and the house pattern
  for derived state is a `lazy val` on `Board`.
- **`5ed78cb1`, the wrapped history by name**, and **`879d0977`, the wrapped piece map by name.** Both
  were strict constructor arguments building a wrapper view nothing on the replay path reads. Both
  are one line, and both apply to all nine logics.

Everything else here is untouched, including the remaining item the profile names:
`History.Go`'s strict score.
