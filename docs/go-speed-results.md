# Go speed dive: results

The go rules live as concrete `def`s on `go/variant/Variant.scala`, with connectivity in
`go/Chain.scala` and position state on `go/Board.scala`, where the area score is a derived
`lazy val` alongside `actors`, `posMap` and `piecesOnBoardCount`. The `Api.Position` seam and the
`go/engine/` package are gone, and there is one replay path.

Measured **2026-08-08** at `dd250768` with `GoRulesBenchmark` in `bench/`. These numbers replace both
the 2026-07-25 table (which measured the engine-backed seam) and an intermediate table taken earlier
today, before the area score moved off `go.History` onto `go.Board`.

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
| **`wrapperReplay`** — wrapper, one `Game` per ply, score unread | **1205.7 ± 8.3** | **2750.2 ± 85.3** | **8494.6 ± 477.8** |
| **`wrapperReplayReadingEveryScore`** — wrapper, score read every ply | **3326.3 ± 87.5** | **10135.1 ± 216.1** | **39874.5 ± 1560.1** |

**lila runs go through the wrapper.** The two bold rows are therefore the production figures, and
the two go-package rows measure the rules on their own. At 19x19 a real consumer sees **11.3x
joansala if it never reads the score and 2.4x if it reads one every ply** — not the 29.7x the
`replay` row shows. Quote a wrapper number when the question is what the site will feel, and a
go-package number only when the question is what the rules cost. The gap between the pairs is the
finding, not a footnote: at 19x19 the wrapper adds 5.25 ms to a 3.24 ms replay before anything reads
a score, and 36.6 ms if something reads one every ply.

**Which of the two wrapper numbers is live is not known here.** The spread is 4.7x on identical
library code, decided entirely by whether the caller touches a history field per ply. Nothing in this
repo can answer which lila does; the question has to go to the integration. Go is a scoring game and
its score is normally on screen, so the pessimistic figure may well be the live one — that is a
reason to find out, not an answer.

- **`go.Replay.gameFromUciStrings`** returns one final `go.Game` and never scores unless something
  reads `Board.areaScore`. Reading it once at the end costs one flood fill: +67 / +47 / +198 µs.
- **`strategygames.Replay.gameWithUciWhileValid`** materialises one wrapper `Game` per ply. Since
  `1d658123` the wrapped history is by-name into a `lazy val`, so a consumer that never reads a
  history field never scores. Reading `history.score` on every ply costs **2.8x / 3.7x / 4.7x**,
  because `History.Go` takes the score as a strict parameter — so reading *any* history field forces
  that ply's flood fill.
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
the rules in isolation.

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
| `fenRenderMidGame` | 13.6 ± 0.1 | 32.6 ± 0.3 | 80.4 ± 1.1 | 7.8 engine |

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
| `wrapperReplay` | 5,317,980 | 12,836,084 | 44,432,706 |
| `replay`, score strict (this branch, earlier) | 6,701,569 | 20,722,629 | 82,142,796 |
| engine batch path (removed) | 174,249 | 282,548 | 566,081 |

The score change cut go-package replay allocation **2.9x / 5.0x / 8.4x**.

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

The wrapper path's overhead over the go-package path is 3.4 / 6.3 / 13.1 µs per ply, scaling with
board area, and the profile names one cause:

| share | frame |
| --- | --- |
| 13.1% | `HashMap.map` ← `Board$.Go$superArg$1` ← **`Board$Go.<init>`** |
| 9.6% | `HashMapBuilder.update` ← the same `map` |
| 1.9% + 1.4% + 1.2% | more builder and iterator frames under the same `map` |

That is one line: `strategygames.Board.Go` rebuilds the whole `PieceMap` as
`b.pieces.map { case (pos, piece) => (Pos.Go(pos), (Piece.Go(piece), 1)) }`, and `pieces` is a
**strict** constructor argument, so every wrapper `Board` — one per ply — pays a full board scan.
`1d658123` deferred the wrapped history this way; `pieces` is the same shape and was not deferred.
~27% of named wrapper samples sit under it.

## What the follow-up branch should take first

Both items are on the wrapper path, which is why they lead: that is the path lila runs. Neither was
actionable before, because until the owner confirmed the path nobody knew which numbers described
production.

1. **`strategygames.Board.Go` rebuilds the whole `PieceMap` as a strict constructor argument.** One
   full board scan per wrapper `Board`, one wrapper `Board` per ply; ~27% of named samples in
   `wrapperReplay` at 19x19, and per-ply wrapper overhead that scales with board area (3.4 / 6.3 /
   13.1 µs). This is the 5.25 ms.
2. **`History.Go` holds the area score strictly**, so reading *any* history field on a wrapper board
   forces that ply's flood fill — the 4.7x between the two wrapper rows.

Both are the shape `1d658123` already fixed for the wrapped history: a strict constructor argument
that a `lazy val` behind a by-name parameter would defer. `pieces` is the same shape and was not
deferred; the score needs the by-name treatment or a board-carrying `History.Go`, and
`docs/go-refactor.md` records what each costs.

## What this is not

None of the above was acted on here. This is the idiomatic branch; every number is a reported finding
and an input to the follow-up performance branch. The one design change that did land —
`History.score` strict → `Board.areaScore` derived — landed because it was a misreading of the house
idiom rather than a considered trade: Togyzkumalak's strict `history.score` is an accumulated capture
count, essentially free, whereas Go's is a flood fill, and the house pattern for derived state is a
`lazy val` on `Board`.
