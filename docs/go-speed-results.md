# Go speed dive: results

The go rules now live as concrete `def`s on `go/variant/Variant.scala`, with connectivity in
`go/Chain.scala` and position state on `go/Board.scala`. The `Api.Position` seam and the
`go/engine/` package are gone, and there is one replay path.

Numbers below were measured on that code on **2026-08-08** with `GoRulesBenchmark` in `bench/`.
They replace the 2026-07-25 table, which measured the engine-backed seam; the two `Replay` paths
that table compared ("batch default" and "per-ply") no longer exist as separate paths, so the
comparison is now against the single `Replay.gameFromUciStrings`.

The historical investigation that produced the joansala baseline is frozen in
[docs/go-engine-speed-dive.md](go-engine-speed-dive.md).

## Protocol and machine

```
sbt "bench/Jmh/run -wi 5 -w 2s -i 10 -r 2s -f 1 -to 120s -rf json -rff go-jmh.json \
  strategygames.bench.Go.*"
```

JMH 1.37, `avgt`, µs/op ± 99.9% CI. AMD Ryzen 9 8945HS (8 cores / 16 threads), 60 GB RAM, NixOS,
OpenJDK 21.0.12+2, otherwise idle. Corpora: go9x9 120 turns, go13x13 200, go19x19 400 — generated
on first use from the fixed seed, not committed.

**Cross-machine caveat, and it matters.** The joansala baseline (7,395 / 24,313 / 96,385 µs) and
every "removed engine" column below were measured on a *different* box — 32 cores, JDK 25.0.2, load
average 5.6–7.5 — per the protocol line in `docs/go-engine-speed-dive.md`. joansala and the engine
are both deleted, so no same-run comparison is possible any more. Every ratio in this document that
crosses those two runs is indicative, not a controlled measurement.

## Full-game replay (µs per game, lower is better)

`GoRulesBenchmark.replay` = `go.Replay.gameFromUciStrings` over the whole corpus fixture.

| µs/op                        | go9x9                | go13x13               | go19x19                 |
| ---------------------------- | -------------------- | --------------------- | ----------------------- |
| **rules in `Variant`**       | **2698.5 ± 29.5**    | **7777.8 ± 47.8**     | **30690.3 ± 327.1**     |
| joansala (removed)           | 7395                 | 24313                 | 96385                   |
| **vs joansala**              | **2.7x faster**      | **3.1x faster**       | **3.1x faster**         |
| engine batch path (removed)  | 72.7                 | 122.0                 | 277.6                   |
| vs engine batch path         | 37x slower           | 64x slower            | 111x slower             |
| engine per-ply path (removed)| 331.9                | 648.2                 | 2127.3                  |
| vs engine per-ply path       | 8.1x slower          | 12.0x slower          | 14.4x slower            |

Per ply that is 22.5 / 38.9 / 76.7 µs.

## Legal-drop list for a client (µs per call, mid-game position)

`GoRulesBenchmark.validDropsMidGame` = `Situation.dropsAsDrops` = `Variant.validDrops`.

| µs/op                              | go9x9            | go13x13           | go19x19           |
| ---------------------------------- | ---------------- | ----------------- | ----------------- |
| **`validDropsMidGame`**            | **35.4 ± 0.2**   | **104.1 ± 0.7**   | **278.2 ± 2.8**   |
| **`dropsByRoleMidGame`**           | **38.8 ± 0.4**   | **106.3 ± 0.7**   | **297.4 ± 3.0**   |
| seam `validDrops`, lazy (removed)  | 1.9              | 5.9               | 13.9              |
| vs seam lazy                       | 18x slower       | 18x slower        | 20x slower        |
| seam `validDrops`, eager (removed) | 39.1             | 202.2             | 794.0             |
| vs seam eager                      | 1.1x faster      | 1.9x faster       | 2.9x faster       |

The lazy seam row is the honest comparison and it is 18–20x. The eager row is included because the
current list is fully materialised — every candidate `Drop` is constructed — and against the seam's
*eager* behaviour the new code is faster at every size.

## Single-shot costs (µs per call, mid-game position)

| µs/op                        | go9x9      | go13x13    | go19x19     | removed reference @19x19 |
| ---------------------------- | ---------- | ---------- | ----------- | ------------------------ |
| `applyDrop`                  | 13.0 ± 0.1 | 29.6 ± 1.2 | 59.9 ± 0.4  | 60.9 joansala, ~6 seam   |
| `areaScoreMidGame`           | 11.9 ± 0.2 | 27.2 ± 0.4 | 60.8 ± 0.7  | 4.3 engine               |
| `fenParseMidGame`            | 28.4 ± 0.1 | 50.1 ± 0.4 | 101.1 ± 1.0 | 5.9 engine               |
| `fenRenderMidGame`           | 14.6 ± 0.2 | 32.9 ± 0.4 | 80.0 ± 1.0  | 7.8 engine               |

`applyDrop` = `Variant.boardAfter`, and at 19x19 it now costs what joansala's did (60.9 µs) — about
10x the engine-backed seam it replaced. `fenRenderMidGame` = `Forsyth.exportBoardFen`, which itself
calls `areaScore`; the 19.2 µs @19x19 between it and `areaScoreMidGame` is the rendering.
`fenParseMidGame` = `Forsyth.<<@`.

## Allocation per full-game replay

`-prof gc`, same protocol at `-wi 3 -i 5`.

| B/op                        | go9x9      | go13x13     | go19x19     |
| --------------------------- | ---------- | ----------- | ----------- |
| **rules in `Variant`**      | 6,701,569  | 20,722,629  | 82,142,796  |
| engine batch path (removed) | 174,249    | 282,548     | 566,081     |
| engine per-ply path (removed)| 1,086,081 | 2,549,019   | 8,573,746   |

## Where the time goes

`applyDrop × plies` accounts for **58% / 76% / 78%** of full-game replay (9x9 / 13x13 / 19x19), and
`areaScore` accounts for **92% / 92% / ~100%** of `applyDrop`. So the strict `History.score` is
roughly **53% / 70% / 79%** of a full-game replay. (`areaScore` is measured on one mid-game board
and applied to every ply, so treat the share as an estimate, not an integral.)

JMH's sampling profiler on `replay` at 19x19 (`-prof stack:lines=4`) names only `areaScore`'s own
call tree and the collections underneath it:

| RUNNABLE share (of named samples) | frame |
| --------------------------------- | ----- |
| 5.1% | `Statics.anyHash` → `HashMap.contains` → `Variant.emptyRegionsOf`'s `isEmpty` predicate |
| 4.6% | `Statics.anyHash` → `HashMap.get` → `Variant.borderingPlayersAt` |
| 3.6% + 2.9% | `BoxesRunTime.equals2` → `BitmapIndexedMapNode.containsKey` / `.get` |
| 2.5% | `HashSet.concat` (region and chain accumulation in `Chain.Stones.grownFrom`) |
| 1.9% + 1.9% | `HashMapBuilder` / `StrictOptimizedMapOps.collect` → `Board.playerPiecesOnBoardCount` |
| 1.6% | `List.filter` in `Chain.Stones.grownFrom` |

The single named root cause under all of it: **`PieceMap = Map[Pos, Piece]`.** `Pos` is a case class,
so every liberty test and every region step is a boxed structural hash and equality against an
immutable `HashMap`. The deleted engine indexed a byte array by `Pos.index`. That representation
difference, not the flood fill's asymptotics, is what makes `areaScore` 14x its engine cost and
`fenParse` 17x its.

The ~9.5 / 9.3 / 16.9 µs per ply of replay *not* explained by `applyDrop` is `Replay`'s own
bookkeeping: `Variant.drop`'s `isPlayable` for the played point, the superko `hasOccurred` scan, the
uci regex, a fresh `Role.allByForsyth(gameFamily)` map per drop ply, and one `Game` allocated per ply
of which `gameFromUciStrings` keeps only the last.

## What this is not

None of the above was acted on. This branch is the idiomatic one; every number here is a reported
finding and a candidate for the follow-up performance branch, not a defect. The two costs the design
deliberately accepted — strict `History.score`, and per-candidate legality computed from the
`PieceMap` rather than a maintained union-find index — are exactly the two the measurements are
dominated by, which is the expected outcome of that choice and not a surprise.
