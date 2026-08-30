# The go speed dive

> **Frozen, 2026-08-08. Every subject of this document has been deleted.** It measures the
> `Api.Position` seam, the `go/engine/` package, the batch and per-ply replay paths and the joansala
> baseline, none of which exist any more ([ADR 0003](adr/0003-go-rules-in-variant.md)). It is kept
> for the joansala reference numbers, the measurement protocol, and the reasoning about what was and
> was not reachable — not as a description of the code. Current measurements:
> [docs/go-speed-results.md](go-speed-results.md).

Headline tables: [docs/go-speed-results.md](go-speed-results.md).

Can the pure-Scala go engine's production surface reach 10x / 20x / 30x / 100x the retired joansala
engine? Five experiments on branch `lakin/go-speed-dive` (base b3cd2803) answered it per workload.
Reference denominators are the joansala JMH baselines: replay 7395 / 24313 / 96385 µs, legalDrops
4.21 / 11.56 / 26.77 µs, applyDrop 17.84 / 30.06 / 60.88 µs (9x9 / 13x13 / 19x19).

| target | full-game replay (production path)                                                                                                      | movegen (legalDrops)                                                                               | apply (drop)                                                                                                             |
| ------ | --------------------------------------------------------------------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------ |
| 10x    | **DEMONSTRATED** — E1 alone: 18x / 32x / 41x, byte-identical output                                                                     | **DEMONSTRATED** at 13/19 — E3 bitboard: 12.1x / 12.3x (9x9: 7.7x, fixed costs dominate 81 points) | at hand — production is already 9.1–10.5x                                                                                |
| 20x    | **DEMONSTRATED** — E1 at 13/19; E4 everywhere                                                                                           | not reached (E3 measured 12.3x); **IMPLAUSIBLE under the immutable contract** — see below          | **PROJECTED ~29x** — E1's score deferral: 6.4 µs @19 = 1.2 engine + 4.3 forced areaScore + 0.9 wrapper → ~2.1 µs         |
| 30x    | **DEMONSTRATED** — E4: 80x / 118x / 162x                                                                                                | as above; plausible only fused with E2's mutable state (unmeasured)                                | **PROJECTED ~46x** — E1 arithmetic + E3 engine apply (422 ns @19 + ~0.9 µs wrapper)                                      |
| 100x   | **DEMONSTRATED at 13/19** — E4: 118x / 162x. **PROJECTED at 9x9**: E4 wrapper (92.2 − 41.9 µs) + E2 bulk engine (~13 µs) ≈ 63 µs ≈ 117x | **IMPLAUSIBLE** — no identified path; exact-superko candidate discovery is the floor               | not meaningful per-call at the seam; the 100x class is batch replay (E4+E2), where E3's engine apply is 144x cross-layer |
| beyond | **PROJECTED ~570x @19** — E4's measured ~120 µs wrapper floor + E2's 52.6 µs materialized bulk engine ≈ 170 µs                          | —                                                                                                  | —                                                                                                                        |

DEMONSTRATED means measured on this machine with same-run ratios and an equivalence suite green.
PROJECTED means arithmetic over measured components, stated inline. IMPLAUSIBLE means a measured
floor argument, not a hunch.

## Where production replay time went

Profiling (GoLayerBenchmark + async-profiler, 19x19, 400 plies) showed the engine core is **~5% of
production replay**. The wrapper forced work the engine never asked for:

| production replay cost, 19x19 | share | cause                                                                                             |
| ----------------------------- | ----- | ------------------------------------------------------------------------------------------------- |
| forced movegen per ply        | ~25%  | `Game.apply` → `status` → `Variant.specialEnd` probed `legalActions.size`                         |
| forced areaScore per ply      | ~18%  | `Board.afterDrop` forced `fenScore` — a full flood fill nobody read until game end                |
| pass-path full re-replays     | ~20%  | every pass re-parsed the FEN and replayed the whole prefix through the seam                       |
| wrapper residual              | ~30%  | two independent O(n²) `List :+` chains, copies, uci regex, per-ply `Role.allByForsyth` map builds |
| engine core (`GoState`)       | ~6%   | of which the five per-move array clones are 15% of total                                          |

Layer decomposition, whole-game replay 19x19: engine fold 465 µs → seam batch 492 → seam per-ply
630 → production 8575 µs. The corpus pass indices meant production did 3.7–4.7x the engine work of
the game itself. Full working notes, target tables, and raw JMH JSON live in the session scratchpad
(`speeddive/NOTES-analysis.md` and `speeddive/results/`).

## The experiments

| #   | hypothesis                                                                                                    | verdict                     | headline (9x9 / 13x13 / 19x19)                                                                                                                                                         |
| --- | ------------------------------------------------------------------------------------------------------------- | --------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| E1  | kill forced per-ply movegen/areaScore + pass re-replay: ~2–4x on replay, no engine change                     | **CONFIRMED**, exceeded     | prod replay 2.5x / 3.5x / 3.4x → **18x / 32x / 41x vs joansala**, byte-identical, full suite green; 5 files, +21/−12                                                                   |
| E2  | batch-replay seam: one mutable scratch state, publish the final position → engine floor                       | **CONFIRMED**, above range  | **86 / 115 / 104 ns/ply** (4.6x / 6.6x / 11.8x over the immutable fold); allocation 12x / 28x / 69x down — per-ply marginal ~0 B                                                       |
| E3  | bitboard core beats the byte-array + clone engine 5–10x per move                                              | **PARTIAL**                 | movegen 1.4x / 2.0x / **2.2x** vs current (= 12.3x vs joansala @19), apply 2.17x (422 ns @19), engine replay 1.83x — exact parity, but the 20x tier is unreachable under copy-per-move |
| E4  | int-move replay entry (no uci strings, regex, Pos, pieceMap until the end) removes the remaining wrapper cost | **CONFIRMED** beyond target | full-`Game` replay **594 µs @19 = 162x** (80x / 118x / 162x); per-ply-positions variant 193x; engine is now 69% of the path                                                            |
| E5  | superko `Set[Long]` → primitive table + allocation shapes: 1.3–2x on the engine core                          | **REFUTED**                 | 1.12x / 1.01x / **1.07x**; superko-deleted control proves the whole subsystem is worth ≤1.23x / 1.07x / 1.11x — there is no 1.3x in the JVM shapes                                     |

**E1** — the three changes: `specialEnd` reads `apiPosition.gameEnd` (the `legalActions.size == 0`
clause is provably redundant — a finished game lists no legal actions, see the divergence table in
[docs/go-engine.md](go-engine.md));
`go.History` carries `scoring: () => Score` so a replay runs one flood fill instead of 400; the pass
path plays one action on the position in hand instead of re-replaying the prefix. Lesson: the score
deferral paid ~3.7 ms @19 where the 4.31 µs × 400 microbenchmark predicted ~1.7 ms — the flood
fill's per-ply allocation (two scratch arrays) made GC pressure, not just scan time.

**E2** — `replayAll(start, moves): FrozenGoPosition`: the exact `GoState` algorithm mutating one
private copy of the arrays, superko in an open-addressed `Array[Long]`, nothing published until the
fold completes (interior mutability per [ADR 0001](adr/0001-pure-scala-go-engine.md)).
Hit the a-priori floor estimate exactly (104 ns/ply @19). Two lessons: clone-traffic removal scales
with board area, so the win grows with size; and **the bench corpora never exercise the positional-
superko probe** — the differential spec stayed green with the probe disabled across all 723 corpus
prefixes. A constructed ko cycle (capture, two passes, forbidden recapture) provides the red-green
case; as landed it lives in `BulkReplayTest` and is the sole probe guard, since the prefix
differential stays green with the probe neutered — the scratch still records the hashes and the
materialized `legalMoves` stay right. Corpus-only differential coverage is not sufficient for
superko; any future engine work must keep that constructed case and must never collapse it into the
differential.

**E3** — unpadded bitboards, bulk movegen (`empty`, one dilation pass, per-word emit), scalar rules
only for capture candidates and eye-shaped points; hashes bit-identical to production. Bulk
legality works — the residual is **exact-superko capture-candidate discovery**: matching production
means knowing per movegen call which points capture, i.e. the sole liberty of every enemy atari
chain, and that scan dominates once per-point work is gone. The unlock (incrementally maintained
per-chain liberty bitmaps) blows the clone budget of an immutable state — it belongs with E2's
mutable scratch, not with copy-per-move.

**E4** — a pre-pass plans int moves (shape check instead of regex, `Role.allByForsyth` hoisted —
production rebuilds that Map every ply), the fold runs on `GoGame`, and `Board`/`Situation`/`Game`
plus pieceMap/score/FEN materialize once at the end. Equivalence field-by-field against
`Replay.gameFromUciStrings` on all corpora plus hand-written pass/`ss:`/mid-game-FEN cases, both
turn framings. Two production quirks were reproduced, not fixed (score not updated on a pass;
`ss:` captures off by +1). Lesson: the uci path was worth ~140 µs @19, not the ~2.3% the flamegraph
suggested — profiles under-attribute costs that are smeared across allocation and map-building.

**E5** — the negative result, ceiling-proven: deleting superko outright (unshippable) bounds every
possible superko fix at 1.11x @19, and the shipped bundle captures most of that. Three durable
lessons: a flat copy-on-write superko table is a **regression** (0.74x @19) under copy-per-move —
mutate-and-freeze (E2) is the only home for it; in an immutable structure the amortized rebuild
must be a memcpy, not a rehash (rehashing per fold cancelled the entire win); and `scala.runtime.*Ref`
cells in bytecode are not evidence of runtime allocation — escape analysis scalarised all of them
(bytecode-verified 31 → 0 refs, measured effect nil). By-product worth keeping: a clean immutable
`SuperkoHistory` primitive-table implementation.

## Productionization order

Patches were built in isolated worktrees off b3cd2803 and live in the session scratchpad
(`speeddive/results/E1.patch` … `E5.patch`) — session-scoped storage, so land or copy them before
the session's scratchpad is discarded. Nothing was committed; production code on the branch is
untouched.

1. **E1 wrapper fixes** — smallest (+21/−12), byte-identical output, ~40x replay on its own.
   Risk: `go.History`'s constructor shape changes (`score: Score` → `scoring: () => Score`);
   nothing in this repo constructs it positionally, but downstream (lila) needs a grep before
   landing. `Api.Position.legalActions` loses its last production caller — retiring it is a
   separate decision.
2. **E4 + E2 as one batch-replay seam API** — the 100x-class path. Give the seam
   (`Api.Position`, [docs/go-engine.md](go-engine.md)) a batch entry point that folds int moves
   and publishes one final `Position` (E2's shape); wrapper assembly stays in `Replay`, so no
   second engine-aware file. Prerequisite: resolve the **fenPassCount inconsistency** — when a
   resumed FEN carries `fenPassCount` 1..3, `Forsyth.<<@` seeds synthetic pass entries which the
   production pass path re-replays, inflating `GoGame.plyCount` (and the FEN full-move field)
   relative to the drop path, and double-counting `consecutivePasses` (unobservable only because
   every reader clamps it). The fast path plays exactly the actions given; production must agree
   with itself before either can be called the reference. Also carry E2's constructed-superko test
   and E4's hand-written pass/`ss:` cases — the corpora alone catch neither superko probes nor
   halfMoveClock inversions.
3. **E3 bitboards** — a real ~2x engine multiplier with exact parity, but its 20x-movegen future
   requires fusing per-chain liberty bitmaps into E2's mutable scratch state. Recommend as a later
   separate effort, or fold into the batch-API work if movegen throughput ever matters (it is not
   on the replay critical path once E4+E2 land). Parked; see below.

E5 is not worth landing for speed; keep `SuperkoHistory` only if E2's table wants to share the
implementation.

## What we now know about the floor

- **Engine replay floor, immutable contract:** ~1.16 µs/ply @19x19 (465 µs/400 plies). The five
  array clones per placement (7.6 KB @19) are ~96% of engine allocation and half the fast-path
  profile. This is the price of [ADR 0001](adr/0001-pure-scala-go-engine.md)'s
  copy-per-move, and no JVM micro-work moves it (E5: 1.07x, ceiling-proven).
- **Engine replay floor, mutate-and-freeze:** **104 ns/ply @19x19** measured (E2), matching the
  a-priori estimate of 100–200 ns for legality-checked replay with positional superko. Allocation
  is per-game fixed cost (~46 KB @19), ~0 B/ply marginal.
- **Exact positional superko costs almost nothing on replay** — deleting it entirely is worth at
  most 1.11x @19 on the immutable engine (E5 control) — **but it is the movegen floor**: exact
  parity requires per-call knowledge of which points capture, and without incremental per-chain
  liberty tracking that discovery scan caps bitboard movegen at ~2.2x over current (E3).
- **Wrapper floor for a full `Game`-producing replay:** ~120 µs @19x19 above the engine (E4), not
  the ~30 µs E2's projection assumed. The remaining levers (each ~35 µs @19): an index walk
  replacing `actionStrsWithEndTurn`'s tuple churn, builders for the `actionStrs` Vector, and
  making `Board.pieces` lazy so the one-shot pieceMap scan is only paid when read.
- **Single-shot costs @19x19** for budgeting a materialize-once wrapper: pieceMap full scan
  21.9 µs, areaScore 4.3 µs, legalDrops 4.2 µs, FEN render 7.8 µs, FEN parse 5.9 µs.
- **Combined projection**, E4 wrapper + E2 engine: ~170 µs @19x19 ≈ 570x joansala; ~63 µs @9x9
  ≈ 117x — every size clears 100x, none of it requiring bitboards.

## Productionized (post-landing addendum)

Everything above reports prototype numbers from throwaway worktrees. This section reports the
**landed API** measured on 2026-07-25: E1 + the batch-replay seam (`Api.positionFromVariantStartingFenAndMoves`
as `Replay.gameFromUciStrings`'s default) + the `validDrops` laziness fix, at branch HEAD `ba81f8ce`
plus this section's bench additions.

Machine/protocol: JDK 25.0.2 (devenv, OpenJDK 64-Bit Server VM), 32 cores, load average 5.6–7.5
throughout (not an idle box — the wide 99.9% error bars below are dominated by that, not by variance
in the code). JMH `-wi 3 -w 2s -i 5 -r 2s -f 1 -to 60s`, `avgt`, µs/op ± 99.9% CI. Corpora:
go9x9 120 turns, go13x13 200, go19x19 400, go9x9superko 23 (`ss:`-terminated). The tables below and
this protocol line are the record of the run: the raw JMH JSON artifacts were session-local and are
not committed — every row is reproducible from the committed benchmarks (`GoLayerBenchmark`,
`GoEngineBenchmark`, `GoMovegenConsumerBenchmark` in `bench/`) with the invocation above, plus
`-prof gc` for the allocation rows.

### (a) Full-game `Game`-producing replay — the headline

`GoLayerBenchmark`: `prodReplay` = `Replay.gameFromUciStrings` (batch default);
`prodReplayPerPly` = `Replay.gameFromUciStringsPerPly` (the retained old path, same run).

| µs/op                   | go9x9              | go13x13            | go19x19              |
| ----------------------- | ------------------ | ------------------ | -------------------- |
| batch default           | **72.701 ± 23.525** | **122.036 ± 5.862** | **277.589 ± 51.364** |
| per-ply (retained path) | 331.937 ± 79.274   | 648.235 ± 75.051   | 2127.302 ± 622.382   |
| joansala baseline       | 7395               | 24313              | 96385                |
| **batch vs joansala**   | **102x**           | **199x**           | **347x**             |
| per-ply vs joansala     | 22x                | 38x                | 45x                  |
| batch vs per-ply        | 4.6x               | 5.3x               | 7.7x                 |

The `ss:`-terminated corpus, where the batch path pays two folds and two pieceMap scans
(`GoEngineBenchmark` at `size=go9x9superko`, no joansala denominator):

| µs/op        | go9x9superko (23 turns) |
| ------------ | ----------------------- |
| batch        | 43.226 ± 4.775          |
| per-ply      | 52.816 ± 17.947         |
| batch vs per-ply | 1.2x                |

This row is a one-shot: it was taken by temporarily extending `GoReplayInput`'s `@Param` with
`go9x9superko` and adding a per-ply twin to `GoEngineBenchmark`. Both were removed again in the
review fixups — the committed `@Param` lists stay as order 06 decided, and `prodReplayPerPly` in
`GoLayerBenchmark` is the single committed home for the per-ply comparison. Reproducing this row
means re-extending the param list by hand.

### (b) Seam level — batch, per-ply positions, per-ply

`GoLayerBenchmark` seam workloads. All four consume only `.turn`/`.size`
except `seamPerPlyPieceMapReplay`, which forces a pieceMap every ply.

| µs/op                       | go9x9            | go13x13           | go19x19           |
| --------------------------- | ---------------- | ----------------- | ----------------- |
| `seamBatchReplay`           | 32.045 ± 5.590   | 61.433 ± 9.527    | 132.594 ± 25.270  |
| `seamPerPlyPositionsReplay` | 55.270 ± 3.697   | 142.665 ± 54.509  | 486.454 ± 48.913  |
| `seamPerPlyReplay`          | 70.372 ± 14.964  | 155.640 ± 53.521  | 477.733 ± 44.249  |
| `seamPerPlyPieceMapReplay`  | 105.081 ± 9.624  | 200.868 ± 12.355  | 584.538 ± 66.138  |
| positions vs per-ply        | 1.27x            | 1.09x             | 0.98x             |
| positions vs per-ply+pieceMap | 1.90x          | 1.41x             | 1.20x             |

The per-ply positions entry publishes every intermediate `Position`, so it must keep the immutable
per-ply `GoGame.play` fold; it only removes per-ply string planning and FEN re-parse. That win
shrinks with board area and is gone by 19x19 — its value there is the unforced pieceMap (1.20x),
not the fold.

### (c) Engine fold

`GoLayerBenchmark`: `bulkEngineReplay` = `BulkReplay.replay` (mutate-and-freeze scratch);
`engineReplay` = the immutable `GoGame.play` loop.

| µs/op               | go9x9          | go13x13         | go19x19         |
| ------------------- | -------------- | --------------- | --------------- |
| `bulkEngineReplay`  | 20.504 ± 2.211 | 38.661 ± 4.341  | 92.837 ± 5.159  |
| `engineReplay`      | 39.177 ± 4.114 | 109.154 ± 11.572 | 393.785 ± 37.388 |
| bulk vs immutable   | 1.9x           | 2.8x            | 4.2x            |
| ns/ply, bulk        | 171            | 193             | 232             |

Not comparable to E2's 104 ns/ply @19: the landed `BulkReplay.replay` includes the terminal
`fromStoneOwners` + `withReplayHistory` materialization that E2 measured as a separate benchmark,
and this box is loaded where E2's was quiet.

### (d) Movegen consumer surface

`GoMovegenConsumerBenchmark`, mid-game situation. The replica is what `validDrops` used to do
(apply every candidate drop eagerly); prod now defers `afterDrop` into a `LazyBoardAfter` thunk.

| µs/op                                  | go9x9          | go13x13         | go19x19          |
| -------------------------------------- | -------------- | --------------- | ---------------- |
| `eagerlyAppliedValidDropsReplicaMidGame` | 39.135 ± 2.562 | 202.166 ± 27.420 | 794.011 ± 106.099 |
| `prodValidDropsMidGame`                | 1.935 ± 0.750  | 5.926 ± 1.454   | 13.921 ± 3.802   |
| `prodDropsByRoleMidGame`               | 2.791 ± 0.721  | 7.840 ± 1.407   | 17.865 ± 3.943   |
| validDrops vs replica                  | 20x            | 34x             | 57x              |
| dropsByRole vs replica                 | 14x            | 26x             | 44x              |

### (e) Allocation, headline rows

The (a) workloads rerun under `-prof gc`, same protocol; that run's timings — 71.163 / 125.549 /
285.211 batch, 314.524 / 690.637 / 2024.866 per-ply — corroborate (a).

| B/op                    | go9x9       | go13x13     | go19x19     |
| ----------------------- | ----------- | ----------- | ----------- |
| batch default           | 174,249     | 282,548     | 566,081     |
| per-ply (retained path) | 1,086,081   | 2,549,019   | 8,573,746   |
| reduction               | 6.2x        | 9.0x        | 15.2x       |

### Verdict

Sanity gate passed with room: 19x19 batch full-game replay is **277.6 µs**, better than E4's 594 µs
prototype and inside the 170–250 µs projection's order of magnitude. The prototype paid `Replay`'s
own per-ply planner; production hoists parsing into the seam, so the wrapper does one index walk and
one materialization. Production replay now clears 100x joansala at every board size (102x / 199x /
347x) and the engine fold, not the wrapper, is the remaining floor: at 19x19 the fold is 92.8 µs of
the 277.6 µs path (33%).

### E3 stays parked

E3's bitboard core does not land. Under copy-per-move it is worth 2.2x on movegen @19x19 — real, but
the tier it was chased for (20x-class movegen) needs incrementally maintained per-chain liberty
bitmaps, and maintaining those across a copy-per-move state blows the clone budget
[ADR 0001](adr/0001-pure-scala-go-engine.md) sets. They belong fused into the mutable
scratch state, which is a different engine, not a faster one. Revisit if go movegen throughput ever
appears on a production critical path; replay is not that path now that batch replay has landed, and
the movegen consumers that exist are wrapper-dominated. The measured tiers a movegen-bound consumer
could then cash, and the order to take them in, are recorded in
[ADR 0002](adr/0002-go-batch-replay.md) ("Parked: engine-level movegen options").
