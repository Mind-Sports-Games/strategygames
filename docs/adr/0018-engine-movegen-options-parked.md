# 0018 — Engine-level movegen options parked behind a movegen-bound consumer

**Status:** Proposed (awaiting Lakin's review — recorded for later decision, not for
implementation now)

## Context

The MOVEGEN follow-up to the go speed dive (session scratchpad
`speeddive/results/RESULTS-MOVEGEN.md`, building on `RESULTS-E3.md` and
[docs/go-engine-speed-dive.md](../go-engine-speed-dive.md)) prototyped and measured how far
engine-level `legalDrops` can go past E3's 12.3x-vs-joansala under the immutable contract of
[ADR 0005](0005-immutable-api-interior-mutability.md).

The decisive measurement is the consumer profile: no production consumer is
engine-movegen-bound. Every production consumer goes through `go.Variant.validDrops`, which
costs ~100x the engine call it wraps (engine `legalDrops` ≈ 1% of one validDrops call). The
validDrops laziness fix — move `afterDrop` inside the `LazyBoardAfter` thunk and delete the
dead `isRepetition` filter (constant-false per
[ADR 0011](0011-superko-forbidden-up-front-isrepetition-false.md)) — ships separately as its
own queue order and is not in this ADR's scope. After it ships, production movegen calls
land at ~11.5µs @19x19 and are still wrapper-dominated. Any engine-level speedup below is
therefore invisible to production today.

## Options (measured on the E3 bitboard prototype, same-run ratios)

**A — incremental atari registry.** Per-chain `(pseudoLibertyCount, libertySum,
libertySumSquares)`. Atari detection is the Cauchy-Schwarz equality condition
`n·Σx² == (Σx)²` ⇔ all pseudo-liberties identical ⇔ exactly one real liberty; the sole
liberty is `Σx/n`. Merge is componentwise addition, no dedup; maintenance rides apply's
existing neighbor loops; the per-chain liberty walks E3 identified as the movegen floor are
deleted. Clone cost: +2.9KB/move @19x19. Measured: engine `legalDrops` 1.59–1.68µs @19x19 =
**16–17x vs joansala** (E3: 12.3x); price: apply 0.42 → 0.72–0.77µs, a **~1.7x tax on the
operation production replay actually leans on** (engine replay gain over production shrinks
to ~1.2x).

**B — bitmask return contract (stacks on A).** `legalDropWordsInto(dst: Array[Long])`: 6
longs @19x19 into a reused caller buffer instead of a ~200-int array — no materialization,
no lazy-val, no result clone. Measured 0.956µs @19x19 on a loaded machine ≈ **28x vs
joansala**, ~30x plausible quiet. Breaks the `Api.Position.legalDrops: Array[Int]` seam and
gives it a mutable-buffer discipline; only pays for bit-oriented consumers — converting the
mask back to `List[Pos]` at the seam repays exactly the saving.

**C — full E3 bitboard core fusion with mutable make/unmake state (recorded for
completeness).** The only route past ~30x; even then the floor is ~0.4–0.5µs @19x19 with
exact-superko parity. 100x is implausible without abandoning both the immutable contract
(ADR 0005) and exact-superko-at-movegen parity.

## Decision

None of these options ships now. All three are parked behind the same trigger condition as
E3's parking: an engine-internal movegen-bound consumer — go playouts, AI, or
analysis-engine work in playstrategy. Absent such a consumer, no production caller can
observe the speedup, and Option A actively taxes replay.

If the trigger fires, revisit in order A → B → C: A is contract-preserving; B requires a
seam change only a bit-oriented consumer can cash; C requires superseding ADR 0005 for that
consumer's path.

## Consequences

- Production keeps the current engine movegen (post-E3 landing decisions unchanged);
  the shipped speedup for go movegen is the wrapper-level validDrops laziness fix.
- Apply and replay pay no registry tax; the immutable contract and the `Array[Int]`
  movegen seam stay intact.
- The demonstrated 16–17x / ~28x tiers are foregone until a movegen-bound consumer exists;
  claiming them later requires re-doing the work, not just accepting this ADR.
- Prototype and evidence are session artifacts: `MOVEGEN.patch` (supersedes `E3.patch` on
  the d708e002 base) plus parity evidence (22/22 ported scenarios, 132 expectations;
  every-ply corpus differential across all 720 prefixes at three sizes) in the session
  scratchpad results directory. If this ADR is later accepted, re-derive the prototype from
  the patch or rebuild it against the then-current engine — the patches are not durable
  storage.
