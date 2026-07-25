# 0001 — Ship the pure-Scala Go engine as parallel variants (ids 5/6/7)

**Status:** Accepted (2026-07-24)

## Context

The Go game logic delegates move generation, scoring, and repetition detection to the
external joansala engine behind `strategygames.go.Api.Position`. A pure-Scala replacement
engine is being built (target: ≥5x faster on legal-move generation and full-game replay).
Cutting the existing variants over to it in one step would leave no in-tree oracle to
benchmark or differential-test against, and would put live games at risk if the new engine
diverges.

Go variant ids in use: 1, 2, 4 (3 was historically skipped and must not be reused).
PerfIds 500/501/502 are taken; 503/504/505 are free.

## Decision

Add three new variants backed by the pure-Scala engine, running side by side with the
joansala-backed ones:

| key           | id | perfId |
|---------------|----|--------|
| `go9x9Scala`  | 5  | 503    |
| `go13x13Scala`| 6  | 504    |
| `go19x19Scala`| 7  | 505    |

Existing variants (ids 1/2/4) keep the joansala engine untouched. Id 3 stays skipped.
The new case objects join `go.variant.Variant.all`; the top-level wrapper layer
(`src/main/scala/variant/Variant.scala`, `go.variant.Variant.all.map(Go.apply)`) enumerates
the go list generically, so no new cases are expected in the wrapper types.

## Alternatives

- **Replace the variants in place** — no oracle for differential tests or benchmarks;
  any divergence hits live games immediately. Rejected.
- **Feature-flag the engine inside the existing variants** — one variant id with two
  behaviors makes replays and stored FENs ambiguous. Rejected.

## Consequences

- Old and new engines can be benchmarked and differential-tested against each other in-tree.
- Doubles the go variant list until the joansala variants are retired (a later decision).
- Risks carried into the work orders:
  - FEN→variant inference by board size (`go/format/FEN.scala`, `Api.scala` `GoPosition.variant`)
    resolves to the OLD variants; any path inferring variant from a FEN alone routes to the
    old engine. The new engine's position must carry its variant explicitly (see 0002).
  - The PlayStrategy app may assume things about ids/keys; ids 5/6/7 and mixed-case keys
    (`go9x9Scala`) are new territory for app-side wiring.
  - `GoBinaryTest` may encode variant-dependent data; audited before wiring lands.
  - JMH benchmarks historically hang; runs must always pass tight `-wi/-i/-f -to` params
    and filter to a single benchmark class.
