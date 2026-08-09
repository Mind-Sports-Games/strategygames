# Go performance: the deviation log

`lakin/go-idiomatic-refactor` set go's shape. This branch, `lakin/go-idiomatic-perf`, is allowed to
move away from that shape only where a number forces it. Every such move is recorded here, in order,
with the measurement that bought it. A change that costs no idiom is recorded too, and said to cost
none — the record is of what was considered, not only of what was conceded.

Base: `be190b35`, the tip of `lakin/go-idiomatic-refactor` (1,989 tests green).

## The rule

Every deviation from branch one's idiom must be justified by a measurement taken **on this branch**,
must be the **smallest change that moves the number**, and must be **documented at the point of
deviation**. A deviation without a measurement behind it does not land. A measurement that does not
move does not justify a deviation.

Two of the remaining candidates — making `History.Go` non-strict about its score, and replacing
`PieceMap` with an array indexed by `Pos.index` — trade house shape for speed at a size this rule
cannot settle on its own. They go to the owner as explicit decisions, with an ADR, rather than being
decided inside a perf commit.

## How to measure on this branch

These rules were learned on branch one, each at the cost of a wasted round or a false claim.

**Benchmark from a separate git worktree.** Another process compiling in the main tree perturbs JMH
forks. A paired comparison means two worktrees, both built and both green before either is measured,
run back to back in one session on one machine.

**Never trust sbt's exit code for JMH.** A forked JVM that fails with `Could not find or load main
class org.openjdk.jmh.runner.ForkedMain` is dropped by JMH from *both* the console table and the
result JSON, and sbt still exits 0. A run is verified only by counting the results in the JSON and
grepping the log for `ForkedMain`. Both numbers, both sides, every run.

**The JSON is the only record.** sbt's logger truncates the console summary table, so the console is
not evidence of anything.

**`bench` is aggregated by no root task, so it rots silently.** It did not compile for several
commits on branch one and nothing went red. Compile it explicitly.

**No figure may be written before it is measured.** Branch one had one fabricated before/after pair
reach two documents and two commit messages before a reviewer caught it.

**One fork is not a measurement for the mid-game instruments.** `validDropsMidGame` @19x19 produced
fork means spanning 229 to 294 µs on *identical bytecode* during this branch's first paired run. Use
`-f 3` from the start for the mid-game instruments; the replay instruments are stable at `-f 1`.

---

## 1. `Chain.grownFrom` folds its frontier instead of filtering per step — `4403b747`

**Deviation: none.** This is the first change on the branch and it conceded nothing, which is the
reason it went first.

`Chain.Stones.grownFrom` built an intermediate `List` for each dequeued point and then copied it
twice — once onto the queue with `:::`, once into `reached` with `HashSet.concat`. It now carries the
dequeued point's neighbours as a fourth parameter and classifies them one at a time, so the
intermediate list, the copy and the concat all disappear.

What the branch-one idiom asked for, and what this still is:

- **one private method.** `grownFrom` is `private` inside `private case class Stones`; it gained a
  parameter and nothing else gained anything.
- **one `@tailrec` fill.** `src/main/scala/go/Chain.scala` still contains exactly one
  `@annotation.tailrec` and still contains the package's only flood fill. `Variant.emptyRegionsOf`
  still calls it rather than writing a second — the constraint the file exists to hold.
- **no mutable state.** No `var` in `Chain.scala`, before or after. No `while` loop was needed, so
  the branch's preference for a `@tailrec` fold over a mutable loop was not spent.
- **`regionFrom`'s signature untouched.** `Chain.regionFrom(board, origin)(extendsThrough)` is still
  `private[go]` with the same shape (`src/main/scala/go/Chain.scala:18`), and its one caller
  (`src/main/scala/go/variant/Variant.scala:327`) is unchanged.

The enqueue-time bookkeeping the `NOTE` above the method describes is preserved exactly:
`reached + neighbour` happens in the same step as `neighbour :: pending`, so a point that several
frontier points touch is still walked once.

### The measurement

Two git worktrees off `be190b35` — one unmodified, one carrying this change and nothing else —
benchmarked back to back in one session on one machine. There is **no comparison against a stored
table**; every figure below is a before and an after taken minutes apart on the same hardware, which
is what makes them comparable. AMD Ryzen 9 8945HS, OpenJDK 21.0.12+2-nixos, JMH 1.37, `avgt` µs/op ±
99.9% CI, `-wi 5 -w 2s -i 10 -r 2s -f 1 -to 120s -rf json`.

Run verification, both sides: 30 JSON results (10 workloads × 3 sizes), `grep -c ForkedMain` = 0.
JMH dropped nothing.

The headline pair, at 19x19:

| µs/op @ go19x19 | before | after | |
| --- | --- | --- | --- |
| `replay` | 3310.80 ± 44.82 | 2946.98 ± 46.97 | **−11.0%** |
| `areaScoreMidGame` | 65.78 ± 0.59 | 57.79 ± 0.32 | **−12.2%** |

27 of the 30 rows moved down with separation, across all three board sizes. `wrapperReplay`, the
production path, went −9.5% / −9.6% / −7.4% at go9x9 / go13x13 / go19x19; `replay` went −11.5% /
−8.9% / −11.0%.

Three 19x19 rows disagreed with the other twenty-seven and were re-measured at `-f 3` on both
worktrees. None is a regression: `replayReadingFinalScore` was a mid-run JIT step and re-measures at
−7.4%; `validDropsMidGame` was fork luck on the *before* side and re-measures at −4.1%;
`dropsByRoleMidGame`'s apparent −29.9% does not survive, and the honest figure for it is
`validDropsMidGame`'s, since both run the same `validDrops` underneath.

The `List.filter` frame under `grownFrom` — the top named frame in both baseline profiles, at 7.2% of
`replay` and 6.4% of `wrapperReplay` — is absent from both post-change profiles, as is
`HashSet.concat`.

`sbt test`: 1,989 tests, 0 failures, 0 errors. The go suite was run green in the changed worktree
before any measurement was taken.

The full record, including the raw per-fork numbers and the profile tables, is
`.superpowers/sdd/2026-08-08-go-perf/candidate-5-report.md`. That directory is gitignored, so treat
it as working evidence: the figures above are the committed record.

### What this left behind

- **The `NOTE` above `grownFrom` still says something true, in language that no longer fits.**
  `reached` does still grow at enqueue rather than at dequeue, so the claim holds; but the method is
  now a two-phase traversal and the comment describes it in one-phase terms. Nothing was written or
  edited there in the perf commit. Open.
- **`grownFrom` is four parameters and two nested matches**, so its bytecode is larger at a method
  several call sites inline into. No inlining regression appeared on any instrument. It is the first
  thing to check if a later change grows the method again.
- **The next thing in this method is `Pos` boxing, not allocation.** `HashSet.contains` plus
  `HashSet.incl` on `reached` is now 14.4% / 15.3% of RUNNABLE, all of it hashing and comparing a
  boxed `go.Pos` inside one method. A set keyed on `Pos.index` inside `grownFrom` alone may capture
  most of that without the repo-wide `PieceMap` question — and would be a real deviation, so it
  belongs in this document with its own measurement when it is taken.
