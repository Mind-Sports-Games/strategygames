# strategygames benchmarks (`bench`)

A non-published sbt subproject holding JMH benchmarks and the deterministic
corpus generator that feeds them. It `dependsOn` the root `strategygames`
project and enables `JmhPlugin`. Nothing here is part of the published
`org.playstrategy %% strategygames` artifact — `sbt publishLocal` publishes only
`strategygames`.

## What it measures

`UciDumpBenchmark` reproduces lila's hot path in `BotJsonView.gameState`:
call `strategygames.format.UciDump` over a game's `actionStrs` + initial FEN,
then join the result into the single `moves` string
(`uciMoves.map(_.mkString(",")).mkString(" ")`), handing the String to a JMH
`Blackhole`. It is parametrized by `family` (backgammon, go, chess, togyzkumalak,
fairysf, samurai, abalone, draughts, dameo, loa) and `size` (short, medium, long),
loading one on-disk corpus fixture per combination in `@Setup`.

It carries two `@Benchmark` methods over the same `@Setup` fields —
`oldGameStateMoves` (`UciDump` + join) and `newGameStateMoves` (`GameToUciStrings`)
— so a single JMH run emits old-vs-new side by side across every fixture. See
`../../lila/docs/uci-perf/SUMMARY.md` for the full old-vs-new results and which
families are fast-pathed vs delegated.

## Corpus fixtures

`src/main/resources/corpus/<family>-<size>.txt`. The fixtures are **not committed**
— that directory is gitignored, and any fixture a test or benchmark asks for is
generated on first use and reused from then on (see "Regenerate the corpus" for
the seed and the determinism guarantee). Each fixture is a small text file:

```
family=<family key>
gameLogic=<GameLogic id>
variant=<variant key>
initialFen=<FEN value, empty when absent>
turns=<number of turns that follow>
<turn 0 actions, comma-joined>
<turn 1 actions, comma-joined>
...
```

Turns map to the outer `Seq` of `ActionStrs = Seq[Seq[String]]`; the comma-joined
actions on each line map to the inner `Seq`. This is exactly the encoding
`BotJsonView.gameState` produces for the `moves` string, so a fixture is trivial
to eyeball.

The codec treats an empty turn line as zero actions: both `Seq()` and `Seq("")`
render to `""` and parse back to an empty turn. This collision is harmless
because UCI action strings are never empty, so no real fixture depends on the
distinction.

## Tests

`sbt "bench/test"` runs several specs. `CorpusFixtureSpec` validates the fixture
codec — it loads each default fixture (one per family × size), round-trips it
through `parse`/`render`, and asserts the two corrupt-input rejections. The
differential specs (`GameToUciStringsDifferentialSpec`, `…BackgammonSpec`,
`…ChessSpec`) assert `GameToUciStrings` is byte-identical to `UciDump` over every
default and non-default-variant fixture.

The on-disk fixture files are the memoization: the first run on a fresh checkout
plays every game it needs and writes the files (~50s, one `corpus: generated …`
line per fixture), and every run after that reuses them (~4s, no generation).
`CorpusFixture.load` is the only entry point that reads a fixture, and it is what
generates the missing one — so deleting a fixture file is all it takes to force
that one to be replayed.

Non-default variants are covered by extra `<family>-<variant>-long` fixtures (all
backgammon, go, togyzkumalak, abalone, and fairysf variants), so a variant that
would break its family's fast path is caught differentially — as GrandAbalone was
(multi-action grouping), which is routed to the delegate.

## Regenerate the corpus

The generator plays random legal games through strategygames with a fixed seed
(`20240724L`) and writes the fixtures. Regeneration with the same seed is
byte-identical, which is what makes generate-on-first-use safe: every checkout
produces the same corpus. Running it explicitly rewrites all fixtures in one go,
including any the test suite has not asked for yet:

```
sbt "bench/runMain strategygames.bench.CorpusGenerator"
```

It writes to `bench/src/main/resources/corpus` by default; pass an alternate
output directory as the first argument. Each output line reports the turn count,
ply (action) count, the stop reason (`Ended` = natural game end, `CapReached` =
hit the ply cap, `NoCandidates` = no applyable action), whether `UciDump` accepts
the fixture, and a determinism status versus the file already on disk
(`BYTE_IDENTICAL`, `CHANGED`, or `NEW`).

To verify the fixtures on disk are still reproducible without overwriting them:

```
sbt "bench/runMain strategygames.bench.CorpusGenerator check"
```

`check` mode writes nothing, reports the per-fixture status (a fixture not yet
generated on this machine reports `NEW`, which counts as drift), prints
`DETERMINISM: ALL_BYTE_IDENTICAL` or `DETERMINISM: CHANGES_DETECTED`, and exits
non-zero on any drift.

## Run the JMH suite

Full suite (all families × sizes, default JMH iteration settings):

```
sbt "bench/Jmh/run .*UciDumpBenchmark.*"
```

Restrict parameters (e.g. a quick check):

```
sbt "bench/Jmh/run -i 1 -wi 1 -f 1 -r 1s -w 1s -p family=backgammon,go -p size=short,long .*UciDumpBenchmark.*"
```

## Go benchmarks

`GoRulesBenchmark` times the go rules — `go/variant/Variant.scala`, `go/Chain.scala`
and `go/Board.scala` — on the canonical variants (`go9x9`, `go13x13`, `go19x19`)
over the `go-go9x9-long`, `go-go13x13-long` and `go-long` corpus fixtures, which
are generated on first use like every other fixture here. It reports absolute
timings per board size; the recorded baselines are in
[docs/go-speed-results.md](../docs/go-speed-results.md). `GoCorpus.scala` holds the
fixture loading the benchmarks share.

| Benchmark | Call path | Fixture level |
|---|---|---|
| `replay` | `go.Replay.gameFromUciStrings` over the whole fixture, score never read | `Level.Trial` |
| `replayReadingFinalScore` | the same, then one read of the final `Board.areaScore` | `Level.Trial` |
| `wrapperReplay` | `strategygames.Replay.gameWithUciWhileValid`, one wrapper `Game` per ply, score never read | `Level.Trial` |
| `wrapperReplayReadingEveryScore` | the same, reading `history.score` on every ply | `Level.Trial` |
| `applyDrop` | `Variant.boardAfter` on a mid-game situation | `Level.Trial` |
| `validDropsMidGame` | `Situation.dropsAsDrops` → `Variant.validDrops` | `Level.Trial` |
| `dropsByRoleMidGame` | `Situation.dropsByRole` | `Level.Trial` |
| `areaScoreMidGame` | `Board.areaScore` | `Level.Invocation` |
| `fenParseMidGame` | `Forsyth.<<@` | `Level.Trial` |
| `fenRenderMidGame` | `Forsyth.exportBoardFen` | `Level.Invocation` |

The two `Level.Invocation` workloads read `Board`'s derived `lazy val`s —
`areaScore` and, under it, `playerPiecesOnBoardCount` — so a `Level.Trial` board
would memoise them after the first invocation and the benchmark would under-report.
Each invocation gets a board freshly parsed from the same mid-game FEN, which is
also what production sees: `boardAfter` builds a new `Board` every placement. JMH
excludes fixture time from the reported score.

The four replay workloads exist because the two paths no longer cost the same. The
go-package path returns one `Game` and computes the area score only if something
reads it; the wrapper path materialises one `strategygames.Game` per ply, and
reading any history field on one of those forces that ply's score. Quote whichever
matches the consumer you care about, and say which one it is.

Quick go-only run (roughly four minutes, wide error bars — use for a smoke check):

```
sbt "bench/Jmh/run -wi 3 -i 3 -f 1 -to 60s strategygames.bench.GoRulesBenchmark"
```

Checkpoint baseline (roughly twenty minutes, error bars under 2%):

```
sbt "bench/Jmh/run -wi 5 -w 2s -i 10 -r 2s -f 1 -to 120s \
  -rf json -rff go-jmh.json strategygames.bench.GoRulesBenchmark"
```

Filter to one workload or one size with the regex and `-p`:

```
sbt "bench/Jmh/run -wi 5 -w 2s -i 10 -r 2s -f 1 -to 120s \
  -p size=go19x19 GoRulesBenchmark.validDropsMidGame"
```

Never run `bench/Jmh/run` without iteration and fork limits: the default settings
(5 forks × 5 warmup × 5 measurement over every benchmark in the project) take
hours.

Run `bench/clean` first after renaming or deleting a benchmark or `@State` class.
The generated JMH sources are not invalidated by the rename and the run fails with
`ClassNotFoundException` before it measures anything.

### Benchmark from a separate worktree, and check the run for completeness

Two traps, both of which quietly produce a plausible table rather than an error.

**A second sbt in the same working directory corrupts a JMH run.** JMH forks a JVM
per configuration; a concurrent `sbt test` rewriting `target/` and `bench/target/`
underneath it makes those forks fail. Give the run its own checkout:

```
git worktree add /tmp/bench-run HEAD
cd /tmp/bench-run && sbt "bench/Jmh/run ..."
git worktree remove --force /tmp/bench-run
```

**`sbt` exits 0 even when JMH dropped configurations.** When a fork fails the log
carries `Could not find or load main class org.openjdk.jmh.runner.ForkedMain`, JMH
omits those rows from both the console table and the `-rff` JSON, and the build
still reports `[success]`. The console table is truncated by sbt's logger too, so
it is not a reliable record either. Verify the run rather than trusting its exit
code — count the results in the JSON and grep the log:

```
python3 -c "import json;print(len(json.load(open('bench/go-jmh.json'))))"   # expect benchmarks x sizes
grep -c ForkedMain run.log                                                  # expect 0
```

`GoSmokeTiming` answers the same question without JMH: median wall-clock over a
few rounds of full-game replay and legal-drop generation, printed as a table of
absolute ns/op and appended to a CSV. It finishes in seconds, so it suits a quick
check between edits; `GoRulesBenchmark` remains the measure of record. Its rounds
are too few to reach steady state — expect it to read high, by up to 3x on the
smaller boards.

```
sbt "bench/runMain strategygames.bench.GoSmokeTiming /path/to/results.csv"
```

The output path can also come from `-Dsmoke.output` (which wins over the argument);
without either it defaults to `bench/target/go-smoke-results.csv`.

## Allocation profiling

Add the GC profiler to report bytes allocated per operation:

```
sbt "bench/Jmh/run -prof gc .*UciDumpBenchmark.*"
```

## CPU flamegraphs (async-profiler)

JMH's `-prof async:...` uses `;`-separated options, which collide with sbt's own
command separator. Attach async-profiler as a JVM agent instead (comma-separated,
no `;`), pointing `libPath` at the devenv-provided library:

```
LIB=$(dirname $(dirname $(readlink -f $(command -v async-profiler))))/lib/libasyncProfiler.so
sbt "bench/Jmh/run -f 1 -wi 1 -i 3 -p family=go -p size=long \
  -jvmArgsAppend -agentpath:$LIB=start,event=itimer,file=flame-go-long-old.html \
  UciDumpBenchmark.oldGameStateMoves"
```

`event=itimer` avoids needing `perf_event` permissions; the `.html` extension yields
a flamegraph.

## Toolchain

Run everything through the devenv shell so sbt uses OpenJDK 21 (the project's pinned
sbt 1.10.1 honours `project/build.properties`):

```
cd ../lila-dev && devenv shell -- bash -c 'cd ../strategygames && sbt "bench/test"'
```

Do not mix a system sbt running on a newer JDK with the devenv JDK 21: classes compiled
by the newer JDK carry a higher bytecode version and fail to load under JDK 21
(`UnsupportedClassVersionError`). If that happens, `sbt clean bench/clean` and recompile
under the devenv toolchain.
