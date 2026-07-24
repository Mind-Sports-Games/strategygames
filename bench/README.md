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

`src/main/resources/corpus/<family>-<size>.txt`. Each fixture is a small text
file:

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
codec against the committed corpus files only — it loads each default fixture
(one per family × size), round-trips it through `parse`/`render`, and asserts the
two corrupt-input rejections; it plays no games, so it finishes in seconds. The
differential specs (`GameToUciStringsDifferentialSpec`, `…BackgammonSpec`,
`…ChessSpec`) assert `GameToUciStrings` is byte-identical to `UciDump` over every
default and non-default-variant fixture. The committed fixture files are the
memoization: game generation happens once, offline, when you run the generator
below — never on every test run.

Non-default variants are covered by extra `<family>-<variant>-long` fixtures (all
backgammon, go, togyzkumalak, abalone, and fairysf variants), so a variant that
would break its family's fast path is caught differentially — as GrandAbalone was
(multi-action grouping), which is routed to the delegate.

## Regenerate the corpus

The generator plays random legal games through strategygames with a fixed seed
(`20240724L`) and writes the fixtures. Regeneration with the same seed is
byte-identical, so this is the expensive step (it replays go area-scoring and
backgammon forced-turn games) and is deliberately kept out of the default test
suite.

```
sbt "bench/runMain strategygames.bench.CorpusGenerator"
```

It writes to `bench/src/main/resources/corpus` by default; pass an alternate
output directory as the first argument. Each output line reports the turn count,
ply (action) count, the stop reason (`Ended` = natural game end, `CapReached` =
hit the ply cap, `NoCandidates` = no applyable action), whether `UciDump` accepts
the fixture, and a determinism status versus the file already on disk
(`BYTE_IDENTICAL`, `CHANGED`, or `NEW`).

To verify the committed fixtures are still reproducible without overwriting them:

```
sbt "bench/runMain strategygames.bench.CorpusGenerator check"
```

`check` mode writes nothing, reports the per-fixture status, prints
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
