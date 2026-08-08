package strategygames.bench

import java.util.concurrent.TimeUnit

import scala.compiletime.uninitialized

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole

import strategygames.go.Situation

@State(Scope.Thread)
class GoMidGameSituation {

  @Param(Array("go9x9", "go13x13", "go19x19"))
  var size: String = ""

  var situation: Situation = uninitialized

  @Setup(Level.Trial)
  def setup(): Unit = {
    val corpus  = GoCorpusGame.load(size)
    val variant = corpus.size.variant
    situation = GoCorpusGame.replay(corpus, variant, corpus.turnsBeforeMidGameDrop).situation
  }
}

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1)
@Measurement(iterations = 3, time = 1)
@Fork(1)
class GoMovegenConsumerBenchmark {

  @Benchmark
  def prodValidDropsMidGame(input: GoMidGameSituation, bh: Blackhole): Unit =
    bh.consume(input.situation.dropsAsDrops.size)

  @Benchmark
  def prodDropsByRoleMidGame(input: GoMidGameSituation, bh: Blackhole): Unit =
    bh.consume(input.situation.dropsByRole.map(_.size))
}
