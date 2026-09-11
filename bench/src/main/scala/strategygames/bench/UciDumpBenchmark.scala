package strategygames.bench

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole

import strategygames.{ ActionStrs, GameLogic }
import strategygames.format.{ FEN, GameToUciStrings, UciDump }
import strategygames.variant.Variant

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 5, time = 1)
@Fork(1)
class UciDumpBenchmark {

  @Param(
    Array(
      "backgammon",
      "go",
      "chess",
      "togyzkumalak",
      "fairysf",
      "samurai",
      "abalone",
      "draughts",
      "dameo",
      "loa"
    )
  )
  var family: String = ""

  @Param(Array("short", "medium", "long"))
  var size: String = ""

  var lib: GameLogic          = GameLogic.Chess()
  var variant: Variant        = Variant.libStandard(GameLogic.Chess())
  var initialFen: Option[FEN] = None
  var actionStrs: ActionStrs  = Vector.empty

  @Setup(Level.Trial)
  def setup(): Unit = {
    val fixture = CorpusFixture.load(family, size)
    lib = fixture.lib
    variant = fixture.variant
    initialFen = fixture.initialFen
    actionStrs = fixture.actionStrs
  }

  @Benchmark
  def oldGameStateMoves(bh: Blackhole): Unit = {
    val uciMoves = UciDump(lib, actionStrs, initialFen, variant)
      .fold(err => throw new IllegalStateException(s"UciDump invalid for $family-$size: $err"), identity)
    bh.consume(uciMoves.map(_.mkString(",")).mkString(" "))
  }

  @Benchmark
  def newGameStateMoves(bh: Blackhole): Unit = {
    val moves = GameToUciStrings(lib, actionStrs, initialFen, variant)
      .fold(
        err => throw new IllegalStateException(s"GameToUciStrings invalid for $family-$size: $err"),
        identity
      )
    bh.consume(moves)
  }
}
