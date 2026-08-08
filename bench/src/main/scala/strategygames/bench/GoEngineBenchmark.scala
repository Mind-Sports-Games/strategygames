package strategygames.bench

import java.util.concurrent.TimeUnit

import scala.compiletime.uninitialized

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole

import cats.data.Validated

import strategygames.{ ActionStrs, Player }
import strategygames.go.{ Game, Pos, Replay, Situation }
import strategygames.go.format.FEN
import strategygames.go.variant.{ Go13x13, Go19x19, Go9x9, Variant => GoVariant }

final case class GoBoardSize(
    key: String,
    corpusFamily: String,
    variant: GoVariant
)

object GoBoardSize {

  val all: List[GoBoardSize] = List(
    GoBoardSize("go9x9", "go-go9x9", Go9x9),
    GoBoardSize("go9x9superko", "go-go9x9-superko", Go9x9),
    GoBoardSize("go13x13", "go-go13x13", Go13x13),
    GoBoardSize("go19x19", "go", Go19x19)
  )

  def named(key: String): GoBoardSize =
    all.find(_.key == key).getOrElse(sys.error(s"unknown go board size: ${key}"))
}

final case class GoCorpusGame(size: GoBoardSize, initialFen: Option[FEN], actionStrs: ActionStrs) {

  private def startPlayer: Player =
    initialFen.getOrElse(size.variant.initialFen).player.getOrElse(Player.P1)

  def activePlayerAfter(turns: Int): Player =
    Player.fromTurnCount(turns + startPlayer.fold(0, 1))

  val midGameDropTurn: Int = {
    val fromHalfway = actionStrs.indexWhere(isSingleDrop, actionStrs.size / 2)
    if (fromHalfway < 0) sys.error(s"no single-drop turn in second half of ${size.key} corpus")
    else fromHalfway
  }

  def turnsBeforeMidGameDrop: ActionStrs = actionStrs.take(midGameDropTurn)

  def midGameDropPos: Pos = {
    val uci = actionStrs(midGameDropTurn).head
    Pos.fromKey(uci.drop(2)).getOrElse(sys.error(s"unparsable drop ${uci} in ${size.key} corpus"))
  }

  private def isSingleDrop(turn: Seq[String]): Boolean =
    turn.size == 1 && turn.head.length > 2 && turn.head.charAt(1) == '@'
}

object GoCorpusGame {

  val corpusSize = "long"

  def load(sizeKey: String): GoCorpusGame = {
    val size    = GoBoardSize.named(sizeKey)
    val fixture = CorpusFixture.load(size.corpusFamily, corpusSize)
    GoCorpusGame(size, fixture.initialFen.map(_.toGo), fixture.actionStrs)
  }

  def replay(corpus: GoCorpusGame, variant: GoVariant, turns: ActionStrs): Game =
    Replay.gameFromUciStrings(turns, corpus.activePlayerAfter(turns.size), corpus.initialFen, variant) match {
      case Validated.Valid(game)    => game
      case Validated.Invalid(error) => sys.error(s"go replay failed for ${variant.key}: ${error}")
    }
}

@State(Scope.Thread)
class GoReplayInput {

  @Param(Array("go9x9", "go13x13", "go19x19"))
  var size: String = ""

  var corpus: GoCorpusGame = uninitialized
  var variant: GoVariant   = uninitialized

  @Setup(Level.Trial)
  def setup(): Unit = {
    corpus = GoCorpusGame.load(size)
    variant = corpus.size.variant
  }

  def replayWholeGame(): Game = GoCorpusGame.replay(corpus, variant, corpus.actionStrs)
}

@State(Scope.Thread)
class GoMidGameBoard {

  @Param(Array("go9x9", "go13x13", "go19x19"))
  var size: String = ""

  var situation: Situation = uninitialized
  var dropPos: Pos         = uninitialized

  @Setup(Level.Trial)
  def setup(): Unit = {
    val corpus  = GoCorpusGame.load(size)
    val variant = corpus.size.variant
    situation = GoCorpusGame.replay(corpus, variant, corpus.turnsBeforeMidGameDrop).situation
    dropPos = corpus.midGameDropPos
  }
}

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1)
@Measurement(iterations = 3, time = 1)
@Fork(1)
class GoEngineBenchmark {

  @Benchmark
  def replay(input: GoReplayInput, bh: Blackhole): Unit =
    bh.consume(input.replayWholeGame().plies)

  @Benchmark
  def applyDrop(input: GoMidGameBoard, bh: Blackhole): Unit =
    bh.consume(input.situation.board.variant.boardAfter(input.situation, input.dropPos))
}
