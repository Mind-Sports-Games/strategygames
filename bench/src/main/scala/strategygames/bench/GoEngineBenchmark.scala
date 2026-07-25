package strategygames.bench

import java.util.concurrent.TimeUnit

import scala.compiletime.uninitialized

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole

import cats.data.Validated

import strategygames.{ ActionStrs, Player }
import strategygames.go.{ Api, Board, Game, Pos, Replay }
import strategygames.go.format.FEN
import strategygames.go.variant.{
  Go13x13,
  Go13x13Scala,
  Go19x19,
  Go19x19Scala,
  Go9x9,
  Go9x9Scala,
  Variant => GoVariant
}

final case class GoBoardSize(
    key: String,
    corpusFamily: String,
    joansalaVariant: GoVariant,
    scalaVariant: GoVariant
)

object GoBoardSize {

  val all: List[GoBoardSize] = List(
    GoBoardSize("go9x9", "go-go9x9", Go9x9, Go9x9Scala),
    GoBoardSize("go13x13", "go-go13x13", Go13x13, Go13x13Scala),
    GoBoardSize("go19x19", "go", Go19x19, Go19x19Scala)
  )

  def named(key: String): GoBoardSize =
    all.find(_.key == key).getOrElse(sys.error(s"unknown go board size: ${key}"))
}

object GoEngine {

  val Joansala  = "joansala"
  val PureScala = "scala"

  def variantFor(engine: String, size: GoBoardSize): GoVariant = engine match {
    case Joansala  => size.joansalaVariant
    case PureScala => size.scalaVariant
    case unknown   => sys.error(s"unknown go engine: ${unknown}")
  }
}

final case class GoCorpusGame(size: GoBoardSize, initialFen: Option[FEN], actionStrs: ActionStrs) {

  private def startPlayer: Player =
    initialFen.getOrElse(size.joansalaVariant.initialFen).player.getOrElse(Player.P1)

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

  @Param(Array("joansala", "scala"))
  var engine: String = ""

  var corpus: GoCorpusGame = uninitialized
  var variant: GoVariant   = uninitialized

  @Setup(Level.Trial)
  def setup(): Unit = {
    corpus = GoCorpusGame.load(size)
    variant = GoEngine.variantFor(engine, corpus.size)
  }

  def replayWholeGame(): Game = GoCorpusGame.replay(corpus, variant, corpus.actionStrs)
}

@State(Scope.Thread)
class GoMidGameBoard {

  @Param(Array("go9x9", "go13x13", "go19x19"))
  var size: String = ""

  @Param(Array("joansala", "scala"))
  var engine: String = ""

  var board: Board   = uninitialized
  var player: Player = Player.P1
  var dropPos: Pos   = uninitialized

  @Setup(Level.Trial)
  def setup(): Unit = {
    val corpus    = GoCorpusGame.load(size)
    val variant   = GoEngine.variantFor(engine, corpus.size)
    val situation = GoCorpusGame.replay(corpus, variant, corpus.turnsBeforeMidGameDrop).situation
    board = situation.board
    player = situation.player
    dropPos = corpus.midGameDropPos
    board.apiPosition.pieceMap
  }
}

@State(Scope.Thread)
class GoMidGamePosition {

  @Param(Array("go9x9", "go13x13", "go19x19"))
  var size: String = ""

  @Param(Array("joansala", "scala"))
  var engine: String = ""

  var variantKey: String     = ""
  var fen: String            = ""
  var position: Api.Position = uninitialized

  @Setup(Level.Trial)
  def setupCorpus(): Unit = {
    val corpus  = GoCorpusGame.load(size)
    val variant = GoEngine.variantFor(engine, corpus.size)
    variantKey = variant.key
    fen = GoCorpusGame
      .replay(corpus, variant, corpus.turnsBeforeMidGameDrop)
      .situation
      .board
      .apiPosition
      .fen
      .value
  }

  // Per invocation, not per trial: `legalDrops` is cached on the position, so a reused one would
  // time a field access. JMH keeps this setup out of the reported score.
  @Setup(Level.Invocation)
  def freshPosition(): Unit =
    position = Api.positionFromVariantNameAndFEN(variantKey, fen)
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
    bh.consume(input.board.afterDrop(input.player, input.dropPos))

  @Benchmark
  def legalDrops(input: GoMidGamePosition, bh: Blackhole): Unit =
    bh.consume(input.position.legalDrops.length)
}
