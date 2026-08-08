package strategygames.bench

import java.util.concurrent.TimeUnit

import scala.compiletime.uninitialized

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole

import strategygames.{ ActionStrs, Game => StratGame, GameLogic, Player, Replay => StratReplay }
import strategygames.format.{ FEN => StratFEN, Uci => StratUci }
import strategygames.variant.{ Variant => StratVariant }
import strategygames.go.{ Board, Game, Pos, Situation }
import strategygames.go.format.{ FEN, Forsyth }
import strategygames.go.variant.{ Variant => GoVariant }

object GoMidGame {

  def fenOf(sizeKey: String): (GoVariant, FEN) = {
    val corpus  = GoCorpusGame.load(sizeKey)
    val variant = corpus.size.variant
    val board   = GoCorpusGame.replay(corpus, variant, corpus.turnsBeforeMidGameDrop).situation.board
    (variant, Forsyth.exportBoardFen(board))
  }

  def boardOf(variant: GoVariant, fen: FEN): Board =
    Forsyth
      .<<@(variant, fen)
      .map(_.board)
      .getOrElse(sys.error(s"unparsable mid-game fen for ${variant.key}: ${fen.value}"))
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
class GoWrapperReplayInput {

  @Param(Array("go9x9", "go13x13", "go19x19"))
  var size: String = ""

  var actionStrs: ActionStrs = uninitialized
  var startPlayer: Player    = uninitialized
  var activePlayer: Player   = uninitialized
  var initialFen: StratFEN   = uninitialized
  var variant: StratVariant  = uninitialized

  @Setup(Level.Trial)
  def setup(): Unit = {
    val corpus = GoCorpusGame.load(size)
    val goFen  = corpus.initialFen.getOrElse(corpus.size.variant.initialFen)
    actionStrs = corpus.actionStrs
    startPlayer = goFen.player.getOrElse(Player.P1)
    activePlayer = corpus.activePlayerAfter(corpus.actionStrs.size)
    initialFen = StratFEN.Go(goFen)
    variant = StratVariant.Go(corpus.size.variant)
  }

  def replayWholeGame(): List[(StratGame, StratUci.WithSan)] =
    StratReplay
      .gameWithUciWhileValid(
        GameLogic.Go(),
        actionStrs,
        startPlayer,
        activePlayer,
        initialFen,
        variant
      )
      ._2
}

@State(Scope.Thread)
class GoMidGameSituation {

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
class GoMidGameFen {

  @Param(Array("go9x9", "go13x13", "go19x19"))
  var size: String = ""

  var variant: GoVariant = uninitialized
  var fen: FEN           = uninitialized

  @Setup(Level.Trial)
  def setup(): Unit = {
    val (loadedVariant, loadedFen) = GoMidGame.fenOf(size)
    variant = loadedVariant
    fen = loadedFen
  }
}

@State(Scope.Thread)
class GoFreshMidGameBoard {

  @Param(Array("go9x9", "go13x13", "go19x19"))
  var size: String = ""

  var variant: GoVariant = uninitialized
  var fen: FEN           = uninitialized
  var board: Board       = uninitialized

  @Setup(Level.Trial)
  def loadCorpus(): Unit = {
    val (loadedVariant, loadedFen) = GoMidGame.fenOf(size)
    variant = loadedVariant
    fen = loadedFen
  }

  @Setup(Level.Invocation)
  def freshBoard(): Unit = board = GoMidGame.boardOf(variant, fen)
}

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1)
@Measurement(iterations = 3, time = 1)
@Fork(1)
class GoRulesBenchmark {

  @Benchmark
  def replay(input: GoReplayInput, bh: Blackhole): Unit =
    bh.consume(input.replayWholeGame().plies)

  @Benchmark
  def replayReadingFinalScore(input: GoReplayInput, bh: Blackhole): Unit =
    bh.consume(input.replayWholeGame().situation.board.areaScore.p1)

  @Benchmark
  def wrapperReplay(input: GoWrapperReplayInput, bh: Blackhole): Unit =
    bh.consume(input.replayWholeGame().size)

  @Benchmark
  def wrapperReplayReadingEveryScore(input: GoWrapperReplayInput, bh: Blackhole): Unit =
    bh.consume(input.replayWholeGame().foldLeft(0) { case (total, (game, _)) =>
      total + game.situation.board.history.score.p1
    })

  @Benchmark
  def applyDrop(input: GoMidGameSituation, bh: Blackhole): Unit =
    bh.consume(input.situation.board.variant.boardAfter(input.situation, input.dropPos))

  @Benchmark
  def validDropsMidGame(input: GoMidGameSituation, bh: Blackhole): Unit =
    bh.consume(input.situation.dropsAsDrops.size)

  @Benchmark
  def dropsByRoleMidGame(input: GoMidGameSituation, bh: Blackhole): Unit =
    bh.consume(input.situation.dropsByRole.map(_.size))

  @Benchmark
  def areaScoreMidGame(state: GoFreshMidGameBoard, bh: Blackhole): Unit =
    bh.consume(state.board.areaScore.p1)

  @Benchmark
  def fenRenderMidGame(state: GoFreshMidGameBoard, bh: Blackhole): Unit =
    bh.consume(Forsyth.exportBoardFen(state.board).value)

  @Benchmark
  def fenParseMidGame(input: GoMidGameFen, bh: Blackhole): Unit =
    bh.consume(GoMidGame.boardOf(input.variant, input.fen).pieces.size)
}
