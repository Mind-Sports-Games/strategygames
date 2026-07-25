package strategygames.bench

import java.util.concurrent.TimeUnit

import scala.compiletime.uninitialized

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole

import strategygames.go.Api
import strategygames.go.engine.{ GoFen, GoGame }
import strategygames.go.format.FEN
import strategygames.go.variant.{ Variant => GoVariant }

@State(Scope.Thread)
class GoLayerInput {

  @Param(Array("go9x9", "go13x13", "go19x19"))
  var size: String = ""

  var corpus: GoCorpusGame    = uninitialized
  var variant: GoVariant      = uninitialized
  var initialFenValue: String = ""
  var ucis: List[String]      = Nil
  var engineMoves: Array[Int] = uninitialized
  var engineStart: GoGame     = uninitialized
  var midGameFen: String      = ""

  @Setup(Level.Trial)
  def setup(): Unit = {
    corpus = GoCorpusGame.load(size)
    variant = corpus.size.variant
    val startFen = corpus.initialFen.getOrElse(variant.initialFen)
    initialFenValue = startFen.value
    ucis = corpus.actionStrs.flatten.toList
    if (ucis.exists(_.startsWith("ss:"))) sys.error(s"unexpected ss action in ${size} corpus")
    engineMoves = ucis.map(Api.uciToMove(_, variant)).toArray
    engineStart = GoFen.parse(initialFenValue) match {
      case Right(game) => game
      case Left(error) => sys.error(s"unparsable ${size} initial fen (${error}): ${initialFenValue}")
    }
    midGameFen = Api
      .positionFromVariantStartingFenAndMoves(
        variant,
        FEN(initialFenValue),
        corpus.turnsBeforeMidGameDrop.flatten.toList
      )
      .fen
      .value
  }
}

@State(Scope.Thread)
class GoFreshMidGameState {

  @Param(Array("go9x9", "go13x13", "go19x19"))
  var size: String = ""

  var variantKey: String = ""
  var midGameFen: String = ""
  var game: GoGame       = uninitialized

  @Setup(Level.Trial)
  def setupCorpus(): Unit = {
    val corpus   = GoCorpusGame.load(size)
    val variant  = corpus.size.variant
    variantKey = variant.key
    val startFen = corpus.initialFen.getOrElse(variant.initialFen)
    midGameFen = Api
      .positionFromVariantStartingFenAndMoves(
        variant,
        startFen,
        corpus.turnsBeforeMidGameDrop.flatten.toList
      )
      .fen
      .value
  }

  @Setup(Level.Invocation)
  def freshGame(): Unit =
    game = GoFen.parse(midGameFen) match {
      case Right(parsed) => parsed
      case Left(error)   => sys.error(s"unparsable ${size} mid-game fen (${error}): ${midGameFen}")
    }
}

@State(Scope.Thread)
class GoFreshMidGamePosition {

  @Param(Array("go9x9", "go13x13", "go19x19"))
  var size: String = ""

  var variantKey: String     = ""
  var midGameFen: String     = ""
  var position: Api.Position = uninitialized

  @Setup(Level.Trial)
  def setupCorpus(): Unit = {
    val corpus   = GoCorpusGame.load(size)
    val variant  = corpus.size.variant
    variantKey = variant.key
    val startFen = corpus.initialFen.getOrElse(variant.initialFen)
    midGameFen = Api
      .positionFromVariantStartingFenAndMoves(
        variant,
        startFen,
        corpus.turnsBeforeMidGameDrop.flatten.toList
      )
      .fen
      .value
  }

  @Setup(Level.Invocation)
  def freshPosition(): Unit =
    position = Api.positionFromVariantNameAndFEN(variantKey, midGameFen)
}

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1)
@Measurement(iterations = 3, time = 1)
@Fork(1)
class GoLayerBenchmark {

  @Benchmark
  def engineReplay(input: GoLayerInput, bh: Blackhole): Unit = {
    var game  = input.engineStart
    val moves = input.engineMoves
    var i     = 0
    while (i < moves.length) {
      game = game.play(moves(i))
      i += 1
    }
    bh.consume(game.state.positionHash)
  }

  @Benchmark
  def seamBatchReplay(input: GoLayerInput, bh: Blackhole): Unit =
    bh.consume(
      Api
        .positionFromVariantStartingFenAndMoves(input.variant, FEN(input.initialFenValue), input.ucis)
        .turn
    )

  @Benchmark
  def seamPerPlyReplay(input: GoLayerInput, bh: Blackhole): Unit = {
    var position = Api.positionFromVariantNameAndFEN(input.variant.key, input.initialFenValue)
    var rest     = input.ucis
    while (rest.nonEmpty) {
      position = position.makeMoves(List(rest.head))
      rest = rest.tail
    }
    bh.consume(position.turn)
  }

  @Benchmark
  def seamPerPlyPieceMapReplay(input: GoLayerInput, bh: Blackhole): Unit = {
    var position = Api.positionFromVariantNameAndFEN(input.variant.key, input.initialFenValue)
    var rest     = input.ucis
    while (rest.nonEmpty) {
      position = position.makeMoves(List(rest.head))
      bh.consume(position.pieceMap.size)
      rest = rest.tail
    }
    bh.consume(position.turn)
  }

  @Benchmark
  def prodReplay(input: GoLayerInput, bh: Blackhole): Unit =
    bh.consume(GoCorpusGame.replay(input.corpus, input.variant, input.corpus.actionStrs).plies)

  @Benchmark
  def areaScoreMidGame(state: GoFreshMidGameState, bh: Blackhole): Unit =
    bh.consume(state.game.state.areaScore.black)

  @Benchmark
  def engineLegalDropsMidGame(state: GoFreshMidGameState, bh: Blackhole): Unit =
    bh.consume(state.game.state.legalDrops.length)

  @Benchmark
  def pieceMapFullScanMidGame(state: GoFreshMidGamePosition, bh: Blackhole): Unit =
    bh.consume(state.position.pieceMap.size)

  @Benchmark
  def fenParseMidGame(input: GoLayerInput, bh: Blackhole): Unit =
    bh.consume(GoFen.parse(input.midGameFen))

  @Benchmark
  def fenRenderMidGame(state: GoFreshMidGameState, bh: Blackhole): Unit =
    bh.consume(GoFen.render(state.game))
}
