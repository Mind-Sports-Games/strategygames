package strategygames.bench

import org.specs2.mutable.Specification

import strategygames.go.Api
import strategygames.go.engine.{ BulkReplay, GoFen, GoGame, GoState }

class GoBulkReplayDifferentialSpec extends Specification {

  private val superkoCorpusKey = "go9x9superko"
  private val renderingKomi    = 5.5

  "the go bulk replay fold" should {

    "agree with the immutable engine over every prefix of every committed go corpus" in {
      GoBoardSize.all.flatMap(size => prefixMismatches(size.key)) must beEmpty
    }

    "carry a corpus whose replay reaches a point forbidden only by the position history" in {
      pointsForbiddenOnlyByHistory(superkoCorpusKey) must not(beEmpty)
    }
  }

  private def startStateOf(corpus: GoCorpusGame): GoState = {
    val fen = corpus.initialFen.getOrElse(corpus.size.variant.initialFen)
    GoFen.parse(fen.value) match {
      case Right(game) => game.state
      case Left(error) => sys.error(s"unparsable ${corpus.size.key} initial fen (${error}): ${fen.value}")
    }
  }

  private def engineMovesOf(corpus: GoCorpusGame): Array[Int] = {
    val ucis               = corpus.actionStrs.toList.flatMap(_.toList)
    val (played, settling) = ucis.span((uci: String) => !uci.startsWith("ss:"))
    if (settling.size > 1)
      sys.error(s"${corpus.size.key} corpus settles dead stones before its final action")
    played.map(Api.uciToMove(_, corpus.size.variant)).toArray
  }

  private def fieldsOf(state: GoState): List[(String, Any)] = List(
    "positionHash"                 -> state.positionHash,
    "playerTurn"                   -> state.playerTurn,
    "capturesByBlack"              -> state.capturesByBlack,
    "capturesByWhite"              -> state.capturesByWhite,
    "simpleKoMove"                 -> state.simpleKoMove,
    "consecutivePasses"            -> state.consecutivePasses,
    "capturedMovesOnLastPlacement" -> state.capturedMovesOnLastPlacement,
    "areaScore"                    -> state.areaScore,
    "legalMoves"                   -> state.legalMoves.toList,
    "stones"                       -> (0 until state.passMove).map(state.stoneOwnerAt).toList,
    "fen"                          -> GoFen.render(GoGame(state, renderingKomi, 0, false))
  )

  private def prefixMismatches(sizeKey: String): List[String] = {
    val corpus     = GoCorpusGame.load(sizeKey)
    val start      = startStateOf(corpus)
    val moves      = engineMovesOf(corpus)
    val mismatches = List.newBuilder[String]
    var oracle     = start
    var prefix     = 0
    while (prefix <= moves.length) {
      if (prefix > 0) oracle = oracle(moves(prefix - 1))
      val bulk = BulkReplay.replay(start, java.util.Arrays.copyOf(moves, prefix))
      fieldsOf(oracle).zip(fieldsOf(bulk)).foreach { case ((field, expected), (_, found)) =>
        if (expected != found) mismatches += s"$sizeKey prefix=$prefix $field: oracle=$expected bulk=$found"
      }
      prefix += 1
    }
    mismatches.result()
  }

  private def pointsForbiddenOnlyByHistory(sizeKey: String): List[String] = {
    val corpus    = GoCorpusGame.load(sizeKey)
    val moves     = engineMovesOf(corpus)
    val forbidden = List.newBuilder[String]
    var state     = startStateOf(corpus)
    var prefix    = 0
    while (prefix <= moves.length) {
      if (prefix > 0) state = state(moves(prefix - 1))
      val readmitted = withoutPositionHistory(state).legalMoves.toSet -- state.legalMoves.toSet
      if (readmitted.nonEmpty)
        forbidden += s"$sizeKey prefix=$prefix forbids ${readmitted.toList.sorted.mkString(",")}"
      prefix += 1
    }
    forbidden.result()
  }

  private def withoutPositionHistory(state: GoState): GoState =
    GoState.fromStoneOwners(
      state.size,
      state.stoneOwnerAt,
      state.playerTurn,
      state.capturesByBlack,
      state.capturesByWhite,
      state.simpleKoMove,
      state.consecutivePasses
    )
}
