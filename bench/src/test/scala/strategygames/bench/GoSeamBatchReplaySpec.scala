package strategygames.bench

import org.specs2.mutable.Specification

import strategygames.go.Api
import strategygames.go.format.FEN
import strategygames.go.variant.{ Variant => GoVariant }

class GoSeamBatchReplaySpec extends Specification {

  private val superkoCorpusKey   = "go9x9superko"
  private val pliesBeforeSuperko = 10
  private val forbiddenRecapture = "s@c3"

  private def startFenOf(corpus: GoCorpusGame): FEN =
    corpus.initialFen.getOrElse(corpus.size.variant.initialFen)

  private def actionsOf(corpus: GoCorpusGame): List[String] =
    corpus.actionStrs.toList.flatMap(_.toList)

  private def perPly(variant: GoVariant, fen: FEN, ucis: List[String]): Api.Position =
    Api.positionFromVariantNameAndFEN(variant.key, fen.value).makeMoves(ucis)

  "the go seam batch entries" should {

    "give one position per ply, matching the per ply oracle's fen, on every corpus" in {
      GoBoardSize.all
        .map { size =>
          val corpus   = GoCorpusGame.load(size.key)
          val fen      = startFenOf(corpus)
          val ucis     = actionsOf(corpus)
          val built    = Api.positionsFromVariantStartingFenAndMoves(size.variant, fen, ucis)
          val expected =
            (0 to ucis.size).map(played => perPly(size.variant, fen, ucis.take(played)).fen.value)
          (built.size must beEqualTo(ucis.size + 1)) and
            (built.map(_.fen.value).toList must beEqualTo(expected.toList))
        }
        .reduce(_ and _)
    }

    "inherit every per ply stone map across a capture and a settlement" in {
      val corpus   = GoCorpusGame.load(superkoCorpusKey)
      val fen      = startFenOf(corpus)
      val ucis     = actionsOf(corpus)
      val built    = Api.positionsFromVariantStartingFenAndMoves(corpus.size.variant, fen, ucis)
      val expected =
        (0 to ucis.size).map(played => perPly(corpus.size.variant, fen, ucis.take(played)).pieceMap)
      built.map(_.pieceMap).toList must beEqualTo(expected.toList)
    }

    "fold a whole corpus into the position the per ply oracle reaches" in {
      GoBoardSize.all
        .map { size =>
          val corpus = GoCorpusGame.load(size.key)
          val fen    = startFenOf(corpus)
          val ucis   = actionsOf(corpus)
          val folded = Api.positionFromVariantStartingFenAndMoves(size.variant, fen, ucis)
          val oracle = perPly(size.variant, fen, ucis)
          (folded.fen.value must beEqualTo(oracle.fen.value)) and
            (folded.pieceMap must beEqualTo(oracle.pieceMap)) and
            (folded.gameEnd must beEqualTo(oracle.gameEnd)) and
            (folded.legalActions.toList must beEqualTo(oracle.legalActions.toList))
        }
        .reduce(_ and _)
    }

    "carry the position history that a fen reload cannot, on the superko corpus" in {
      val corpus         = GoCorpusGame.load(superkoCorpusKey)
      val variant        = corpus.size.variant
      val fen            = startFenOf(corpus)
      val ucis           = actionsOf(corpus).take(pliesBeforeSuperko)
      val folded         = Api.positionFromVariantStartingFenAndMoves(variant, fen, ucis)
      val oracle         = perPly(variant, fen, ucis)
      val reloaded       = Api.positionFromVariantNameAndFEN(variant.key, folded.fen.value)
      val recapturePoint = Api.uciToMove(forbiddenRecapture, variant)
      (folded.legalActions.toList must beEqualTo(oracle.legalActions.toList)) and
        (folded.legalActions.toList must not(contain(recapturePoint))) and
        (reloaded.legalActions.toList must contain(recapturePoint)) and
        (folded.makeMoves(List(forbiddenRecapture)) must throwAn[Exception]) and
        (oracle.makeMoves(List(forbiddenRecapture)) must throwAn[Exception])
    }
  }
}
