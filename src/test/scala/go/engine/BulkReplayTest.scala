package strategygames.go.engine

import org.specs2.mutable.Specification

class BulkReplayTest extends Specification {

  import GoEngineTestSupport._

  private val size = 9

  private val koCapture   = engineMove(size, "b3")
  private val koRecapture = engineMove(size, "c3")
  private val pass        = size * size

  private val koShapeWhiteToPlay: GoState =
    playAll(size, List("b2", "c2", "a3", "d3", "b4", "c4", "c3"))

  private val captureThenBothPass = Array(koCapture, pass, pass)

  private def foldedOracle(start: GoState, moves: Array[Int]): GoState =
    moves.foldLeft(start)((state, move) => state(move))

  private def fieldsOf(state: GoState): List[(String, Any)] = List(
    "positionHash"                 -> state.positionHash,
    "playerTurn"                   -> state.playerTurn,
    "capturesByBlack"              -> state.capturesByBlack,
    "capturesByWhite"              -> state.capturesByWhite,
    "simpleKoMove"                 -> state.simpleKoMove,
    "consecutivePasses"            -> state.consecutivePasses,
    "capturedMovesOnLastPlacement" -> state.capturedMovesOnLastPlacement,
    "occurredPositionHashes"       -> state.occurredPositionHashes,
    "areaScore"                    -> state.areaScore,
    "legalMoves"                   -> state.legalMoves.toList,
    "stones"                       -> (0 until state.passMove).map(state.stoneOwnerAt).toList
  )

  "BulkReplay.replay" should {

    "agree with the immutable engine on every field of the folded state" in {
      val bulk = BulkReplay.replay(koShapeWhiteToPlay, captureThenBothPass)
      fieldsOf(bulk) must beEqualTo(fieldsOf(foldedOracle(koShapeWhiteToPlay, captureThenBothPass)))
    }

    "forbid the superko recapture in the folded state once the simple ko point has lapsed" in {
      val bulk   = BulkReplay.replay(koShapeWhiteToPlay, captureThenBothPass)
      val oracle = foldedOracle(koShapeWhiteToPlay, captureThenBothPass)
      (bulk.simpleKoMove must beNone) and
        (oracle.legalMoves.toList must not(contain(koRecapture))) and
        (bulk.legalMoves.toList must beEqualTo(oracle.legalMoves.toList))
    }

    "reject a superko recreation mid-fold, naming the ply and the legal alternatives" in {
      val oracle = foldedOracle(koShapeWhiteToPlay, captureThenBothPass)
      (oracle(koRecapture) must throwAn[IllegalArgumentException]) and
        (BulkReplay.replay(koShapeWhiteToPlay, captureThenBothPass :+ koRecapture) must
          throwAn[BulkReplay.IllegalMoveAt].like { case illegal: BulkReplay.IllegalMoveAt =>
            (illegal.index must beEqualTo(captureThenBothPass.length)) and
              (illegal.move must beEqualTo(koRecapture)) and
              (illegal.legalMoves.toList must not(contain(koRecapture)))
          })
    }

    "fold in stages exactly as it folds in one pass" in {
      val staged = BulkReplay.replay(BulkReplay.replay(koShapeWhiteToPlay, captureThenBothPass), Array(pass))
      fieldsOf(staged) must beEqualTo(fieldsOf(foldedOracle(koShapeWhiteToPlay, captureThenBothPass :+ pass)))
    }
  }
}
