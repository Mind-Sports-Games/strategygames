package strategygames.go.engine

import org.specs2.mutable.Specification

import strategygames.go.Api
import strategygames.go.variant.{ Go13x13, Go19x19, Go9x9 }

class UpstreamJoansalaPortedTest extends Specification {

  private def playEngineMoves(size: Int, moves: List[Int]): GoState =
    moves.foldLeft(GoState.initial(size))((state, move) => state(move))

  private def parsedGame(fen: String): GoGame =
    GoFen.parse(fen).fold(error => sys.error(s"unparsable go fen: $error"), identity)

  private val pocket = "[SSSSSSSSSSssssssssss]"

  "starting position" should {
    "give black the first turn" in {
      val state = GoState.initial(19)
      (state.playerTurn === GoState.BlackPlayer) and (state.turn === "b")
    }
    "keep the board size it was created with" in {
      GoState.initial(13).size === 13
    }
    "offer one drop per intersection plus a pass" in {
      forall(List(9, 13, 19)) { size =>
        GoState.initial(size).legalMoves.length === size * size + 1
      }
    }
  }

  "a board diagram" should {
    "take its size from the number of rows" in {
      forall(List(9, 13, 19)) { size =>
        val emptyRows = List.fill(size)(size.toString).mkString("/")
        parsedGame(s"$emptyRows$pocket w - 0 0 0 0 65 0 1").state.size === size
      }
    }
    "place the stones of a 9x9 diagram" in {
      val game  = parsedGame(
        s"9/9/SS3ss2/9/SSSSSSSSS/9/1S1s1SS2/sssssssss/9$pocket w - 0 0 0 0 65 0 1"
      )
      val state = game.state
      (state.size === 9) and
        (state.stoneOwnerAt(9 * 6 + 0) === GoState.BlackPlayer) and
        (state.stoneOwnerAt(9 * 6 + 1) === GoState.BlackPlayer) and
        (state.stoneOwnerAt(9 * 6 + 5) === GoState.WhitePlayer) and
        (state.stoneOwnerAt(9 * 6 + 6) === GoState.WhitePlayer) and
        (state.stoneOwnerAt(9 * 1 + 0) === GoState.WhitePlayer) and
        (state.stoneOwnerAt(9 * 0 + 0) === GoState.NoOwner)
    }
    "reject a row whose runs overflow the board (rules-correct, diverges from joansala)" in {
      val overflowingRows = List.fill(19)("199").mkString("/")
      GoFen.parse(s"$overflowingRows$pocket b - 0 0 0 0 65 0 1") must beLeft(
        GoFenError.MalformedBoardRow("199", 19, 199)
      )
    }
    "read multi digit empty runs and re-emit them (rules-correct, diverges from joansala)" in {
      val rows =
        List.fill(11)("19") ++ List("7S11") ++ List.fill(3)("19") ++
          List("2s16", "19", "11S7", "4S11s2")
      val fen  = s"${rows.mkString("/")}$pocket b - 30 85 0 0 65 0 1"
      val game = parsedGame(fen)
      (game.state.size === 19) and
        (game.state.stoneOwnerAt(19 * 7 + 7) === GoState.BlackPlayer) and
        (game.state.stoneOwnerAt(19 * 3 + 2) === GoState.WhitePlayer) and
        (game.state.stoneOwnerAt(19 * 1 + 11) === GoState.BlackPlayer) and
        (game.state.stoneOwnerAt(19 * 0 + 4) === GoState.BlackPlayer) and
        (game.state.stoneOwnerAt(19 * 0 + 16) === GoState.WhitePlayer) and
        (GoFen.render(game) === fen)
    }
  }

  "an engine move index" should {
    "round trip through algebraic coordinates on every board size" in {
      forall(List((35, Go9x9), (120, Go13x13), (135, Go19x19))) { case (move, variant) =>
        Api.uciToMove(Api.moveToUci(move, variant), variant) === move
      }
    }
  }

  "area scoring of a settled 19x19 position" should {
    val rows =
      List("s1" * 9 + "s", "s" * 19, "1" + "S" * 18) ++
        List.fill(10)("S" * 19) ++
        List("S1" * 9 + "S", "S" * 19, "S1" * 9 + "S", "S" * 19, "S" * 19, "19")
    val game = parsedGame(s"${rows.mkString("/")}$pocket b - 3220 445 0 0 65 0 1")
    "count stones plus enclosed territory for each player" in {
      (game.areaScore === AreaScore(322, 38)) and
        (game.p1Score === 322.0) and
        (game.p2Score === 44.5)
    }
    "report the score difference in tenths" in {
      game.gameScore === 2775
    }
  }

  "a capture" should {
    "free the captured intersections for the next drop" in {
      val state = playEngineMoves(9, List(40, 39, 49, 41, 72, 31, 30, 48, 32, 50, 63, 58))
      (state.capturesByWhite === 2) and
        (state.stoneOwnerAt(40) === GoState.NoOwner) and
        (state.stoneOwnerAt(49) === GoState.NoOwner) and
        (state.isLegal(49) === true) and
        (state.legalMoves.length === 72)
    }
  }

  "a simple ko" should {
    "forbid the immediate recapture" in {
      val state = playEngineMoves(9, List(41, 40, 49, 48, 39, 50, 72, 58, 31, 80, 49))
      (state.simpleKoMove === Some(40)) and
        (state.isLegal(40) === false) and
        (state.legalMoves.length === 72)
    }
    "be cleared by a pass (upstream issue 489)" in {
      val state = playEngineMoves(19, List(2, 59, 20, 39, 22, 41, 40, 21) :+ (19 * 19))
      (state.simpleKoMove === None) and
        (state.isLegal(40) === true) and
        (state.legalMoves.length === 355)
    }
    "not forbid a recapture of more than one stone (upstream issue 490)" in {
      val beforeRecapture = playEngineMoves(9, List(40, 49, 48, 58, 57, 76, 67, 68, 59, 66, 50, 81))
      val recapture       = beforeRecapture(67)
      (recapture.capturesByBlack === 2) and
        (recapture.stoneOwnerAt(49) === GoState.NoOwner) and
        (recapture.stoneOwnerAt(58) === GoState.NoOwner) and
        (recapture.simpleKoMove === None)
    }
    "forbid the capture that recreates an earlier position (rules-correct, diverges from joansala)" in {
      val state = playEngineMoves(9, List(40, 49, 48, 58, 57, 76, 67, 68, 59, 66, 50, 81, 67, 49, 81))
      (state.playerTurn === GoState.WhitePlayer) and
        (state.simpleKoMove === None) and
        (state.isLegal(58) === false) and
        (state.legalMoves.toList must not(contain(58))) and
        (state(58) must throwAn[IllegalArgumentException]) and
        (state.legalMoves.length === 71)
    }
  }

  "a capture cycle" should {
    val beforeReturn = playEngineMoves(9, List(80, 71, 79, 70, 69, 62, 77, 78, 79, 53, 80, 78))
    "forbid the returning capture (rules-correct, diverges from joansala)" in {
      (beforeReturn.isLegal(79) === false) and
        (beforeReturn.legalMoves.toList must not(contain(79))) and
        (beforeReturn(79) must throwAn[IllegalArgumentException])
    }
    "still allow a pass, which never repeats a position" in {
      val passed = beforeReturn(beforeReturn.passMove)
      (passed.consecutivePasses === 1) and
        (passed.positionHash === beforeReturn.positionHash) and
        (passed.isLegal(passed.passMove) === true)
    }
  }

  "playing on from a state" should {
    val snapshot = playEngineMoves(9, List(41, 40, 49))
    val advanced = List(48, 39, 50).foldLeft(snapshot)((state, move) => state(move))
    "leave the earlier state untouched" in {
      (snapshot.stoneOwnerAt(48) === GoState.NoOwner) and
        (advanced.stoneOwnerAt(48) === GoState.WhitePlayer) and
        (snapshot.positionHash !== advanced.positionHash) and
        (snapshot.playerTurn !== advanced.playerTurn) and
        (snapshot.isLegal(48) !== advanced.isLegal(48)) and
        (snapshot.areaScore !== advanced.areaScore)
    }
  }
}
