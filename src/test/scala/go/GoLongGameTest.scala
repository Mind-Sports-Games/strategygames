package strategygames.go

import org.specs2.matcher.ValidatedMatchers
import org.specs2.mutable.Specification

class GoLongGameTest extends Specification with ValidatedMatchers {

  "A Go 19x19 game with more than 602 total makeMove calls (capacity boundary)" should {
    "not throw ArrayIndexOutOfBoundsException when making 603+ moves via makeMovesNoLegalCheck" in {
      // Construct a deterministic move list causing 604 makeMove calls on the underlying GoGame:
      //   - 300 stone placements (1 makeMove call each) = 300 calls
      //   - 152 "ss:" moves (2 makeMove calls each, same as two passes) = 304 calls
      //   Total = 604 calls, exceeding the default GoGame capacity of 602.
      //
      // GoGame(int boardSize) bytecode reveals: GoGame(int) calls GoGame(602, boardSize),
      // so the default capacity is 602, NOT boardSize.
      //
      // makeMovesNoLegalCheck is used to bypass position.legalMoves.contains(), which
      // has an unrelated AIOOBE in GoGame.attacks for pass (index 361) in certain board
      // states. The capacity fix lives in makeMovesImpl, shared by both paths.
      //
      // uciToMove("ss:", variant) returns passMove, so each "ss:" move calls
      // position.makeMove(passMove) twice, each triggering pushState().
      val stoneMoves = (0 until 300).map(i => Api.moveToUci(i, variant.Go19x19)).toList
      val ssMoves    = List.fill(152)("ss:")
      val allMoves   = stoneMoves ++ ssMoves // 452 items but 604 makeMove calls

      // Before fix: AIOOBE at 603rd makeMove call (GoGame.pushState index 602, array length 602).
      // After fix: ensureCapacity(0 + 452 * 2) = ensureCapacity(904) expands arrays first.
      val result = Api.position(variant.Go19x19).makeMovesNoLegalCheck(allMoves)
      result.fenString.nonEmpty must_== true
    }
  }
}
