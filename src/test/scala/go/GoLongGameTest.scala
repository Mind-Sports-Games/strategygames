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

  "A Go 19x19 game with all 361 positions played (P1 from index 0, P2 from last index)" should {
    "still have legal moves available after filling the board" in {
      // 19x19 board: 361 positions, indices 0–360.
      // P1 plays indices 0..180 (181 moves), P2 plays indices 360..181 (180 moves).
      // Interleave: P1(0), P2(360), P1(1), P2(359), ..., P1(179), P2(182), P1(180).
      // Together they cover every index exactly once. Captures may occur at territory
      // boundaries but pass is always legal in Go unless the game has ended.
      val boardPositions = 19 * 19                                            // 361
      val half           = boardPositions / 2                                 // 180
      val p1Indices      = (0 to half).toList                                 // 0..180 (181 elements)
      val p2Indices      = ((boardPositions - 1) to (half + 1) by -1).toList // 360..181 (180 elements)

      val allMoves = p1Indices
        .zip(p2Indices)
        .flatMap { case (a, b) =>
          List(Api.moveToUci(a, variant.Go19x19), Api.moveToUci(b, variant.Go19x19))
        } :+ Api.moveToUci(half, variant.Go19x19) // remaining P1 move at index 180

      val result = Api.position(variant.Go19x19).makeMovesNoLegalCheck(allMoves)
      result.legalActions.nonEmpty must_== true
    }
  }

  "A Go 19x19 game played to 602 total moves (P1 first legal drop, P2 last legal drop)" should {
    "still have legal moves available at 602 total moves" in {
      // Build 602 moves iteratively: after each move, re-query legalDrops for the next pick.
      // P1 always plays legalDrops(0) (lowest index), P2 always plays legalDrops.last
      // (highest index). legalDrops excludes pass, so no pass-skip logic is needed for P2.
      var position  = Api.position(variant.Go19x19)
      var moveCount = 0

      while (moveCount < 602) {
        val drops = position.legalDrops
        position  = position.makeMovesNoLegalCheck(List(Api.moveToUci(drops(0), variant.Go19x19)))
        moveCount += 1

        if (moveCount < 602) {
          val drops2 = position.legalDrops
          position   = position.makeMovesNoLegalCheck(List(Api.moveToUci(drops2.last, variant.Go19x19)))
          moveCount += 1
        }
      }

      moveCount must_== 602
      position.legalActions.nonEmpty must_== true
    }
  }
}
