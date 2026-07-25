package strategygames.go

import org.specs2.matcher.ValidatedMatchers
import org.specs2.mutable.Specification

class GoLongGameTest extends Specification with ValidatedMatchers {

  "A Go 19x19 game played to 610 total moves (P1 first legal drop, P2 last legal drop)" should {
    "still have legal moves available at 610 total moves" in {
      // Build 610 moves iteratively: after each move, re-query legalDrops for the next pick.
      // P1 always plays legalDrops(0) (lowest index), P2 always plays legalDrops.last
      // (highest index). legalDrops excludes pass, so no pass-skip logic is needed for P2.
      val moves     = 610 // was failing at 602 so play more to confirm no off-by-one errors
      var position  = Api.position(variant.Go19x19)
      var moveCount = 0

      while (moveCount < moves) {
        val drops = position.legalDrops
        position = position.makeMovesNoLegalCheck(List(Api.moveToUci(drops(0), variant.Go19x19)))
        moveCount += 1

        if (moveCount < moves) {
          val drops2 = position.legalDrops
          position = position.makeMovesNoLegalCheck(List(Api.moveToUci(drops2.last, variant.Go19x19)))
          moveCount += 1
        }
      }

      moveCount === moves
      position.legalActions.nonEmpty === true
    }
  }
}
