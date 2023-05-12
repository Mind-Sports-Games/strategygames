package strategygames.go

import org.specs2.matcher.ValidatedMatchers
import org.specs2.mutable.Specification

import strategygames.Player

class GoSituationTest extends Specification with ValidatedMatchers {

  "valid moves and drops in situation" should {
    val board     = Board.init(variant.Go19x19)
    val p1        = Player(true)
    val situation = Situation(board, p1)

    val moves = variant.Go19x19.validMoves(situation)
    val drops = variant.Go19x19.validDrops(situation)

    "have no moves" in {
      moves.size must_== 0
    }
    "all 361 drops" in {
      drops.size must_== 361
    }
  }

}
