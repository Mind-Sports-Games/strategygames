package strategygames.fairysf

import org.specs2.matcher.ValidatedMatchers
import org.specs2.mutable.Specification

class FairyStockfishBoardApiTest extends Specification with ValidatedMatchers {

  "Shogi board " should {
    val position = new FairyPosition(variant.Shogi)
    position.legalMoves().length must_!= 0
  }
}
