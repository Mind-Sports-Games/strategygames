package strategygames.fiveinarow

import org.specs2.mutable.Specification

import strategygames.fiveinarow.variant.{ Gomoku, Pente, Renju, Variant }

class FiveInARowDrawOfferTest extends Specification with FiveInARowTestSupport {

  // colours change at least every second stone along rows, columns and both diagonals
  private def stones(variant: Variant, count: Int): Seq[(String, Role)] =
    (for {
      rank <- 0 until variant.boardSize.height
      file <- 0 until variant.boardSize.width
    } yield {
      val key  = s"${('a' + file).toChar}${rank + 1}"
      val role = if (((file + 2 * rank) / 2) % 2 == 0) BlackStone else WhiteStone
      key -> role
    }).take(count)

  private def gameWith(variant: Variant, count: Int): Game =
    Game(situationOf(boardIn(variant)(stones(variant, count)*)))

  "draw offers" should {

    "be refused in gomoku and renju before 75 stones" in {
      gameWith(Gomoku, 74).drawOfferAllowed must beFalse
      gameWith(Renju, 74).drawOfferAllowed must beFalse
      start.drawOfferAllowed must beFalse
    }

    "be allowed in gomoku and renju from 75 stones" in {
      gameWith(Gomoku, 75).situation.end must beFalse
      gameWith(Gomoku, 75).drawOfferAllowed must beTrue
      gameWith(Renju, 75).drawOfferAllowed must beTrue
      gameWith(Gomoku, 100).drawOfferAllowed must beTrue
    }

    "never be allowed in pente" in {
      gameWith(Pente, 120).drawOfferAllowed must beFalse
    }

    "reach the wrapper" in {
      strategygames.Game.FiveInARow(gameWith(Gomoku, 74)).drawOfferAllowed must beFalse
      strategygames.Game.FiveInARow(gameWith(Gomoku, 75)).drawOfferAllowed must beTrue
    }
  }

}
