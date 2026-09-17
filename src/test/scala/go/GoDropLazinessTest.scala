package strategygames.go
package variant

import org.specs2.mutable.Specification

import strategygames.Player
import strategygames.go._

final private class BoardAfterCountingGo19x19
    extends Variant(
      id = GoDropLazinessTest.idOfNoRegisteredVariant,
      key = Go19x19.key,
      name = Go19x19.name,
      standardInitialPosition = Go19x19.standardInitialPosition,
      boardSize = Go19x19.boardSize
    ) {

  private var boardsBuilt: Int = 0

  def boardsBuiltCount: Int = boardsBuilt

  override def boardAfter(situation: Situation, pos: Pos): Board = {
    boardsBuilt += 1
    super.boardAfter(situation, pos)
  }

  def gameFamily = Go19x19.gameFamily
  def perfIcon   = Go19x19.perfIcon
  def perfId     = Go19x19.perfId

  override def initialFen = Go19x19.initialFen
}

class GoDropLazinessTest extends Specification {

  import GoDropLazinessTest._

  "generating the drops of a mid game position" should {

    "build no board of its own" in {
      val variant = new BoardAfterCountingGo19x19
      val drops   = variant.validDrops(midGameOn(variant))
      (drops.size === emptyPointsLeft) and (variant.boardsBuiltCount === 0)
    }

    "build one board for each drop whose after is forced, and no others" in {
      val variant = new BoardAfterCountingGo19x19
      val drops   = variant.validDrops(midGameOn(variant))
      drops.take(dropsForced).foreach(_.after)
      variant.boardsBuiltCount === dropsForced
    }

    "build one board however often the same after is read" in {
      val variant = new BoardAfterCountingGo19x19
      val drop    = variant.validDrops(midGameOn(variant)).head
      (drop.after === drop.after) and (variant.boardsBuiltCount === 1)
    }
  }
}

object GoDropLazinessTest {

  val idOfNoRegisteredVariant = -1

  private val dropsForced = 3

  private val midGameKeys =
    List("d4", "q16", "q4", "d16", "f17", "c6", "r14", "f3", "d10", "k10")

  val emptyPointsLeft: Int = Board.Dim19x19.validPos.size - midGameKeys.size

  def midGameOn(variant: Variant): Situation =
    Situation(Board(midGameStones, variant), Player.fromTurnCount(midGameKeys.size))

  private def midGameStones: List[(Pos, Piece)] =
    midGameKeys.zipWithIndex.map { case (key, played) =>
      Pos.fromKey(key).getOrElse(sys.error(s"unreadable mid game point ${key}")) ->
        Piece(Player.fromTurnCount(played), Role.defaultRole)
    }
}
