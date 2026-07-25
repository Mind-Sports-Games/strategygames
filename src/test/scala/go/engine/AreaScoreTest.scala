package strategygames.go.engine

import org.specs2.mutable.Specification

class AreaScoreTest extends Specification {

  import GoEngineTestSupport._

  private def scoreOf(size: Int, blackKeys: List[String], whiteKeys: List[String]): AreaScore = {
    val black = blackKeys.map(engineMove(size, _)).toSet
    val white = whiteKeys.map(engineMove(size, _)).toSet
    GoState
      .fromStoneOwners(
        size,
        move =>
          if (black.contains(move)) GoState.BlackPlayer
          else if (white.contains(move)) GoState.WhitePlayer
          else GoState.NoOwner,
        GoState.BlackPlayer,
        0,
        0,
        None,
        0
      )
      .areaScore
  }

  private val fullFileOfBlack = (1 to 9).map(rank => s"e$rank").toList

  "an empty board" should {
    "give neither player any area" in {
      forall(List(9, 13, 19)) { size =>
        GoState.initial(size).areaScore === AreaScore(0, 0)
      }
    }
  }

  "a board holding a single black stone" should {
    "give black every point" in {
      scoreOf(9, List("e5"), Nil) === AreaScore(81, 0)
    }
  }

  "a region touching both players" should {
    "belong to neither" in {
      scoreOf(9, List("a1"), List("b1")) === AreaScore(1, 1)
    }
  }

  "a black wall splitting the board" should {
    "give black both empty regions" in {
      scoreOf(9, fullFileOfBlack, Nil) === AreaScore(81, 0)
    }
    "give black only the region white does not touch" in {
      scoreOf(9, fullFileOfBlack, List("g5")) === AreaScore(45, 1)
    }
  }

  "a single point eye" should {
    "belong to the player enclosing it" in {
      scoreOf(9, List("a2", "b1"), List("i9")) === AreaScore(3, 1)
    }
  }

  "removing dead stones" should {
    "hand their surrounded region to the opponent" in {
      val wall      = fullFileOfBlack.map(engineMove(9, _)).toSet
      val deadStone = engineMove(9, "g5")
      val withDead  = GoState.fromStoneOwners(
        9,
        move =>
          if (wall.contains(move)) GoState.BlackPlayer
          else if (move == deadStone) GoState.WhitePlayer
          else GoState.NoOwner,
        GoState.BlackPlayer,
        0,
        0,
        None,
        0
      )
      (withDead.areaScore === AreaScore(45, 1)) and
        (withDead.withoutStones(Set(deadStone)).areaScore === AreaScore(81, 0))
    }
  }

  "area score of a played position" should {
    "count captured stones as territory once they are off the board" in {
      val state = playAll(9, List("a1", "b1", "pass", "a2"))
      (state.stoneOwnerAt(engineMove(9, "a1")) === GoState.NoOwner) and
        (state.capturesByWhite === 1) and
        (state.areaScore === AreaScore(0, 81))
    }
  }
}
