package strategygames.samurai

import org.specs2.matcher.ValidatedMatchers
import org.specs2.mutable.Specification

class OwareLongGameTest extends Specification with ValidatedMatchers {

  "A game with more than 256 total moves" should {
    // https://playstrategy.org/ARQRYUmL/p1
    val game          = Api.position
    val previousMoves = List(
      0, 11, 0, 8, 0, 11, 0, 6, 5, 8, 4, 10, 1, 10, 3, 10, 0, 6, 2, 8, // 10 moves
      3, 7, 4, 9, 4, 7, 3, 11, 1, 6, 2, 8, 3, 7, 4, 8, 0, 10, 3, 11,   // 20
      1, 9, 4, 6, 1, 7, 2, 8, 3, 9, 4, 6, 5, 10, 2, 8, 3, 9, 4, 11,    // 30
      0, 10, 1, 11, 5, 6, 2, 7, 4, 9, 5, 7, 0, 8, 1, 11, 0, 9, 1, 10,  // 40
      0, 11, 0, 6, 3, 6, 4, 7, 2, 8, 1, 10, 2, 9, 3, 10, 5, 7, 4, 8,   // 50
      5, 11, 0, 7, 1, 10, 2, 9, 3, 8, 4, 11, 0, 10, 1, 9, 5, 7, 2, 8,  // 60
      3, 11, 0, 10, 4, 9, 1, 6, 2, 9, 3, 11, 1, 8, 4, 7, 2, 9, 3, 8,   // 70
      4, 9, 0, 10, 0, 11, 5, 8, 0, 7, 1, 6, 3, 7, 2, 9, 3, 8, 5, 6,    // 80
      4, 6, 5, 10, 0, 7, 1, 8, 2, 6, 3, 7, 4, 8, 5, 11, 0, 9, 0, 6,    // 90
      1, 7, 2, 8, 3, 11, 4, 10, 0, 9, 1, 11, 0, 10, 1, 11, 5, 6, 2, 7, // 100
      0, 8, 5, 9, 1, 6, 2, 7, 4, 8, 3, 10, 4, 6, 5, 11, 0, 9, 1, 7,    // 110
      2, 6, 3, 8, 4, 7, 5, 10, 0, 8, 1, 9, 2, 6, 3, 7, 4, 8, 5, 6,     // 120
      0, 11, 0, 10, 1, 9, 2, 7, 3, 8, 4, 11, 5                         // 127
    )
    val moves         = List[Int](10, 0, 11, 1, 6) // move 127 that used to break?

    val preMovesgame = game.makeMovesWithPrevious(List[Int](), previousMoves.map(m => Api.moveToUci(m)))
    println("Position after 127 moves each as the game ended")
    println(preMovesgame.toBoard)

    val newGame =
      game.makeMovesWithPrevious(moves, previousMoves.map(m => Api.moveToUci(m)))
    println(newGame.toBoard)

    "not fail after capacity reached 127th individual move and therefore have legal moves" in {
      newGame.legalMoves.size > 0 === true
    }

  }
}
