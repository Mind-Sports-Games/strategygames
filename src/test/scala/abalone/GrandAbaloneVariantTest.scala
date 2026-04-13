package strategygames.abalone

import strategygames.abalone.variant.GrandAbalone

class GrandAbaloneVariantTest extends AbaloneTest {
  /*
   *      ● ● · · o o   · · · ·
   *     ● ● ● · o o o   · · ·
   *    · ● ● · · o o ·   · ·
   *   · · · · · · · · ·   ·
   *  o o · · · · · · ● ●
   * o o o · · · · · ● ● ●
   *  o o · · · · · · ● ●
   *   · · · · · · · · ·   ·
   *    · ● ● · · o o ·   · ·
   *     ● ● ● · o o o   · · ·
   *      ● ● · · o o   · · · ·
   */
  "\"Belgian daisy\" start position" should {
    val fen        = GrandAbalone.initialFen
    val board      = Board(fen.pieces(GrandAbalone), History(), GrandAbalone)
    val situation  = Situation(board, P1)
    val moves      = valid(situation).flatMap(_._2)
    val lineMoves  = valid_line(situation).flatMap(_._2)
    val jumpMoves  = valid_jump(situation).flatMap(_._2)
    val movesOf1   = moves.filter(of1(board))
    val movesOfGt1 = moves.filter(ofGt1(board))

    "compute the correct number of moves" in {
      moves.size === 93

      lineMoves.size === 75
      jumpMoves.size === 18

      movesOf1.size === 33
      movesOfGt1.size === 60
    }
  }
}
