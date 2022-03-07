package strategygames.chess

class PieceTest extends ChessTest {

  "Piece" should {
    "compare" in {
      "objects and - method" in {
        Pawn - !P1 must_== Pawn - P2
      }
      "value and - method" in {
        val player = P1
        Pawn - !player must_== Pawn - P2
      }
    }
  }
}
