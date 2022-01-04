package strategygames.chess

class PieceTest extends ChessTest {

  "Piece" should {
    "compare" in {
      "objects and - method" in {
        Pawn - !White must_== Pawn - Black
      }
      "value and - method" in {
        val color = White
        Pawn - !color must_== Pawn - Black
      }
    }
  }
}
