package strategygames.chess

class PlayerTest extends ChessTest {

  "Player" should {
    "unary !" in {
      "p1" in { !P1 === P2 }
      "p2" in { !P2 === P1 }
    }
  }
}
