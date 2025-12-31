package strategygames.chess

import Pos._

class PosTest extends ChessTest {

  "A position" should {

    "be used to derive a relative list of positions" in {
      "D4 >| false" in { D4 >| (_ => false) must contain(E4, F4, G4, H4) }
      "D4 |< false" in { D4 |< (_ => false) must contain(C4, B4, A4) }
      "D4 >| (==G4)" in { D4 >| (G4 ==) must contain(E4, F4, G4) }
      "D4 |< (==C4)" in { D4 |< (C4 ==) must contain(C4) }
    }

    "be a string" in {
      "D5" in { D5.toString === "d5" }
    }

    "piotr" in {
      A1.piotr === 'a'
      B4.piotr === 'z'
      C4.piotr === 'A'
      D7.piotr === 'Z'
      E7.piotr === '0'
      F7.piotr === '1'
      F8.piotr === '9'
      G8.piotr === '!'
      H8.piotr === '?'
    }
  }
}
