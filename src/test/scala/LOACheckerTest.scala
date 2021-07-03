package chess

import Pos._

class LOACheckerTest extends ChessTest {

  "a loa checker" should {

    val loachecker = White - LOAChecker

    "move one space in any direction on empty board" in {
      pieceMoves(loachecker, D4) must bePoss(
        D5,
        D3,
        E4,
        C4,
        C3,
        E5,
        C5,
        E3
      )
    }

    "move one space in any direction even from corner on an empty board" in {
      pieceMoves(loachecker, A1) must bePoss(
        A2,
        B1,
        B2
      )
    }

    "not move to positions that are occupied by the same colour" in {
      val board = """
L   L  l
l
      L
     
L   L  L


    L  L
"""
      board destsFrom E4 must bePoss(
        B4,
        E7,
        C2,
        B7
      )
    }

    "move to positions over the same colour" in {
      val board = """
       l
l
  L L 
     L
  L L  L


    L  L
"""
      board destsFrom E4 must bePoss(
        B4,
        E7,
        C2,
        G6,
        B7
      )
    }

    "not move to positions over opposition colour" in {
      val board = """


  l l 
     l
  l L  L


    L  L
"""
      board destsFrom E4 must bePoss(
        C2
      )
    }

    "capture opponent pieces" in {
      val board = """
       L

  l l l

  l L  l


    l  l
"""
      board destsFrom E4 must bePoss(
        H4,
        E1,
        C2,
        G6,
        H1
      )
    }
  }
}
