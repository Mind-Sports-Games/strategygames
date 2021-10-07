package strategygames.chess

import Pos._
import format.Uci

class PawnTest extends ChessTest {

  "a white pawn" should {

    "move towards rank by 1 square" in {
      makeBoard(
        A4 -> Pawn.white
      ) destsFrom A4 must bePoss(A5)
    }

    "not move to positions that are occupied by the same color" in {
      makeBoard(
        A4 -> Pawn.white,
        A5 -> Pawn.white
      ) destsFrom A4 must bePoss()
    }

    "capture in diagonal" in {
      makeBoard(
        D4 -> Pawn.white,
        C5 -> Pawn.black,
        E5 -> Bishop.black
      ) destsFrom D4 must bePoss(C5, D5, E5)
    }

    "require a capture to move in diagonal" in {
      makeBoard(
        A4 -> Pawn.white,
        C5 -> Pawn.white
      ) destsFrom A4 must bePoss(A5)
    }

    "move towards rank by 2 squares" in {
      "if the path is free" in {
        makeBoard(
          A2 -> Pawn.white
        ) destsFrom A2 must bePoss(A3, A4)
      }
      "if the path is occupied by a friend" in {
        "close" in {
          makeBoard(
            A2 -> Pawn.white,
            A3 -> Rook.white
          ) destsFrom A2 must bePoss()
        }
        "far" in {
          makeBoard(
            A2 -> Pawn.white,
            A4 -> Rook.white
          ) destsFrom A2 must bePoss(A3)
        }
      }
      "if the path is occupied by a enemy" in {
        "close" in {
          makeBoard(
            A2 -> Pawn.white,
            A3 -> Rook.black
          ) destsFrom A2 must bePoss()
        }
        "far" in {
          makeBoard(
            A2 -> Pawn.white,
            A4 -> Rook.black
          ) destsFrom A2 must bePoss(A3)
        }
      }
    }
    "capture en passant" in {
      "with proper position" in {
        val board = makeBoard(
          D5 -> Pawn.white,
          C5 -> Pawn.black,
          E5 -> Pawn.black
        )
        "without history" in {
          board destsFrom D5 must bePoss(D6)
        }
        "with irrelevant history" in {
          board withHistory History(
            lastMove = Option(Uci.Move(A2, A4))
          ) destsFrom D5 must bePoss(D6)
        }
        "with relevant history on the left" in {
          board withHistory History(
            lastMove = Option(Uci.Move(C7, C5))
          ) destsFrom D5 must bePoss(D6, C6)
        }
        "with relevant history on the right" in {
          board withHistory History(
            lastMove = Option(Uci.Move(E7, E5))
          ) destsFrom D5 must bePoss(D6, E6)
        }
      }
      "enemy not-a-pawn" in {
        makeBoard(
          D5 -> Pawn.white,
          E5 -> Rook.black
        ) withHistory History(
          lastMove = Option(Uci.Move(E7, E5))
        ) destsFrom D5 must bePoss(D6)
      }
      "friend pawn (?!)" in {
        makeBoard(
          D5 -> Pawn.white,
          E5 -> Pawn.white
        ) withHistory History(
          lastMove = Option(Uci.Move(E7, E5))
        ) destsFrom D5 must bePoss(D6)
      }
    }
  }

  "a black pawn" should {

    "move towards rank by 1 square" in {
      makeBoard(
        A4 -> Pawn.black
      ) destsFrom A4 must bePoss(A3)
    }

    "not move to positions that are occupied by the same color" in {
      makeBoard(
        A4 -> Pawn.black,
        A3 -> Pawn.black
      ) destsFrom A4 must bePoss()
    }

    "capture in diagonal" in {
      makeBoard(
        D4 -> Pawn.black,
        C3 -> Pawn.white,
        E3 -> Bishop.white
      ) destsFrom D4 must bePoss(C3, D3, E3)
    }

    "require a capture to move in diagonal" in {
      makeBoard(
        A4 -> Pawn.black,
        C3 -> Pawn.black
      ) destsFrom A4 must bePoss(A3)
    }

    "move towards rank by 2 squares" in {
      "if the path is free" in {
        makeBoard(
          A7 -> Pawn.black
        ) destsFrom A7 must bePoss(A6, A5)
      }
      "if the path is occupied by a friend" in {
        "close" in {
          makeBoard(
            A7 -> Pawn.black,
            A6 -> Rook.black
          ) destsFrom A7 must bePoss()
        }
        "far" in {
          makeBoard(
            A7 -> Pawn.black,
            A5 -> Rook.black
          ) destsFrom A7 must bePoss(A6)
        }
      }
      "if the path is occupied by a enemy" in {
        "close" in {
          makeBoard(
            A7 -> Pawn.black,
            A6 -> Rook.white
          ) destsFrom A7 must bePoss()
        }
        "far" in {
          makeBoard(
            A7 -> Pawn.black,
            A5 -> Rook.white
          ) destsFrom A7 must bePoss(A6)
        }
      }
    }
    "capture en passant" in {
      "with proper position" in {
        val board = makeBoard(
          D4 -> Pawn.black,
          C4 -> Pawn.white,
          E4 -> Pawn.white
        )
        "without history" in {
          board destsFrom D4 must bePoss(D3)
        }
        "with relevant history on the left" in {
          board withHistory History(
            lastMove = Option(Uci.Move(C2, C4))
          ) destsFrom D4 must bePoss(D3, C3)
        }
      }
      "enemy not-a-pawn" in {
        makeBoard(
          D4 -> Pawn.black,
          E4 -> Rook.white
        ) withHistory History(
          lastMove = Option(Uci.Move(E2, E4))
        ) destsFrom D4 must bePoss(D3)
      }
    }
  }
}
