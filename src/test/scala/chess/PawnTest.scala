package strategygames.chess

import Pos._
import format.Uci

class PawnTest extends ChessTest {

  "a p1 pawn" should {

    "move towards rank by 1 square" in {
      makeBoard(
        A4 -> Pawn.p1
      ) destsFrom A4 must bePoss(A5)
    }

    "not move to positions that are occupied by the same player" in {
      makeBoard(
        A4 -> Pawn.p1,
        A5 -> Pawn.p1
      ) destsFrom A4 must bePoss()
    }

    "capture in diagonal" in {
      makeBoard(
        D4 -> Pawn.p1,
        C5 -> Pawn.p2,
        E5 -> Bishop.p2
      ) destsFrom D4 must bePoss(C5, D5, E5)
    }

    "require a capture to move in diagonal" in {
      makeBoard(
        A4 -> Pawn.p1,
        C5 -> Pawn.p1
      ) destsFrom A4 must bePoss(A5)
    }

    "move towards rank by 2 squares" in {
      "if the path is free" in {
        makeBoard(
          A2 -> Pawn.p1
        ) destsFrom A2 must bePoss(A3, A4)
      }
      "if the path is occupied by a friend" in {
        "close" in {
          makeBoard(
            A2 -> Pawn.p1,
            A3 -> Rook.p1
          ) destsFrom A2 must bePoss()
        }
        "far" in {
          makeBoard(
            A2 -> Pawn.p1,
            A4 -> Rook.p1
          ) destsFrom A2 must bePoss(A3)
        }
      }
      "if the path is occupied by a enemy" in {
        "close" in {
          makeBoard(
            A2 -> Pawn.p1,
            A3 -> Rook.p2
          ) destsFrom A2 must bePoss()
        }
        "far" in {
          makeBoard(
            A2 -> Pawn.p1,
            A4 -> Rook.p2
          ) destsFrom A2 must bePoss(A3)
        }
      }
    }
    "capture en passant" in {
      "with proper position" in {
        val board = makeBoard(
          D5 -> Pawn.p1,
          C5 -> Pawn.p2,
          E5 -> Pawn.p2
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
          D5 -> Pawn.p1,
          E5 -> Rook.p2
        ) withHistory History(
          lastMove = Option(Uci.Move(E7, E5))
        ) destsFrom D5 must bePoss(D6)
      }
      "friend pawn (?!)" in {
        makeBoard(
          D5 -> Pawn.p1,
          E5 -> Pawn.p1
        ) withHistory History(
          lastMove = Option(Uci.Move(E7, E5))
        ) destsFrom D5 must bePoss(D6)
      }
    }
  }

  "a p2 pawn" should {

    "move towards rank by 1 square" in {
      makeBoard(
        A4 -> Pawn.p2
      ) destsFrom A4 must bePoss(A3)
    }

    "not move to positions that are occupied by the same player" in {
      makeBoard(
        A4 -> Pawn.p2,
        A3 -> Pawn.p2
      ) destsFrom A4 must bePoss()
    }

    "capture in diagonal" in {
      makeBoard(
        D4 -> Pawn.p2,
        C3 -> Pawn.p1,
        E3 -> Bishop.p1
      ) destsFrom D4 must bePoss(C3, D3, E3)
    }

    "require a capture to move in diagonal" in {
      makeBoard(
        A4 -> Pawn.p2,
        C3 -> Pawn.p2
      ) destsFrom A4 must bePoss(A3)
    }

    "move towards rank by 2 squares" in {
      "if the path is free" in {
        makeBoard(
          A7 -> Pawn.p2
        ) destsFrom A7 must bePoss(A6, A5)
      }
      "if the path is occupied by a friend" in {
        "close" in {
          makeBoard(
            A7 -> Pawn.p2,
            A6 -> Rook.p2
          ) destsFrom A7 must bePoss()
        }
        "far" in {
          makeBoard(
            A7 -> Pawn.p2,
            A5 -> Rook.p2
          ) destsFrom A7 must bePoss(A6)
        }
      }
      "if the path is occupied by a enemy" in {
        "close" in {
          makeBoard(
            A7 -> Pawn.p2,
            A6 -> Rook.p1
          ) destsFrom A7 must bePoss()
        }
        "far" in {
          makeBoard(
            A7 -> Pawn.p2,
            A5 -> Rook.p1
          ) destsFrom A7 must bePoss(A6)
        }
      }
    }
    "capture en passant" in {
      "with proper position" in {
        val board = makeBoard(
          D4 -> Pawn.p2,
          C4 -> Pawn.p1,
          E4 -> Pawn.p1
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
          D4 -> Pawn.p2,
          E4 -> Rook.p1
        ) withHistory History(
          lastMove = Option(Uci.Move(E2, E4))
        ) destsFrom D4 must bePoss(D3)
      }
    }
  }
}
