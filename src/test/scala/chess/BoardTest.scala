package strategygames.chess

import strategygames.Player

import Pos._

class BoardTest extends ChessTest {

  val board = makeBoard

  "a board" should {

    "position pieces correctly" in {
      board.pieces must havePairs(
        A1 -> (Rook - P1),
        B1 -> (Knight - P1),
        C1 -> (Bishop - P1),
        D1 -> (Queen - P1),
        E1 -> (King - P1),
        F1 -> (Bishop - P1),
        G1 -> (Knight - P1),
        H1 -> (Rook - P1),
        A2 -> (Pawn - P1),
        B2 -> (Pawn - P1),
        C2 -> (Pawn - P1),
        D2 -> (Pawn - P1),
        E2 -> (Pawn - P1),
        F2 -> (Pawn - P1),
        G2 -> (Pawn - P1),
        H2 -> (Pawn - P1),
        A7 -> (Pawn - P2),
        B7 -> (Pawn - P2),
        C7 -> (Pawn - P2),
        D7 -> (Pawn - P2),
        E7 -> (Pawn - P2),
        F7 -> (Pawn - P2),
        G7 -> (Pawn - P2),
        H7 -> (Pawn - P2),
        A8 -> (Rook - P2),
        B8 -> (Knight - P2),
        C8 -> (Bishop - P2),
        D8 -> (Queen - P2),
        E8 -> (King - P2),
        F8 -> (Bishop - P2),
        G8 -> (Knight - P2),
        H8 -> (Rook - P2)
      )
    }

    "have pieces by default" in {
      board.pieces must not(beEmpty)
    }

    "have castling rights by default" in {
      board.history.castles == Castles.all
    }

    "allow a piece to be placed" in {
      board.place(Rook - P1, E3) must beSome.like { case b =>
        b(E3) must beEqualTo(Option(Rook - P1))
      }
    }

    "allow a piece to be taken" in {
      board take A1 must beSome.like { case b =>
        b(A1) must beNone
      }
    }

    "allow a piece to move" in {
      board.move(E2, E4) must beSome.like { case b =>
        b(E4) must beEqualTo(Option(Pawn - P1))
      }
    }

    "not allow an empty position to move" in {
      board.move(E5, E6) must beNone
    }

    "not allow a piece to move to an occupied position" in {
      board.move(A1, A2) must beNone
    }

    "allow a pawn to be promoted to a queen" in {
      makeEmptyBoard.place(Pawn.p2, A8).flatMap(_ promote A8) must beSome.like { case b =>
        b(A8) must beSome(Queen.p2)
      }
    }

    "allow chaining actions" in {
      makeEmptyBoard.seq(
        _.place(Pawn - P1, A2),
        _.place(Pawn - P1, A3),
        _.move(A2, A4)
      ) must beSome.like { case b =>
        b(A4) must beEqualTo(Option(Pawn - P1))
      }
    }

    "fail on bad actions chain" in {
      makeEmptyBoard.seq(
        _.place(Pawn - P1, A2),
        _.place(Pawn - P1, A3),
        _.move(B2, B4)
      ) must beNone
    }

    "provide occupation map" in {
      makeBoard(
        A2 -> (Pawn - P1),
        A3 -> (Pawn - P1),
        D1 -> (King - P1),
        E8 -> (King - P2),
        H4 -> (Queen - P2)
      ).occupation === Player.Map(
        p1 = Set(A2, A3, D1),
        p2 = Set(E8, H4)
      )
    }

    "navigate in pos based on pieces" in {
      "right to end" in {
        val board: Board = """
R   K  R"""
        E1 >| ((p: Pos) => board.pieces contains p) === List(F1, G1, H1)
      }
      "right to next" in {
        val board: Board = """
R   KB R"""
        E1 >| ((p: Pos) => board.pieces contains p) === List(F1)
      }
      "left to end" in {
        val board: Board = """
R   K  R"""
        val result = E1 |< ((p: Pos) => board.pieces contains p)
        result === List(D1, C1, B1, A1)
      }
      "right to next" in {
        val board: Board = """
R  BK  R"""
        val result = E1 |< ((p: Pos) => board.pieces contains p)
        result === List(D1)
      }
    }

    "provide file occupations" in {
      makeBoard(
        A2 -> (Pawn - P1),
        A3 -> (Pawn - P1),
        D1 -> (King - P1),
        E7 -> (King - P2),
        H1 -> (Queen - P2)
      ).fileOccupation(File.A) === Map(
        A2 -> (Pawn - P1),
        A3 -> (Pawn - P1)
      )
    }

    "provide rank occupations" in {
      makeBoard(
        A2 -> (Pawn - P1),
        A3 -> (Pawn - P1),
        D1 -> (King - P1),
        E7 -> (King - P2),
        H1 -> (Queen - P2)
      ).rankOccupation(Rank.First) === Map(
        D1 -> (King - P1),
        H1 -> (Queen - P2)
      )
    }

    "provide diagonal ascending occupations" in {
      makeBoard(
        A2 -> (Pawn - P1),
        A3 -> (Pawn - P1),
        D1 -> (King - P1),
        E7 -> (King - P2),
        H1 -> (Queen - P2)
      ).diagAscOccupation(D6) === Map(
        A3 -> (Pawn - P1),
        E7 -> (King - P2)
      )
    }

    "provide diagonal descending occupations" in {
      makeBoard(
        A3 -> (Pawn - P1),
        B2 -> (Pawn - P1),
        C1 -> (King - P1),
        E7 -> (King - P2),
        H1 -> (Queen - P2)
      ).diagDescOccupation(B2) === Map(
        A3 -> (Pawn - P1),
        B2 -> (Pawn - P1),
        C1 -> (King - P1)
      )
    }
  }
}
