package strategygames.chess

import strategygames.Color

import Pos._

class BoardTest extends ChessTest {

  val board = makeBoard

  "a board" should {

    "position pieces correctly" in {
      board.pieces must havePairs(
        A1 -> (Rook - White),
        B1 -> (Knight - White),
        C1 -> (Bishop - White),
        D1 -> (Queen - White),
        E1 -> (King - White),
        F1 -> (Bishop - White),
        G1 -> (Knight - White),
        H1 -> (Rook - White),
        A2 -> (Pawn - White),
        B2 -> (Pawn - White),
        C2 -> (Pawn - White),
        D2 -> (Pawn - White),
        E2 -> (Pawn - White),
        F2 -> (Pawn - White),
        G2 -> (Pawn - White),
        H2 -> (Pawn - White),
        A7 -> (Pawn - Black),
        B7 -> (Pawn - Black),
        C7 -> (Pawn - Black),
        D7 -> (Pawn - Black),
        E7 -> (Pawn - Black),
        F7 -> (Pawn - Black),
        G7 -> (Pawn - Black),
        H7 -> (Pawn - Black),
        A8 -> (Rook - Black),
        B8 -> (Knight - Black),
        C8 -> (Bishop - Black),
        D8 -> (Queen - Black),
        E8 -> (King - Black),
        F8 -> (Bishop - Black),
        G8 -> (Knight - Black),
        H8 -> (Rook - Black)
      )
    }

    "have pieces by default" in {
      board.pieces must not beEmpty
    }

    "have castling rights by default" in {
      board.history.castles == Castles.all
    }

    "allow a piece to be placed" in {
      board.place(Rook - White, E3) must beSome.like { case b =>
        b(E3) mustEqual Option(Rook - White)
      }
    }

    "allow a piece to be taken" in {
      board take A1 must beSome.like { case b =>
        b(A1) must beNone
      }
    }

    "allow a piece to move" in {
      board.move(E2, E4) must beSome.like { case b =>
        b(E4) mustEqual Option(Pawn - White)
      }
    }

    "not allow an empty position to move" in {
      board.move(E5, E6) must beNone
    }

    "not allow a piece to move to an occupied position" in {
      board.move(A1, A2) must beNone
    }

    "allow a pawn to be promoted to a queen" in {
      makeEmptyBoard.place(Pawn.black, A8) flatMap (_ promote A8) must beSome.like { case b =>
        b(A8) must beSome(Queen.black)
      }
    }

    "allow chaining actions" in {
      makeEmptyBoard.seq(
        _.place(Pawn - White, A2),
        _.place(Pawn - White, A3),
        _.move(A2, A4)
      ) must beSome.like { case b =>
        b(A4) mustEqual Option(Pawn - White)
      }
    }

    "fail on bad actions chain" in {
      makeEmptyBoard.seq(
        _.place(Pawn - White, A2),
        _.place(Pawn - White, A3),
        _.move(B2, B4)
      ) must beNone
    }

    "provide occupation map" in {
      makeBoard(
        A2 -> (Pawn - White),
        A3 -> (Pawn - White),
        D1 -> (King - White),
        E8 -> (King - Black),
        H4 -> (Queen - Black)
      ).occupation must_== Color.Map(
        white = Set(A2, A3, D1),
        black = Set(E8, H4)
      )
    }

    "navigate in pos based on pieces" in {
      "right to end" in {
        val board: Board = """
R   K  R"""
        E1 >| (p => board.pieces contains p) must_== List(F1, G1, H1)
      }
      "right to next" in {
        val board: Board = """
R   KB R"""
        E1 >| (p => board.pieces contains p) must_== List(F1)
      }
      "left to end" in {
        val board: Board = """
R   K  R"""
        E1 |< (p => board.pieces contains p) must_== List(D1, C1, B1, A1)
      }
      "right to next" in {
        val board: Board = """
R  BK  R"""
        E1 |< (p => board.pieces contains p) must_== List(D1)
      }
    }

    "provide file occupations" in {
      makeBoard(
        A2 -> (Pawn - White),
        A3 -> (Pawn - White),
        D1 -> (King - White),
        E7 -> (King - Black),
        H1 -> (Queen - Black)
      ).fileOccupation(File.A) must_== Map(
        A2 -> (Pawn - White),
        A3 -> (Pawn - White)
      )
    }

    "provide rank occupations" in {
      makeBoard(
        A2 -> (Pawn - White),
        A3 -> (Pawn - White),
        D1 -> (King - White),
        E7 -> (King - Black),
        H1 -> (Queen - Black)
      ).rankOccupation(Rank.First) must_== Map(
        D1 -> (King - White),
        H1 -> (Queen - Black)
      )
    }

    "provide diagonal ascending occupations" in {
      makeBoard(
        A2 -> (Pawn - White),
        A3 -> (Pawn - White),
        D1 -> (King - White),
        E7 -> (King - Black),
        H1 -> (Queen - Black)
      ).diagAscOccupation(D6) must_== Map(
        A3 -> (Pawn - White),
        E7 -> (King - Black)
      )
    }

    "provide diagonal descending occupations" in {
      makeBoard(
        A3 -> (Pawn - White),
        B2 -> (Pawn - White),
        C1 -> (King - White),
        E7 -> (King - Black),
        H1 -> (Queen - Black)
      ).diagDescOccupation(B2) must_== Map(
        A3 -> (Pawn - White),
        B2 -> (Pawn - White),
        C1 -> (King - White)
      )
    }
  }
}
