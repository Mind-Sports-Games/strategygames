package strategygames.dameo

import org.specs2.matcher.ValidatedMatchers
import strategygames.dameo.format.FEN

// import variant.Dameo

class DameoVariantTest extends DameoTest with ValidatedMatchers {
  "variant" should {
    "have 52 opening moves" in {
      val board = Board(variant.Dameo.initialFen.pieces, variant.Dameo)
      val situation1 = Situation(board, P1)
      val situation2 = Situation(board, P2)

      val moves1 = board.variant.validMoves(situation1)
      moves1.values.flatten.size must_== 52
      val moves2 = board.variant.validMoves(situation2)
      moves2.values.flatten.size must_== 52
    }

    "Have both man and king moves" in {
      val board = Board(FEN("W:Wc2.k,d4:Bf7.k,g5:H0:F1").pieces, variant.Dameo)
      val situation1 = Situation(board, P1)
      val situation2 = Situation(board, P2)

      val moves1 = board.variant.validMoves(situation1)
      moves1.size must_== 2
      moves1(Pos.D4).size must_== 3
      moves1(Pos.C2).size must_== 23
      val moves2 = board.variant.validMoves(situation2)
      moves2.size must_== 2
      moves2(Pos.F7).size must_== 23
      moves2(Pos.G5).size must_== 3
    }

    "Have only capturing moves when available" in {
      val board = Board(FEN("W:Wd4,e1,f1,g1:Bd5,e8,f8,g8:H0:F1").pieces, variant.Dameo)
      val situation1 = Situation(board, P1)
      val situation2 = Situation(board, P2)

      val moves1 = board.variant.validMoves(situation1)
      moves1.size must_== 1
      moves1(Pos.D4).size must_== 1
      val moves2 = board.variant.validMoves(situation2)
      moves2.size must_== 1
      moves2(Pos.D5).size must_== 1
    }

    "Have only the maximal capturing sequence" in {
      val board = Board(FEN("W:Wc5,d2,d4,e1,f1,g1:Bc4,d5,d7,e8,f8,g8:H0:F1").pieces, variant.Dameo)
      val situation1 = Situation(board, P1)
      val situation2 = Situation(board, P2)

      val moves1 = board.variant.validMoves(situation1)
      moves1.size must_== 1
      moves1(Pos.D4).size must_== 1
      val moves2 = board.variant.validMoves(situation2)
      moves2.size must_== 1
      moves2(Pos.D5).size must_== 1
    }

    "Have multiple equally long capturing sequences" in {
      val board = Board(FEN("W:Wb6,c5,d2,d4,e1,f1,g1:Bb3,c4,d5,d7,e8,f8,g8:H0:F1").pieces, variant.Dameo)
      val situation1 = Situation(board, P1)
      val situation2 = Situation(board, P2)

      val moves1 = board.variant.validMoves(situation1)
      moves1.size must_== 2
      moves1(Pos.D4).size must_== 2
      moves1(Pos.C5).size must_== 1
      val moves2 = board.variant.validMoves(situation2)
      moves2.size must_== 2
      moves2(Pos.D5).size must_== 2
      moves2(Pos.C4).size must_== 1
    }
    
    "Have both man and king captures" in {
      val board = Board(FEN("W:Wb6,c5,d2,d4.k,e1,f1,g1:Bb3,c4,d5.k,d7,e8,f8,g8:H0:F1").pieces, variant.Dameo)
      val situation1 = Situation(board, P1)
      val situation2 = Situation(board, P2)

      val moves1 = board.variant.validMoves(situation1)
      println(moves1)
      moves1.size must_== 2
      moves1(Pos.D4).size must_== 2
      moves1(Pos.C5).size must_== 1
      val moves2 = board.variant.validMoves(situation2)
      moves2.size must_== 2
      moves2(Pos.D5).size must_== 2
      moves2(Pos.C4).size must_== 1
    }

    "Finish the capture sequence that was started" in {
      val board = Board(FEN("W:Wc1,e1,g1:Bc2,c4,c6,e2,e4,e6,g2,g4,g6:H0:F1").pieces, variant.Dameo)
      val situation = Situation(board, P1)

      val moves1 = board.variant.validMoves(situation)
      moves1.size must_== 3

      val situation2 = moves1(Pos.C1)(0).situationAfter
      val moves2 = board.variant.validMoves(situation2)
      moves2.keys must_== Set(Pos.C3)
      moves2.size must_== 1

      val situation3 = moves2(Pos.C3)(0).situationAfter
      val moves3 = board.variant.validMoves(situation3)
      moves3.keys must_== Set(Pos.C5)
      moves3.size must_== 1

      val finalsit = moves3(Pos.C5)(0).situationAfter

      finalsit.player must_== P2
      finalsit.board.pieces must_== FEN("B:Wc7,e1,g1:Be2,e4,e6,g2,g4,g6:H0:F1").pieces
    }
  }
}