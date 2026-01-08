package strategygames.dameo

import org.specs2.matcher.ValidatedMatchers
import strategygames.dameo.format.FEN

import strategygames.Status

class DameoVariantTest extends DameoTest with ValidatedMatchers {
  "variant" should {
    "have 52 opening moves" in {
      val board      = Board(variant.Dameo.initialFen.pieces, variant.Dameo)
      val situation1 = Situation(board, P1)
      val situation2 = Situation(board, P2)

      val moves1 = board.variant.validMoves(situation1)
      moves1.values.flatten.size must_== 52
      val moves2 = board.variant.validMoves(situation2)
      moves2.values.flatten.size must_== 52
    }

    "Have both man and king moves" in {
      val board      = Board(FEN("W:WKc2,d4:BKf7,g5:H0:F1").pieces, variant.Dameo)
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
      val board      = Board(FEN("W:Wd4,e1,f1,g1:Bd5,e8,f8,g8:H0:F1").pieces, variant.Dameo)
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
      val board      = Board(FEN("W:Wc5,d2,d4,e1,f1,g1:Bc4,d5,d7,e8,f8,g8:H0:F1").pieces, variant.Dameo)
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
      val board      = Board(FEN("W:Wb6,c5,d2,d4,e1,f1,g1:Bb3,c4,d5,d7,e8,f8,g8:H0:F1").pieces, variant.Dameo)
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
      val board      = Board(FEN("W:Wb6,c5,d2,Kd4,e1,f1,g1:Bb3,c4,Kd5,d7,e8,f8,g8:H0:F1").pieces, variant.Dameo)
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

    "Finish the capture sequence that was started" in {
      val board     = Board(FEN("W:Wc1,e1,g1:Bc2,c4,c6,e2,e4,e6,g2,g4,g6:H0:F1").pieces, variant.Dameo)
      val situation = Situation(board, P1)

      val moves1 = board.variant.validMoves(situation)
      moves1.size must_== 3

      val situation2 = moves1(Pos.C1)(0).situationAfter
      val moves2     = board.variant.validMoves(situation2)
      moves2.keys must_== Set(Pos.C3)
      moves2.size must_== 1

      val situation3 = moves2(Pos.C3)(0).situationAfter
      val moves3     = board.variant.validMoves(situation3)
      moves3.keys must_== Set(Pos.C5)
      moves3.size must_== 1

      val finalsit = moves3(Pos.C5)(0).situationAfter

      finalsit.player must_== P2
      finalsit.board.pieces must_== FEN("B:Wc7,e1,g1:Be2,e4,e6,g2,g4,g6:H0:F1").pieces
    }

    "Trigger game end and P1 win" in {
      val board      = Board(FEN("W:Wc4,e4,g4:Bc5:H0:F1").pieces, variant.Dameo)
      val situation  = Situation(board, P1)
      val moves      = board.variant.validMoves(situation)
      moves.size must_== 1
      val situation2 = moves(Pos.C4)(0).situationAfter
      situation2.end must_== true
      situation2.playable(true) must_== false
      situation2.status must_== Some(Status.VariantEnd)
      situation2.winner must_== Some(P1)
    }

    "Trigger game end and P2 win" in {
      val board      = Board(FEN("W:Wc4:Bc5,e5,g5:H0:F1").pieces, variant.Dameo)
      val situation  = Situation(board, P2)
      val moves      = board.variant.validMoves(situation)
      moves.size must_== 1
      val situation2 = moves(Pos.C5)(0).situationAfter
      situation2.end must_== true
      situation2.playable(true) must_== false
      situation2.status must_== Some(Status.VariantEnd)
      situation2.winner must_== Some(P2)
    }

    "Win by blocking" in {
      val board      = Board(FEN("W:Wa1,a3,a4,b1,c5:Ba2:H0:F1").pieces, variant.Dameo)
      val situation  = Situation(board, P1)
      val moves      = board.variant.validMoves(situation)
      val situation2 = moves(Pos.C5)(0).situationAfter
      situation2.end must_== true
      situation2.playable(true) must_== false
      situation2.status must_== Some(Status.VariantEnd)
      situation2.winner must_== Some(P1)
    }

    "Trigger draw in king vs king endgame, white to play" in {
      // Each player gets 2 more turns
      val board       = Board(FEN("W:WKd3:Bc3,Ke8:H0:F1").pieces, variant.Dameo)
      val situation   = Situation(board, P1)
      val situation0  = board.variant.validMoves(situation)(Pos.D3).find(_.dest == Pos.B3).get.situationAfter
      val situation1a =
        board.variant.validMoves(situation0)(Pos.E8).find(_.dest == Pos.H8).get.situationAfter
      val situation1b =
        board.variant.validMoves(situation1a)(Pos.B3).find(_.dest == Pos.B5).get.situationAfter
      val situation2a =
        board.variant.validMoves(situation1b)(Pos.H8).find(_.dest == Pos.H3).get.situationAfter
      val situation2b =
        board.variant.validMoves(situation2a)(Pos.B5).find(_.dest == Pos.D5).get.situationAfter

      situation2a.end must_== false

      situation2b.end must_== true
      situation2b.playable(true) must_== false
      situation2b.status must_== Some(Status.Draw)
      situation2b.winner must_== None

      format.Forsyth.>>(situation0).halfMoveClock must_== Some(1)
      format.Forsyth.>>(situation1a).halfMoveClock must_== Some(2)
      format.Forsyth.>>(situation1b).halfMoveClock must_== Some(3)
      format.Forsyth.>>(situation2a).halfMoveClock must_== Some(4)
      format.Forsyth.>>(situation2b).halfMoveClock must_== Some(5)
    }

    "Trigger draw in king vs king endgame, black to play" in {
      // Each player gets 2 more turns
      val board       = Board(FEN("B:WKb3,d8:BKc8:H0:F1").pieces, variant.Dameo)
      val situation   = Situation(board, P2)
      val situation0  = board.variant.validMoves(situation)(Pos.C8).find(_.dest == Pos.E8).get.situationAfter
      val situation1a =
        board.variant.validMoves(situation0)(Pos.B3).find(_.dest == Pos.B5).get.situationAfter
      val situation1b =
        board.variant.validMoves(situation1a)(Pos.E8).find(_.dest == Pos.H8).get.situationAfter
      val situation2a =
        board.variant.validMoves(situation1b)(Pos.B5).find(_.dest == Pos.D5).get.situationAfter
      val situation2b =
        board.variant.validMoves(situation2a)(Pos.H8).find(_.dest == Pos.H3).get.situationAfter

      situation2a.end must_== false

      situation2b.end must_== true
      situation2b.playable(true) must_== false
      situation2b.status must_== Some(Status.Draw)
      situation2b.winner must_== None

      format.Forsyth.>>(situation0).halfMoveClock must_== Some(1)
      format.Forsyth.>>(situation1a).halfMoveClock must_== Some(2)
      format.Forsyth.>>(situation1b).halfMoveClock must_== Some(3)
      format.Forsyth.>>(situation2a).halfMoveClock must_== Some(4)
      format.Forsyth.>>(situation2b).halfMoveClock must_== Some(5)
    }

    "Trigger draw by threefold repetition" in {
      val board       = Board(FEN("W:WKb4,Kb5:BKe7,f8:H0:F1").pieces, variant.Dameo)
      val situation   = Situation(board, P1)
      val situation0a = board.variant.validMoves(situation)(Pos.B4).find(_.dest == Pos.B3).get.situationAfter
      val situation0b =
        board.variant.validMoves(situation0a)(Pos.E7).find(_.dest == Pos.E8).get.situationAfter
      val situation1a =
        board.variant.validMoves(situation0b)(Pos.B3).find(_.dest == Pos.B4).get.situationAfter
      val situation1b =
        board.variant.validMoves(situation1a)(Pos.E8).find(_.dest == Pos.E7).get.situationAfter
      val situation2a =
        board.variant.validMoves(situation1b)(Pos.B4).find(_.dest == Pos.B3).get.situationAfter
      val situation2b =
        board.variant.validMoves(situation2a)(Pos.E7).find(_.dest == Pos.E8).get.situationAfter
      val situation3a =
        board.variant.validMoves(situation2b)(Pos.B3).find(_.dest == Pos.B4).get.situationAfter
      val situation3b =
        board.variant.validMoves(situation3a)(Pos.E8).find(_.dest == Pos.E7).get.situationAfter
      val situation4a =
        board.variant.validMoves(situation3b)(Pos.B4).find(_.dest == Pos.B3).get.situationAfter

      situation3b.end must_== false

      situation4a.end must_== true
      situation4a.playable(true) must_== false
      situation4a.status must_== Some(Status.Draw)
      situation4a.winner must_== None
    }

    "Trigger draw by 50 moves kings repetition" in {
      //https://playstrategy.org/Ge85gdyL#209
      val board       = Board(FEN("B:WKa1,Kg8,Kh8:BKb1,Kc1,Kd1,Ke1,Kf1,Kg1,g2,Kh1,Kh2:H0:F103").pieces, variant.Dameo)
      var situation   = Situation(board, P2)
      situation.end must_== false

      val vectorActionStrs = Vector(
        Vector("b1c2"),
        Vector("g8a8"),
        Vector("c2b1"),
        Vector("a8g8"),
        Vector("f1f2"),
        Vector("g8a8"),
        Vector("g2f1K"),//makes final king (106)
        Vector("a8g8"),
        Vector("f2c2"),
        Vector("g8a8"),
        Vector("c2f2"),
        Vector("a8g8"),
        Vector("f2c2"),
        Vector("g8a8"),
        Vector("h2d2"), //110
        Vector("a8g8"),
        Vector("d2f2"),
        Vector("g8a8"),
        Vector("c2d2"),
        Vector("a8g8"),
        Vector("d2c2"),
        Vector("g8a8"),
        Vector("f2e2"),
        Vector("a8g8"),
        Vector("e2g2"),
        Vector("g8a8"),
        Vector("g2h2"), //116
        Vector("a8g8"),
        Vector("c2e2"),
        Vector("g8a8"),
        Vector("e2g2"),
        Vector("a8g8"),
        Vector("g2e2"),
        Vector("g8a8"),
        Vector("e2d2"), //120
        Vector("a8g8"),
        Vector("h2f2"),
        Vector("g8a8"),
        Vector("d2b2"),
        Vector("a8g8"),
        Vector("b2e2"),
        Vector("g8a8"),
        Vector("f2h2"),
        Vector("a8g8"),
        Vector("e2d2"), //125
        Vector("g8a8"),
        Vector("h2g2"),
        Vector("a8g8"),
        Vector("d2c2"),
        Vector("g8a8"),
        Vector("g2e2"),
        Vector("a8g8"),
        Vector("c2b2"),
        Vector("g8a8"),
        Vector("e2f2"), //130
        Vector("a8g8"),
        //Vector("f2c2"), //drawing move?
        // Vector("g8a8"),
        // Vector("c2g2"), //132
      )

      // Play all moves in order
      for (actions <- vectorActionStrs) {
        val moveStr = actions.head
        val moveOpt = situation.board.variant.validMoves(situation).values.flatten.find(_.toUci.uci == moveStr)
        moveOpt must beSome
        situation = moveOpt.get.situationAfter
      }
      situation.end must_== false

      // One last move to trigger draw
      val s1 = situation.board.variant.validMoves(situation).values.flatten.find(_.toUci.uci == "f2c2").get.situationAfter

      // Now check for draw
      s1.end must_== true
      s1.playable(true) must_== false
      s1.status must_== Some(Status.Draw)
      s1.winner must_== None
    }
  }
}
