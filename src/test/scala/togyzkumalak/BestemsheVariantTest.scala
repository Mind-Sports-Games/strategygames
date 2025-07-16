package strategygames.togyzkumalak

import org.specs2.matcher.ValidatedMatchers

import strategygames.{ Player, Score }

import strategygames.togyzkumalak.variant.Bestemshe

class BestemsheVariantTest extends TogyzkumalakTest with ValidatedMatchers {

  "valid moves in initial situation" should {
    val board     = Board.init(Bestemshe)
    val situation = Situation(board, Player.P1)

    val moves = Bestemshe.validMoves(situation)
    "be valid" in {
      moves.size must_== 5
      board.valid(true) must_== true
    }
  }

  "valid opening moves" should {
    "valid situation after first move" in {
      playActionStrs(List("c1d2"), variant = Some(Bestemshe)) must beValid.like { g =>
        g.situation.player must_== Player.P2
        g.situation.moves.size must_== 4
        g.situation.board.history.score must_== Score(6, 0)
        g.situation.end must_== false
        g.situation.board.valid(true) must_== true
      }
    }
    "valid situation after first two moves" in {
      playActionStrs(List("c1d2", "c2b1"), variant = Some(Bestemshe)) must beValid.like { g =>
        g.situation.player must_== Player.P1
        g.situation.moves.size must_== 4
        g.situation.board.history.score must_== Score(6, 6)
        g.situation.board.valid(true) must_== true
      }
    }
  }

  "no tuzdik created when creating a pile of 3 stones" should {
    val actionStrs =
      List("c1d2", "c2b1", "c1d1", "b2d1", "e1a2", "e2b1", "c1d1", "c2a2", "e1e2", "d2c2", "a1c2")
    "no tuzdiks initially" in {
      playActionStrs(actionStrs, variant = Some(Bestemshe)) must beValid.like { g =>
        g.situation.player must_== Player.P2
        g.situation.board.pieces.filter {
          case (_, (p, _)) if p.role == Tuzdik => true; case _ => false
        }.size must_== 0
        g.situation.board.pieces(Pos.C2) must_== ((Piece(Player.P2, Stone), 3))
        g.situation.board.history.score must_== Score(16, 16)
        g.situation.board.valid(true) must_== true
      }
    }
  }

  "game ends properly" should {
    val actionStrs = List(
      "c1d2",
      "b2c1",
      "e1a2",
      "e2b1",
      "a1d2",
      "c2d1",
      "b1a2",
      "e2c2",
      "e1d2",
      "c2a2",
      "e1e2",
      "b2d1",
      "c1e2"
    )
    "when a player has > 25 stones" in {
      playActionStrs(actionStrs, variant = Some(Bestemshe)) must beValid.like { g =>
        g.situation.end must_== true
        g.situation.winner must_== Some(Player.P1)
        g.situation.board.history.score must_== Score(26, 12)
        g.situation.board.valid(true) must_== true
      }
    }
  }

}
