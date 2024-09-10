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
    }
  }

  "valid opening moves" should {
    "valid situation after first move" in {
      playActionStrs(List("c1d2"), variant = Some(Bestemshe)) must beValid.like { g =>
        g.situation.player must_== Player.P2
        g.situation.moves.size must_== 4
        g.situation.board.history.score must_== Score(6, 0)
        g.situation.end must_== false
      }
    }
    "valid situation after first two moves" in {
      playActionStrs(List("c1d2", "c2b1"), variant = Some(Bestemshe)) must beValid.like { g =>
        g.situation.player must_== Player.P1
        g.situation.moves.size must_== 4
        g.situation.board.history.score must_== Score(6, 6)
      }
    }
  }

  // no tuzdik created
  // "tuzdik rules are respected" should {
  //  val actionStrs  = List("f1e2", "d2e1", "i1a2", "b2i1", "b1g2", "f2e1")
  //  "no tuzdiks initially" in {
  //    playActionStrs(actionStrs.dropRight(1)) must beValid.like { g =>
  //      g.situation.player must_== Player.P2
  //      g.situation.board.pieces.filter {
  //        case (_, (p, _)) if p.role == Tuzdik => true; case _ => false
  //      }.size must_== 0
  //      g.situation.board.pieces(Pos.E1) must_== ((Piece(Player.P1, Stone), 2))
  //      g.situation.board.history.score must_== Score(22, 12)
  //    }
  //  }
  //  "tuzdik created when landing on space with 2 stones" in {
  //    playActionStrs(actionStrs) must beValid.like { g =>
  //      g.situation.player must_== Player.P1
  //      g.situation.board.pieces
  //        .filter {
  //          case (_, (p, _)) if p.role == Tuzdik => true; case _ => false
  //        } must_== Map(Pos.E1 -> ((Piece(Player.P2, Tuzdik), 1)))
  //      g.situation.board.history.score must_== Score(22, 15)
  //    }
  //  }
  // }

  // "game ends properly" should {
  //  // https://playstrategy.org/FgWSk5be
  //  val actionStrs = List(
  //  )
  //  "when a player has > 25 stones" in {
  //    playActionStrs(actionStrs) must beValid.like { g =>
  //      g.situation.end must_== true
  //      g.situation.winner must_== Some(Player.P1)
  //      g.situation.board.history.score must_== Score(82, 43)
  //    }
  //  }
  // }

}
