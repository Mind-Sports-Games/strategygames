package strategygames.togyzkumalak

import org.specs2.matcher.ValidatedMatchers

import strategygames.{ Player, Score }
import strategygames.format.{ FEN => StratFen, Forsyth => StratForsyth, Uci => StratUci }
import strategygames.variant.{ Variant => StratVariant }
import variant.Togyzkumalak

class TogyzkumalakVariantTest extends TogyzkumalakTest with ValidatedMatchers {

  "valid moves in initial situation" should {
    val board     = Board.init(variant.Togyzkumalak)
    val situation = Situation(board, Player.P1)

    val moves = variant.Togyzkumalak.validMoves(situation)
    "be valid" in {
      moves.size === 9
      board.valid(true) === true
    }
  }

  "valid opening moves" should {
    "valid situation after first move" in {
      playActionStrs(List("e1f2")) .toOption must beSome.like { case g =>
        g.situation.player === Player.P2
        g.situation.moves.size === 8
        g.situation.board.history.score === Score(10, 0)
        g.situation.end === false
        g.situation.board.valid(true) === true
      }
    }
    "valid situation after first two moves" in {
      playActionStrs(List("e1f2", "e2d1")) .toOption must beSome.like { case g =>
        g.situation.player === Player.P1
        g.situation.moves.size === 8
        g.situation.board.history.score === Score(10, 10)
        g.situation.board.valid(true) === true
      }
    }
  }

  "tuzdik rules are respected" should {
    val actionStrs  = List("f1e2", "d2e1", "i1a2", "b2i1", "b1g2", "f2e1")
    "no tuzdiks initially" in {
      playActionStrs(actionStrs.dropRight(1)) .toOption must beSome.like { case g =>
        g.situation.player === Player.P2
        g.situation.board.pieces.filter {
          case (_, (p, _)) if p.role == Tuzdik => true; case _ => false
        }.size === 0
        g.situation.board.pieces(Pos.E1) === ((Piece(Player.P1, Stone), 2))
        g.situation.board.history.score === Score(22, 12)
        g.situation.board.valid(true) === true
      }
    }
    "tuzdik created when landing on space with 2 stones" in {
      playActionStrs(actionStrs) .toOption must beSome.like { case g =>
        g.situation.player === Player.P1
        g.situation.board.pieces
          .filter {
            case (_, (p, _)) if p.role == Tuzdik => true; case _ => false
          } === Map(Pos.E1 -> ((Piece(Player.P2, Tuzdik), 1)))
        g.situation.board.history.score === Score(22, 15)
        g.situation.board.valid(true) === true
      }
    }
    "second tuzdik not created when landing on second space with 2 stones" in {
      playActionStrs(actionStrs ++ List("c1d2")) .toOption must beSome.like { case g =>
        g.situation.player === Player.P2
        g.situation.board.pieces(Pos.I1) === ((Piece(Player.P1, Stone), 2))
        g.situation.board.valid(true) === true
      }
      playActionStrs(actionStrs ++ List("c1d2", "c2i1")) .toOption must beSome.like { case g =>
        g.situation.player === Player.P1
        g.situation.board.pieces
          .filter {
            case (_, (p, _)) if p.role == Tuzdik => true; case _ => false
          } === Map(Pos.E1 -> ((Piece(Player.P2, Tuzdik), 1)))
        g.situation.board.valid(true) === true
      }
    }
    val actionStrs2 = List(
      "f1e2",
      "d2e1",
      "i1a2",
      "b2i1",
      "b1g2",
      "f2e1",
      "c1d2",
      "i2d1",
      "a1f2",
      "i2h2",
      "a1b1",
      "g2e2",
      "c1e1",
      "e2a2",
      "h1c1",
      "h2i1",
      "i1f2",
      "i2g2",
      "f1h2",
      "i2h2",
      "i1i2",
      "g2c2",
      "i1i2",
      "e2b2",
      "a1b1",
      "i2h2",
      "h1i2",
      "i2h2"
    )
    "tuzdik not created when landing on two stones on our side" in {
      playActionStrs(actionStrs2.dropRight(1)) .toOption must beSome.like { case g =>
        g.situation.player === Player.P2
        g.situation.board.pieces
          .filter {
            case (_, (p, _)) if p.role == Tuzdik => true; case _ => false
          } === Map(Pos.E1 -> ((Piece(Player.P2, Tuzdik), 1)))
        g.situation.board.pieces(Pos.H2) === ((Piece(Player.P2, Stone), 2))
        g.situation.board.valid(true) === true
      }
      playActionStrs(actionStrs2) .toOption must beSome.like { case g =>
        g.situation.player === Player.P1
        g.situation.board.pieces
          .filter {
            case (_, (p, _)) if p.role == Tuzdik => true; case _ => false
          } === Map(Pos.E1 -> ((Piece(Player.P2, Tuzdik), 1)))
        g.situation.board.pieces(Pos.H2) === ((Piece(Player.P2, Stone), 3))
        g.situation.board.valid(true) === true
      }
    }
  }

  "game ends properly" should {
    // https://playstrategy.org/FgWSk5be
    val actionStrs = List(
      "g1d2",
      "a2h1",
      "f1d2",
      "c2f1",
      "a1h2",
      "b2h1",
      "b1e2",
      "b2a2",
      "i1c1",
      "g2f1",
      "d1a2",
      "f2i1",
      "h1i2",
      "h2f2",
      "i1g2",
      "d2a1",
      "e1c1",
      "b2d1",
      "g1d2",
      "a2i2",
      "i1g2",
      "i2e2",
      "a1c1",
      "a2a1",
      "h1h2",
      "c2d1",
      "a1b1",
      "b2a2",
      "b1h2",
      "f2b2",
      "b1c1",
      "c2b2",
      "f1i2",
      "c2b2",
      "f1g1",
      "a2a1",
      "a1b1",
      "b2b1",
      "a1b1",
      "b2a2",
      "i1g2",
      "h2g2",
      "h1i2",
      "a2b1",
      "a1b1",
      "e2c1",
      "a1b1",
      "b2a2",
      "b1d1",
      "a2b1",
      "a1b1",
      "g2b2",
      "i1i2",
      "i2h2",
      "g1h2",
      "i2h2",
      "g1h1",
      "h2g2",
      "h1i2",
      "f2e2",
      "e1g1",
      "b2a2",
      "g1h1",
      "c2b2",
      "f1g1",
      "c2b2",
      "g1h1",
      "b2a2",
      "h1i2",
      "b2a2",
      "c1i2",
      "b2a2",
      "h1i2",
      "i2h2",
      "i1d2"
    )
    "when a player has > 81 stones" in {
      playActionStrs(actionStrs) .toOption must beSome.like { case g =>
        g.situation.end === true
        g.situation.winner === Some(Player.P1)
        g.situation.board.history.score === Score(82, 43)
        g.situation.board.valid(true) === true
      }
    }
  }

}

class TogyzkumalakVariantTestIsometry extends strategygames.chess.ChessTest {
  "Test Every move can be loaded from fen" in {
    val gameFamily   = Togyzkumalak.gameFamily
    val lib          = gameFamily.gameLogic
    val stratVariant = StratVariant(lib, Togyzkumalak.key).get

    _testEveryMoveLoadFenIsometry(lib, StratFen(lib, Togyzkumalak.initialFen.value), stratVariant)(
      List(
        "g1d2",
        "a2h1",
        "f1d2",
        "c2f1",
        "a1h2",
        "b2h1",
        "b1e2",
        "b2a2",
        "i1c1",
        "g2f1",
        "d1a2",
        "f2i1",
        "h1i2",
        "h2f2",
        "i1g2",
        "d2a1",
        "e1c1",
        "b2d1",
        "g1d2",
        "a2i2",
        "i1g2",
        "i2e2",
        "a1c1",
        "a2a1",
        "h1h2",
        "c2d1",
        "a1b1",
        "b2a2",
        "b1h2",
        "f2b2",
        "b1c1",
        "c2b2",
        "f1i2",
        "c2b2",
        "f1g1",
        "a2a1",
        "a1b1",
        "b2b1",
        "a1b1",
        "b2a2",
        "i1g2",
        "h2g2",
        "h1i2",
        "a2b1",
        "a1b1",
        "e2c1",
        "a1b1",
        "b2a2",
        "b1d1",
        "a2b1",
        "a1b1",
        "g2b2",
        "i1i2",
        "i2h2",
        "g1h2",
        "i2h2",
        "g1h1",
        "h2g2",
        "h1i2",
        "f2e2",
        "e1g1",
        "b2a2",
        "g1h1",
        "c2b2",
        "f1g1",
        "c2b2",
        "g1h1",
        "b2a2",
        "h1i2",
        "b2a2",
        "c1i2",
        "b2a2",
        "h1i2",
        "i2h2",
        "i1d2"
      ).map(uciStr => StratUci(lib, gameFamily, uciStr).get)
    ) .toOption must beSome.like { case gameData =>
      val fen1 = StratForsyth.>>(lib, gameData.game)
      val fen2 = StratForsyth.>>(lib, gameData.fenGame)
      fen1 === fen2
    }
  }
}
