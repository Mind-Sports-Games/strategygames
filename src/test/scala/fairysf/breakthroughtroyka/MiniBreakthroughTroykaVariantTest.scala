package strategygames.fairysf

class MiniBreakthroughTroykaVariantTest extends FairySFTest {

  "MiniBreakthroughTroyka" should {

    "not have winner from start position" in {
      val game = Game.apply(variant.MiniBreakthroughTroyka)
      game.situation.board.apiPosition.gameEnd === false

      val game2 = game.playMoves()

      game2 .toOption must beSome.like { case g =>
        g.situation.board.apiPosition.gameEnd === false
        g.situation.winner === None
      }
    }

    "have a winner after P1 reaches 5th rank" in {
      val game  = Game.apply(variant.MiniBreakthroughTroyka)
      val game3 = game.playMoves(
        // @formatter:off        
        (Pos.A2, Pos.B3), (Pos.E4, Pos.D3),
        (Pos.B3, Pos.A4), (Pos.D3, Pos.E2),
        (Pos.A4, Pos.B5)
        // @formatter:on
      )
      game3 .toOption must beSome.like { case g =>
        g.situation.board.apiPosition.gameEnd === true
        g.situation.winner must beSome.like(winner => winner === P1)
      }
    }

    "have a winner after P2 reaches 1st rank" in {
      val game  = Game.apply(variant.MiniBreakthroughTroyka)
      val game3 = game.playMoves(
        // @formatter:off        
        (Pos.A2, Pos.B3), (Pos.E4, Pos.D3),
        (Pos.B3, Pos.A4), (Pos.D3, Pos.E2),
        (Pos.A1, Pos.A2), (Pos.E2, Pos.D1)
        // @formatter:on
      )
      game3 .toOption must beSome.like { case g =>
        g.situation.board.apiPosition.gameEnd === true
        g.situation.winner must beSome.like(winner => winner === P2)
      }
    }

    "have a winner after P2 lost all his pieces" in {
      val game  = Game.apply(variant.MiniBreakthroughTroyka)
      val game3 = game.playMoves(
        // @formatter:off
        (Pos.A2, Pos.B3), (Pos.A4, Pos.A3),
        (Pos.B2, Pos.A3), (Pos.C4, Pos.B3),
        (Pos.C2, Pos.B3), (Pos.B4, Pos.C3),
        (Pos.D2, Pos.C3), (Pos.A5, Pos.A4),
        (Pos.B3, Pos.A4), (Pos.B5, Pos.A4),
        (Pos.A3, Pos.B4), (Pos.C5, Pos.B4),
        (Pos.C3, Pos.D4), (Pos.E5, Pos.D4),
        (Pos.D1, Pos.D2), (Pos.E4, Pos.E3),
        (Pos.D2, Pos.E3), (Pos.D4, Pos.E3),
        (Pos.E1, Pos.D2), (Pos.E3, Pos.D2),
        (Pos.C1, Pos.D2), (Pos.D5, Pos.C4),
        (Pos.B1, Pos.C2), (Pos.C4, Pos.C3),
        (Pos.D2, Pos.C3), (Pos.B4, Pos.C3),
        (Pos.E2, Pos.D3), (Pos.C3, Pos.B2),
        (Pos.A1, Pos.B2), (Pos.A4, Pos.B3),
        (Pos.C2, Pos.B3)
        // @formatter:on
      )
      game3 .toOption must beSome.like { case g =>
        g.situation.board.apiPosition.gameEnd === true
        g.situation.winner must beSome.like(winner => winner === P1)
      }
    }
  }
}
