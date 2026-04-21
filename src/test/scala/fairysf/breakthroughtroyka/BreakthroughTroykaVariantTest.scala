package strategygames.fairysf

class BreakthroughTroykaVariantTest extends FairySFTest {

  "BreakthroughTroyka" should {

    "not have winner from start position" in {
      val game = Game.apply(variant.BreakthroughTroyka)
      game.situation.board.apiPosition.gameEnd === false

      val game2 = game.playMoves();

      game2 .toOption must beSome.like { case g =>
        g.situation.board.apiPosition.gameEnd === false
        g.situation.winner === None
      }
    }

    "have a winner after P1 reaches 8th rank" in {
      val game  = Game.apply(variant.BreakthroughTroyka)
      val game3 = game.playMoves(
        // @formatter:off
          (Pos.A2, Pos.A3), (Pos.A7, Pos.B6),
          (Pos.A3, Pos.A4), (Pos.B6, Pos.C5),
          (Pos.A4, Pos.A5), (Pos.A8, Pos.A7),
          (Pos.A5, Pos.A6), (Pos.A7, Pos.B6),
          (Pos.A6, Pos.A7), (Pos.B7, Pos.A6),
          (Pos.A7, Pos.A8)
        // @formatter:on
      )
      game3 .toOption must beSome.like { case g =>
        g.situation.board.apiPosition.gameEnd === true
        g.situation.winner must beSome.like(winner => winner === P1)
      }
    }

    "have a winner after P2 reaches 1st rank" in {
      val game  = Game.apply(variant.BreakthroughTroyka)
      val game3 = game.playMoves(
        // @formatter:off
          (Pos.E2, Pos.E3), (Pos.A7, Pos.B6),
          (Pos.D2, Pos.D3), (Pos.B6, Pos.C5),
          (Pos.D3, Pos.D4), (Pos.C5, Pos.D4),
          (Pos.D1, Pos.E2), (Pos.D4, Pos.E3),
          (Pos.E2, Pos.F3), (Pos.E3, Pos.F2),
          (Pos.E1, Pos.D2), (Pos.F2, Pos.G1)
        // @formatter:on
      )
      game3 .toOption must beSome.like { case g =>
        g.situation.board.apiPosition.gameEnd === true
        g.situation.winner must beSome.like(winner => winner === P2)
      }
    }

    "have a winner after P1 lost all his pieces" in {
      val game  = Game.apply(variant.BreakthroughTroyka)
      val game3 = game.playMoves(
        // @formatter:off
          (Pos.B2, Pos.B3), (Pos.F7, Pos.F6),
          (Pos.A2, Pos.A3), (Pos.B7, Pos.B6),
          (Pos.A3, Pos.A4), (Pos.B6, Pos.B5),
          (Pos.D2, Pos.E3), (Pos.B5, Pos.A4),
          (Pos.E2, Pos.D3), (Pos.A4, Pos.B3),
          (Pos.D3, Pos.D4), (Pos.B3, Pos.C2),
          (Pos.B1, Pos.C2), (Pos.F6, Pos.E5),
          (Pos.C1, Pos.B2), (Pos.E5, Pos.D4),
          (Pos.B2, Pos.B3), (Pos.D4, Pos.E3),
          (Pos.A1, Pos.B2), (Pos.E3, Pos.F2),
          (Pos.G1, Pos.F2), (Pos.A7, Pos.A6),
          (Pos.B2, Pos.A3), (Pos.A6, Pos.B5),
          (Pos.A3, Pos.A4), (Pos.B5, Pos.A4),
          (Pos.E1, Pos.D2), (Pos.A4, Pos.B3),
          (Pos.D2, Pos.D3), (Pos.B3, Pos.C2),
          (Pos.D1, Pos.C2), (Pos.D7, Pos.D6),
          (Pos.F2, Pos.E3), (Pos.D6, Pos.D5),
          (Pos.E3, Pos.E4), (Pos.D5, Pos.E4),
          (Pos.F1, Pos.E2), (Pos.E4, Pos.D3),
          (Pos.C2, Pos.D3), (Pos.C7, Pos.C6),
          (Pos.D3, Pos.D4), (Pos.C6, Pos.C5),
          (Pos.E2, Pos.E3), (Pos.C5, Pos.D4),
          (Pos.G2, Pos.F3), (Pos.D4, Pos.E3),
          (Pos.H1, Pos.G2), (Pos.E7, Pos.E6),
          (Pos.G2, Pos.G3), (Pos.E6, Pos.E5),
          (Pos.F3, Pos.E4), (Pos.E5, Pos.F4),
          (Pos.E4, Pos.F5), (Pos.F4, Pos.G3),
          (Pos.F5, Pos.F6), (Pos.G3, Pos.H2),
          (Pos.F6, Pos.G7), (Pos.H8, Pos.G7),
        // @formatter:on
      )
      game3 .toOption must beSome.like { case g =>
        g.situation.board.apiPosition.gameEnd === true
        g.situation.winner must beSome.like(winner => winner === P2)
      }
    }
  }
}
