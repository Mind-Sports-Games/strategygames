package strategygames.entropy

class EntropyForcedActionTest extends EntropyTest {

  "at the start of the game" should {
    val game = Game.apply(variant.Entropy)

    "have P1 as Chaos and P2 as Order" in {
      game.situation.chaosPlayer === P1
      game.situation.orderPlayer === P2
      game.situation.player === P1
      game.situation.isChaos must beTrue
    }

    "require Chaos to draw before it can place" in {
      game.situation.mustDraw must beTrue
      game.situation.dropsAsDrops must beEmpty
    }

    "force a draw when Chaos has run out of time" in {
      game.situation.flaggedAction must beSome[Action].like { case dc: DrawCounter =>
        dc.role must beOneOf(Role.all: _*)
      }
    }
  }

  "a flagged Chaos that has already drawn" should {
    "be forced onto the first empty square in a1-to-g7 order" in {
      val drawn = game0.situation.drawCounter(Red).toOption.get
      val after = game0.apply(drawn)

      after.situation.flaggedAction must beSome[Action].like { case d: Drop =>
        d.pos === Pos.A1
      }
    }

    "sweep along the first rank as squares fill" in {
      // a1 and b1 already taken, so the next forced placement is c1
      val board     = Board(format.FEN("7/7/7/7/7/7/RG5[Y] w 0 0 1 1").pieces, variant.Entropy)
        .withPocketData(
          PocketData(
            strategygames.Pockets(
              strategygames.Pocket(List(strategygames.Role.EntropyRole(Yellow))),
              strategygames.Pocket(Nil)
            )
          )
        )
      val situation = Situation(board, P1)

      situation.flaggedAction must beSome[Action].like { case d: Drop =>
        d.pos === Pos.C1
      }
    }
  }

  "a flagged Order" should {
    "be forced to pass, and never to move" in {
      val drawn   = game0.situation.drawCounter(Red).toOption.get
      val placed  = game0.apply(drawn)
      val dropped = placed.apply(placed.situation.drop(Red, Pos.D4).toOption.get)

      dropped.situation.player === P2
      dropped.situation.isOrder must beTrue
      dropped.situation.moves must not(beEmpty)
      dropped.situation.flaggedAction must beSome[Action].like { case _: Pass => ok }
    }
  }

  private def game0 = Game.apply(variant.Entropy)
}
