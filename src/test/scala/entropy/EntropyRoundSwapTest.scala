package strategygames.entropy

class EntropyRoundSwapTest extends EntropyTest {

  "a game played out entirely by forced actions" should {
    val finished = playForced(Game.apply(variant.Entropy))

    "reach the end of both rounds" in {
      finished.situation.end must beTrue
      finished.situation.board.round === 3
    }

    "finish with a variant end, never a timeout" in {
      finished.situation.status === Some(strategygames.Status.VariantEnd)
    }

    "give both players a score, since each was Order for one round" in {
      val score = finished.situation.board.history.score
      score.p1 must be_>(0)
      score.p2 must be_>(0)
    }

    "decide the winner on the score alone" in {
      val score = finished.situation.board.history.score
      finished.situation.winner === (
        if (score.p1 == score.p2) None
        else Some(if (score.p1 > score.p2) P1 else P2)
      )
    }
  }

  "the board" should {
    "be full at the moment a round closes" in {
      // step until the round number changes, then look at the board just before
      val start = Game.apply(variant.Entropy)
      val games = Iterator
        .iterate(start)(g => g.situation.flaggedAction.fold(g)(a => applyAction(g, a)))
        .take(400)
        .toList

      val beforeSwap = games.zip(games.tail).find { case (a, b) =>
        a.situation.board.round == 1 && b.situation.board.round == 2
      }

      beforeSwap must beSome[(Game, Game)].like { case (before, after) =>
        before.situation.board.isFull must beTrue
        // the next round starts from nothing
        after.situation.board.pieces must beEmpty
        after.situation.board.chaosPlayer === P2
        after.situation.board.orderPlayer === P1
      }
    }

    "bank the first round's score to the player who was Order for it" in {
      val start = Game.apply(variant.Entropy)
      val games = Iterator
        .iterate(start)(g => g.situation.flaggedAction.fold(g)(a => applyAction(g, a)))
        .take(400)
        .toList

      val afterSwap = games.find(_.situation.board.round == 2)

      afterSwap must beSome[Game].like { case g =>
        // P2 was Order in round one, so only P2 has banked anything
        g.situation.board.history.score.p2 must be_>(0)
        g.situation.board.history.score.p1 === 0
      }
    }
  }
}
