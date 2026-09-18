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
    "close as its last empty square is filled" in {
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
        // one square left, which the closing drop fills
        before.situation.board.emptyPositions.size === 1
        // the finished board stays up, so the position shows what the round was scored on
        after.situation.board.isFull must beTrue
        after.situation.board.chaosPlayer === P2
        after.situation.board.orderPlayer === P1
      }
    }

    "be cleared by the next round's first draw, which still draws from a full bag" in {
      val start = Game.apply(variant.Entropy)
      val games = Iterator
        .iterate(start)(g => g.situation.flaggedAction.fold(g)(a => applyAction(g, a)))
        .take(400)
        .toList

      val firstDrawOfRoundTwo = games.zip(games.tail).find { case (a, b) =>
        a.situation.board.round == 2 && a.situation.board.isFull && !b.situation.board.isFull
      }

      firstDrawOfRoundTwo must beSome[(Game, Game)].like { case (before, after) =>
        before.situation.mustDraw must beTrue
        after.situation.board.pieces must beEmpty
        after.situation.board.counterInPocket(P2) must beSome[Role]
        after.situation.board.variant.bag(after.situation.board).size === 48
        after.turnCount === before.turnCount
      }
    }

    "close on the drop that fills it, handing the turn to the next round's Chaos" in {
      val start = Game.apply(variant.Entropy)
      val games = Iterator
        .iterate(start)(g => g.situation.flaggedAction.fold(g)(a => applyAction(g, a)))
        .take(400)
        .toList

      val swap = games.zip(games.tail).find { case (a, b) =>
        a.situation.board.round == 1 && b.situation.board.round == 2
      }

      swap must beSome[(Game, Game)].like { case (before, after) =>
        before.situation.isChaos must beTrue
        after.situation.player === P2
        after.situation.isChaos must beTrue
        after.situation.mustDraw must beTrue
        after.turnCount === 97
      }
    }

    "never offer Order a turn on a full board" in {
      val finished = playForced(Game.apply(variant.Entropy))
      val strs     = finished.actionStrs.toList.flatMap(v => v)
      strs.filter((a: String) => a == "pass").size === 96
      finished.turnCount === 194
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
