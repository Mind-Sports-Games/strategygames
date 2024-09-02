package strategygames.backgammon

import org.specs2.matcher.ValidatedMatchers

import strategygames.backgammon.variant.Backgammon

class BackgammonReplayTest extends BackgammonTest with ValidatedMatchers {

  "Backgammon replay" should {
    "match play through of game" in {
      val vectorActionStrs = Vector(
        Vector("endturn"),
        Vector("3/1", "e2h2", "g2h2", "endturn"),
        Vector("5/3", "e1j1", "l2i2", "endturn"),
        Vector("4/4", "e2i2", "e2i2", "a1d2", "a1d2", "endturn"),
        Vector("3/1", "s@j2", "g1h1", "endturn"),
        Vector("1/6", "g2h2", "l1f1", "endturn"),
        Vector("5/5", "g1l1", "j2e2", "a2e1", "a2e1", "endturn"),
        Vector("1/2", "s@l1", "l1j1", "endturn"),
        Vector("6/5", "endturn"),
        Vector("5/1", "f1a1", "j1i1", "endturn"),
        Vector("5/3", "s@j2", "endturn"),
        Vector("2/4", "a1b2", "b2f2", "endturn"),
        Vector("1/3", "s@j2", "e1f1", "endturn"),
        Vector("3/1", "i1f1", "d2e2", "endturn"),
        Vector("1/3", "s@j2", "s@l2", "endturn"),
        Vector("5/4", "f2k2", "g2k2", "endturn"),
        Vector("2/5", "l2j2", "e1j1", "endturn"),
        Vector("4/1", "e2i2", "d2e2", "endturn"),
        Vector("6/6", "j2d2", "j2d2", "j2d2", "d2c1", "endturn"),
        Vector("1/3", "f1c1", "c1b1", "endturn"),
        Vector("6/6", "endturn"),
        Vector("5/2", "a1e2", "a1b2", "endturn"),
        Vector("4/2", "endturn"),
        Vector("2/5", "h2j2", "e2j2", "endturn"),
        Vector("1/5", "s@l2", "endturn"),
        Vector("6/4", "b2h2", "b1c2", "endturn"),
        Vector("2/6", "endturn"),
        Vector("3/6", "a1c2", "e2k2", "endturn"),
        Vector("6/3", "endturn"),
        Vector("5/6", "c2h2", "c2i2", "endturn"),
        Vector("4/3", "endturn"),
        Vector("6/4", "^g2", "g2k2", "endturn"),
        Vector("3/5", "endturn"),
        Vector("3/1", "h2k2", "i2j2", "endturn"),
        Vector("4/5", "endturn"),
        Vector("1/4", "^i2", "h2i2", "endturn"),
        Vector("2/4", "endturn"),
        Vector("5/6", "^h2", "^h2", "endturn"),
        Vector("6/4", "s@g2", "l2h2", "endturn"),
        Vector("4/5", "^i2", "^i2", "endturn"),
        Vector("5/3", "e1j1", "l2i2", "endturn"),
        Vector("4/2", "s@i1", "j2l2", "endturn"),
        Vector("6/4", "e1i1", "h2b2", "endturn"),
        Vector("6/6", "endturn"),
        Vector("1/1", "g1h1", "h1i1", "a2a1", "a2a1", "endturn"),
        Vector("5/3", "s@h1", "h1e1", "endturn"),
        Vector("6/2", "s@g2", "d2b2", "endturn"),
        Vector("1/6", "e1d1", "d1c2", "endturn"),
        Vector("2/2", "g2e2", "e2c2", "a1c1", "a1c1", "endturn"),
        Vector("2/6", "s@k1", "k1e1", "endturn"),
        Vector("1/5", "a2e1", "g2f2", "endturn"),
        Vector("4/6", "endturn"),
        Vector("1/4", "b2a2", "b2c1", "endturn"),
        Vector("1/1", "s@l1", "k2l2", "l1k1", "k2l2", "endturn"),
        Vector("1/6", "e1k1", "c1d1", "endturn"),
        Vector("6/4", "endturn"),
        Vector("6/4", "a2f1", "f1j1", "endturn"),
        Vector("1/5", "s@h1", "k2l2", "endturn"),
        Vector("3/5", "c1h1", "h1k1", "endturn"),
        Vector("4/6", "endturn"),
        Vector("4/4", "c2b1", "d2a1", "f2b2", "b2c1", "endturn"),
        Vector("3/6", "endturn"),
        Vector("5/4", "c1h1", "d1h1", "endturn"),
        Vector("5/1", "s@l1", "endturn"),
        Vector("2/1", "a1c1", "i2h2", "endturn"),
        Vector("5/5", "endturn"),
        Vector("2/5", "h2f2", "b1g1", "endturn"),
        Vector("2/6", "l1f1", "f1d1", "endturn"),
        Vector("1/1", "c1d1", "c1d1", "f2e2", "e2d2", "endturn"),
        Vector("5/1", "s@l1", "endturn"),
        Vector("1/1", "d1e1", "d1e1", "e1f1", "e1f1", "endturn"),
        Vector("3/3", "endturn"),
        Vector("1/4", "d2c2", "c2b1", "endturn"),
        Vector("4/5", "endturn"),
        Vector("2/4", "b1d1", "d1h1", "endturn"),
        Vector("2/4", "j2l2", "endturn"),
        Vector("2/3", "f1h1", "f1i1", "endturn"),
        Vector("1/4", "j2k2", "endturn"),
        Vector("6/3", "^g1", "^j1", "endturn"),
        Vector("4/3", "endturn"),
        Vector("1/6", "g1h1", "^g1", "endturn"),
        Vector("3/4", "endturn"),
        Vector("6/3", "^h1", "i1l1", "endturn"),
        Vector("4/2", "endturn"),
        Vector("1/5", "^l1", "^h1", "endturn"),
        Vector("6/2", "s@g1", "g1e1", "endturn"),
        Vector("6/1", "^h1", "k1l1", "endturn"),
        Vector("2/3", "e1c1", "c1a2", "endturn"),
        Vector("1/4", "^l1", "^i1", "endturn"),
        Vector("1/5", "a2b2", "b2g2", "endturn"),
        Vector("5/6", "^h1", "^h1", "endturn"),
        Vector("3/6", "^g2", "^k2", "endturn"),
        Vector("1/4", "k1l1", "^i1", "endturn"),
        Vector("6/3", "^k2", "^k2", "endturn"),
        Vector("4/1", "^j1", "^l1", "endturn"),
        Vector("3/6", "^l2", "^l2", "endturn"),
        Vector("3/1", "^j1")
      )
      playActionStrs(vectorActionStrs.flatten.toList) must beValid.like { g =>
        val replay = Replay
          .gameWithUciWhileValid(
            vectorActionStrs,
            Backgammon.initialFen,
            Backgammon
          )
          ._2
          .reverse
          .head
          ._1
        g.plies must_== replay.plies
        g.turnCount must_== replay.turnCount
        g.startedAtPly must_== replay.startedAtPly
        g.startedAtTurn must_== replay.startedAtTurn
        // replay actionStrs muddles up lift dice when it is irrelevant
        // g.actionStrs must_== replay.actionStrs
        g.situation.board.pieces must_== replay.situation.board.pieces
        // g.situation.board.history.lastTurn must_== replay.situation.board.history.lastTurn
        g.situation.board.history.currentTurn must_== replay.situation.board.history.currentTurn
        g.situation.board.history.score must_== replay.situation.board.history.score
        g.situation.board.variant must_== replay.situation.board.variant
        g.situation.board.pocketData must_== replay.situation.board.pocketData
        g.situation.board.unusedDice must_== replay.situation.board.unusedDice
      }
    }

  }

}
