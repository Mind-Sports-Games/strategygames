package strategygames.togyzkumalak

// import format.Uci
import org.specs2.matcher.ValidatedMatchers

import strategygames.Player
import strategygames.togyzkumalak.variant.Togyzkumalak

class TogyzkumalakReplayTest extends TogyzkumalakTest with ValidatedMatchers {

  "Togyzkumalak replay" should {
    "match play through regarding tuzdiks" in {
      val vectorActionStrs = Vector(
        Vector("f1e2"),
        Vector("d2e1"),
        Vector("i1a2"),
        Vector("b2i1"),
        Vector("b1g2"),
        Vector("f2e1"),
        Vector("c1d2"),
        Vector("i2d1"),
        Vector("a1f2"),
        Vector("i2h2"),
        Vector("a1b1"),
        Vector("g2e2"),
        Vector("c1e1"),
        Vector("e2a2"),
        Vector("h1c1"),
        Vector("h2i1"),
        Vector("i1f2"),
        Vector("i2g2"),
        Vector("f1h2"),
        Vector("i2h2"),
        Vector("i1i2"),
        Vector("g2c2"),
        Vector("i1i2"),
        Vector("e2b2"),
        Vector("a1b1"),
        Vector("i2h2"),
        Vector("h1i2"),
        Vector("i2h2")
      )
      playActionStrs(vectorActionStrs.flatten[String].toList) .toOption must beSome.like { case g =>
        val replay = Replay
          .gameWithUciWhileValid(
            vectorActionStrs,
            Player.P1,
            Player.fromTurnCount(vectorActionStrs.size),
            Togyzkumalak.initialFen,
            Togyzkumalak
          )
          ._2
          .reverse
          .head
          ._1
        g.plies === replay.plies
        g.turnCount === replay.turnCount
        g.startedAtPly === replay.startedAtPly
        g.startedAtTurn === replay.startedAtTurn
        g.actionStrs === replay.actionStrs
        g.situation.board.pieces === replay.situation.board.pieces
        g.situation.board.history.score === replay.situation.board.history.score
        g.situation.board.variant === replay.situation.board.variant
        g.situation.board.valid(true) === true
      }
    }

    "full game replay matches gameplay" in {
      // https://playstrategy.org/FgWSk5be
      val vectorActionStrs = Vector(
        Vector("g1d2"),
        Vector("a2h1"),
        Vector("f1d2"),
        Vector("c2f1"),
        Vector("a1h2"),
        Vector("b2h1"),
        Vector("b1e2"),
        Vector("b2a2"),
        Vector("i1c1"),
        Vector("g2f1"),
        Vector("d1a2"),
        Vector("f2i1"),
        Vector("h1i2"),
        Vector("h2f2"),
        Vector("i1g2"),
        Vector("d2a1"),
        Vector("e1c1"),
        Vector("b2d1"),
        Vector("g1d2"),
        Vector("a2i2"),
        Vector("i1g2"),
        Vector("i2e2"),
        Vector("a1c1"),
        Vector("a2a1"),
        Vector("h1h2"),
        Vector("c2d1"),
        Vector("a1b1"),
        Vector("b2a2"),
        Vector("b1h2"),
        Vector("f2b2"),
        Vector("b1c1"),
        Vector("c2b2"),
        Vector("f1i2"),
        Vector("c2b2"),
        Vector("f1g1"),
        Vector("a2a1"),
        Vector("a1b1"),
        Vector("b2b1"),
        Vector("a1b1"),
        Vector("b2a2"),
        Vector("i1g2"),
        Vector("h2g2"),
        Vector("h1i2"),
        Vector("a2b1"),
        Vector("a1b1"),
        Vector("e2c1"),
        Vector("a1b1"),
        Vector("b2a2"),
        Vector("b1d1"),
        Vector("a2b1"),
        Vector("a1b1"),
        Vector("g2b2"),
        Vector("i1i2"),
        Vector("i2h2"),
        Vector("g1h2"),
        Vector("i2h2"),
        Vector("g1h1"),
        Vector("h2g2"),
        Vector("h1i2"),
        Vector("f2e2"),
        Vector("e1g1"),
        Vector("b2a2"),
        Vector("g1h1"),
        Vector("c2b2"),
        Vector("f1g1"),
        Vector("c2b2"),
        Vector("g1h1"),
        Vector("b2a2"),
        Vector("h1i2"),
        Vector("b2a2"),
        Vector("c1i2"),
        Vector("b2a2"),
        Vector("h1i2"),
        Vector("i2h2"),
        Vector("i1d2")
      )
      playActionStrs(vectorActionStrs.flatten[String].toList) .toOption must beSome.like { case g =>
        val replay = Replay
          .gameWithUciWhileValid(
            vectorActionStrs,
            Player.P1,
            Player.fromTurnCount(vectorActionStrs.size),
            Togyzkumalak.initialFen,
            Togyzkumalak
          )
          ._2
          .reverse
          .head
          ._1
        g.plies === replay.plies
        g.turnCount === replay.turnCount
        g.startedAtPly === replay.startedAtPly
        g.startedAtTurn === replay.startedAtTurn
        g.actionStrs === replay.actionStrs
        g.situation.board.pieces === replay.situation.board.pieces
        g.situation.board.history.score === replay.situation.board.history.score
        g.situation.board.variant === replay.situation.board.variant
        g.situation.board.valid(true) === true
      }
    }
  }

}
