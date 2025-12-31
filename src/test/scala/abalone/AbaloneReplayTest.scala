package strategygames.abalone

import org.specs2.matcher.ValidatedMatchers

import strategygames.Player
import strategygames.abalone.variant.Abalone

class AbaloneReplayTest extends AbaloneTest with ValidatedMatchers {

  "Abalone replay" should {
    "match play through of game" in {
      val vectorActionStrs = Vector(
        Vector("a1d4"),
        Vector("e9e6"),
        Vector("b1d3"),
        Vector("e1e4"),
        Vector("c2e4"),
        Vector("d8d6"),
        Vector("b3e3"),
        Vector("g3f4"),
        Vector("i9f6"),
        Vector("e8e5"),
        Vector("a2c2"),
        Vector("f2c2"),
        Vector("h9f7"),
        Vector("e2b2"),
        Vector("a2b3"),
        Vector("e7e4"),
        Vector("i8g6"),
        Vector("e6e3"),
        Vector("b3e3"),
        Vector("d7e6"),
        Vector("h8h6"),
        Vector("g4d4")
      )
      playActionStrs(vectorActionStrs.flatten[String].toList) .toOption must beSome.like { case g: Game =>
        val replay = Replay
          .gameWithUciWhileValid(
            vectorActionStrs,
            Player.P1,
            Player.P1,
            Abalone.initialFen,
            Abalone
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
        g.situation.board.history.lastTurn === replay.situation.board.history.lastTurn
        g.situation.board.history.currentTurn === replay.situation.board.history.currentTurn
        g.situation.board.history.score === replay.situation.board.history.score
        g.situation.board.variant === replay.situation.board.variant
      }
    }

  }

}
