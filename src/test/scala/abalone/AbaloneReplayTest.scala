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
      playActionStrs(vectorActionStrs.flatten.toList) must beValid.like { g =>
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
        g.plies must_== replay.plies
        g.turnCount must_== replay.turnCount
        g.startedAtPly must_== replay.startedAtPly
        g.startedAtTurn must_== replay.startedAtTurn
        g.actionStrs must_== replay.actionStrs
        g.situation.board.pieces must_== replay.situation.board.pieces
        g.situation.board.history.lastTurn must_== replay.situation.board.history.lastTurn
        g.situation.board.history.currentTurn must_== replay.situation.board.history.currentTurn
        g.situation.board.history.score must_== replay.situation.board.history.score
        g.situation.board.variant must_== replay.situation.board.variant
      }
    }

  }

}
