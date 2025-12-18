package strategygames.abalone

import strategygames.abalone.variant.Abalone

class AbaloneReplayTest extends AbaloneTest {
  "Abalone replay" should {
    "match play through of game" in {
      val vectorActionStrs = Vector(
        Vector("a1d4"),
        Vector("i5f5"),
        Vector("a2c4"),
        Vector("a5d5"),
        Vector("b3e6"),
        Vector("h4f4"),
        Vector("c2c7"),
        Vector("c7d6"),
        Vector("i9f6"),
        Vector("h5e5"),
        Vector("b1b3"),
        Vector("b6b1"),
        Vector("i8g6"),
        Vector("b5b1"), // b5xb1
        Vector("b1c2"),
        Vector("g5b5"),
        Vector("h9f7"),
        Vector("f5a5"),
        Vector("c2c6"),
        Vector("g4f5"),
        Vector("h8f8"),
        Vector("d7d3")
      )
      playActionStrs(vectorActionStrs.flatten[String].toList) .toOption must beSome.like { case g: Game =>
        val replay = Replay
          .gameWithUciWhileValid(
            vectorActionStrs,
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

    "match play through another game" in {
      val vectorActionStrs = Vector(
        Vector("a2a3"),
        Vector("c5a3"),//c5xa3
        Vector("a1d4")
      )
      playActionStrs(vectorActionStrs.flatten.toList) must beValid.like { g =>
        val replay = Replay
          .gameWithUciWhileValid(
            vectorActionStrs,
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