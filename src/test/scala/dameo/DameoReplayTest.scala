package strategygames.dameo

import org.specs2.matcher.ValidatedMatchers

import strategygames.dameo.variant.Dameo

class DameoReplayTest extends DameoTest with ValidatedMatchers {

  "Dameo replay" should {
    "match play through of game" in {
      val vectorActionStrs = Vector(
        Vector("d1d4"),
        Vector("d8b6"),
        Vector("e1e4"),
        Vector("d6d5"),
        Vector("d4d6", "d6d8K"),
        Vector("b8a7"),
        Vector("d8b8"),
        Vector("a8c8"),
        Vector("h1d5"),
        Vector("f7g6"),
        Vector("a1d4"),
        Vector("e6e5"),
        Vector("d5f5", "f5f7", "f7d7"),
        Vector("c7e7"),
        Vector("g2d5"),
        Vector("c6c5"),
        Vector("d5b5"),
        Vector("b6b4"),
        Vector("d2d5"),
        Vector("b4a3"),
        Vector("d3d6"),
        Vector("a3a2"),
        Vector("d4d7"),
        Vector("e7c7"),
        Vector("d5c6"),
        Vector("c7c5"),
        Vector("d6d7"),
        Vector("a2a1K"),
        Vector("d7d8K"),
        Vector("a1a4"),
        Vector("d8b8", "b8b5", "b5e5"),
        Vector("a4h4"),
        Vector("e5a5"),
        Vector("a7a6"),
        Vector("a5a7", "a7h7", "h7h1"),
        Vector("g8h7"),
        Vector("g1d4"),
        Vector("h7h6"),
        Vector("h1h7"),
        Vector("h8h6"),
        Vector("e2g4"),
        Vector("h6h5"),
        Vector("f1f4"),
        Vector("h5h4"),
        Vector("f4g5"),
        Vector("h4f4"),
        Vector("g5g7"),
        Vector("f8f7"),
        Vector("g7e7"),
        Vector("e8e6"),
        Vector("f3f5"),
        Vector("e6d5"),
        Vector("d4d6")
      )
      playActionStrs(vectorActionStrs.flatten.toList) must beValid.like { g =>
        val replay = Replay
          .gameWithUciWhileValid(
            vectorActionStrs,
            Dameo.initialFen,
            Dameo
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
        g.situation.board.variant must_== replay.situation.board.variant
      }
    }

    "multi-move capture turn" should {
      /* Test for a problematic situation encountered during front-end implementation.
      After these moves:
        d3d4
        d6d5
        d4d6
      the current turn should be W, and the turnCount should be 2.
      */
      val vectorActionStrs = Vector(
        Vector("d3d4"),
        Vector("d6d5"),
        Vector("d4d6")
      )
      playActionStrs(vectorActionStrs.flatten.toList) must beValid.like { g =>
        val replay = Replay
          .gameWithUciWhileValid(
            vectorActionStrs,
            Dameo.initialFen,
            Dameo
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
        g.situation.board.variant must_== replay.situation.board.variant
      }
    }
  }
}
