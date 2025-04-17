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
        Vector("d4xd5d6","d6xd7d8k"),
        Vector("b8a7"),
        Vector("d8xc8b8"),
        Vector("a8xb8c8"),
        Vector("h1d5"),
        Vector("f7g6"),
        Vector("a1d4"),
        Vector("e6e5"),
        Vector("d5xe5f5", "f5xf6f7", "f7xe7d7"),
        Vector("c7xd7e7"),
        Vector("g2d5"),
        Vector("c6c5"),
        Vector("d5xc5b5"),
        Vector("b6xb5b4"),
        Vector("d2d5"),
        Vector("b4a3"),
        Vector("d3d6"),
        Vector("a3a2"),
        Vector("d4d7"),
        Vector("e7xd7c7"),
        Vector("d5c6"),
        Vector("c7xc6c5"),
        Vector("d6d7"),
        Vector("a2a1k"),
        Vector("d7d8k"),
        Vector("a1a4"),
        Vector("d8xc8b8", "b8xb7b5", "b5xc5e5"),
        Vector("a4xe4h4"),
        Vector("e5a5"),
        Vector("a7a6"),
        Vector("a5xa6a7", "a7xg7h7", "h7xh4h1"),
        Vector("g8h7"),
        Vector("g1d4"),
        Vector("h7h6"),
        Vector("h1xh6h7"),
        Vector("h8xh7h6"),
        Vector("e2g4"),
        Vector("h6h5"),
        Vector("f1f4"),
        Vector("h5h4"),
        Vector("f4g5"),
        Vector("h4xg4f4"),
        Vector("g5xg6g7"),
        Vector("f8f7"),
        Vector("g7xf7e7"),
        Vector("e8xe7e6"),
        Vector("f3xf4f5"),
        Vector("e6d5"),
        Vector("d4xd5d6"),
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
