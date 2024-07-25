package strategygames.fairysf

import org.specs2.matcher.ValidatedMatchers

import strategygames.fairysf.variant.Flipello10

class FairyReplayTest extends FairySFTest with ValidatedMatchers {

  "FairySF replay" should {
    "match play through of Flipello10 game" in {
      val vectorActionStrs = Vector(
        Vector("P@g5"),
        Vector("P@e4"),
        Vector("P@d5"),
        Vector("P@g4"),
        Vector("P@d6"),
        Vector("P@d7"),
        Vector("P@g3"),
        Vector("P@h3"),
        Vector("P@f3"),
        Vector("P@f7"),
        Vector("P@g6"),
        Vector("P@d4"),
        Vector("P@c6"),
        Vector("P@f4"),
        Vector("P@f8"),
        Vector("P@e7"),
        Vector("P@d8"),
        Vector("P@e8"),
        Vector("P@i2"),
        Vector("P@j1"),
        Vector("P@d3"),
        Vector("P@e3"),
        Vector("P@i3"),
        Vector("P@e2"),
        Vector("P@g2"),
        Vector("P@h4"),
        Vector("P@e9"),
        Vector("P@j2"),
        Vector("P@h5"),
        Vector("P@e10"),
        Vector("P@g7"),
        Vector("P@c5"),
        Vector("P@c4"),
        Vector("P@h2"),
        Vector("P@e1"),
        Vector("P@c7"),
        Vector("P@c8"),
        Vector("P@g1"),
        Vector("P@i1"),
        Vector("P@h1"),
        Vector("P@f1"),
        Vector("P@d1"),
        Vector("P@f10"),
        Vector("P@f2"),
        Vector("P@d10"),
        Vector("P@d2"),
        Vector("P@c1"),
        Vector("P@b1"),
        Vector("P@j3"),
        Vector("P@j4"),
        Vector("d2d2")
      )
      playActionStrs(vectorActionStrs.flatten.toList, Some(Game.apply(variant.Flipello10))) must beValid
        .like { g =>
          val replay = Replay
            .gameWithUciWhileValid(
              vectorActionStrs,
              Flipello10.initialFen,
              Flipello10
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
          g.situation.board.variant must_== replay.situation.board.variant
        }
    }

  }

}
