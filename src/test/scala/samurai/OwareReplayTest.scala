package strategygames.samurai

import format.Uci
import org.specs2.matcher.ValidatedMatchers
import org.specs2.mutable.Specification
import strategygames.Player

class OwareReplayTest extends Specification with ValidatedMatchers {

  "Replay game move while valid" should {
    val varaint    = variant.Oware
    val pgnMoves   = Vector("d1e2", "c2b1", "e1c2", "b2d1", "f1a2", "d2c1", "b1d2", "b2a1")
    val initialFen = variant.Oware.initialFen

    val x: (Game, List[(Game, Uci.WithSan)], Option[String]) =
      Replay.gameWithUciWhileValid(pgnMoves.map(Vector(_)), Player.P1, Player.P1, initialFen, varaint)

    val initGame = x._1
    val getFen   = initGame.situation.board.apiPosition.fen

    val calcInitialFen = x match {
      case (init, _, _) =>
        format.Forsyth.>>(init)
    }

    "forsyth method is same as using full steps" in {
      getFen === calcInitialFen
    }

    "give intial fen as init step" in {
      initialFen === calcInitialFen
    }
  }
}
