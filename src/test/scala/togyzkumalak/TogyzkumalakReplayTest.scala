package strategygames.togyzkumalak

import format.Uci
import org.specs2.matcher.ValidatedMatchers
import org.specs2.mutable.Specification

class TogyzkumalakReplayTest extends Specification with ValidatedMatchers {

  //"Replay game move while valid" should {
  //  val varaint    = variant.Togyzkumalak
  //  val pgnMoves   = Vector("d1e2", "c2b1", "e1c2", "b2d1", "f1a2", "d2c1", "b1d2", "b2a1")
  //  val initialFen = variant.Togyzkumalak.initialFen

  //  val x: (Game, List[(Game, Uci.WithSan)], Option[String]) =
  //    Replay.gameMoveWhileValid(pgnMoves, initialFen, varaint)

  //  val initGame = x._1
  //  val getFen   = initGame.situation.board.apiPosition.fen

  //  val calcInitialFen = x match {
  //    case (init, _, _) =>
  //      format.Forsyth.>>(init)
  //  }

  //  "forsyth method is same as using full steps" in {
  //    getFen must_== calcInitialFen
  //  }

  //  "give intial fen as init step" in {
  //    initialFen must_== calcInitialFen
  //  }
  //}
}
