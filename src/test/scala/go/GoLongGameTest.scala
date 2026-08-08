package strategygames.go

import org.specs2.mutable.Specification

import strategygames.go.variant.Go19x19

class GoLongGameTest extends Specification {

  private val pliesPastTheLengthThatOnceRanOut = 610

  private def playedToLength(plies: Int): Game =
    (0 until plies).foldLeft(Game(Go19x19)) { (played, ply) =>
      val drops = Go19x19.validDrops(played.situation)
      if (drops.isEmpty) sys.error(s"no legal drop at ply ${ply}")
      else played.apply(if (ply % 2 == 0) drops.head else drops.last)
    }

  "a 19x19 game whose players take the first and the last legal drop" should {

    s"still be offering drops after ${pliesPastTheLengthThatOnceRanOut} plies" in {
      val played = playedToLength(pliesPastTheLengthThatOnceRanOut)
      (played.plies === pliesPastTheLengthThatOnceRanOut) and
        (Go19x19.validDrops(played.situation) must not(beEmpty)) and
        (played.situation.end === false)
    }
  }
}
