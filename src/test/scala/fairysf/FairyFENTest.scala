package strategygames.fairysf

import org.specs2.matcher.ValidatedMatchers
import org.specs2.mutable.Specification

import strategygames.fairysf.variant.Shogi
import strategygames.fairysf.format.FEN
import strategygames.Player

class FairyFENTest extends Specification with ValidatedMatchers {

  "Shogi Initial FEN" should {
    "have P1 as the player" in {
      Shogi.initialFen.player must_== Some(Player.P1)
    }
    "have P2 as the player when inverting" in {
      Shogi.initialFen.invertPlayer.flatMap(_.player) must_== Some(Player.P2)
    }
  }

  "Example Xiangqi FEN" should {
    "have P2 as the player" in {
      FEN("9/5kR1C/9/9/9/9/9/5A2B/9/4K4 b - - 0 1").player must_== Some(Player.P2)
    }
    "have P1 as the player when inverting" in {
      FEN("9/5kR1C/9/9/9/9/9/5A2B/9/4K4 b - - 0 1").invertPlayer.flatMap(_.player) must_== Some(Player.P1)
    }
    "have P2 as the player when inverting the invert" in {
      FEN("9/5kR1C/9/9/9/9/9/5A2B/9/4K4 b - - 0 1").invertPlayer.flatMap(_.invertPlayer.flatMap(_.player)) must_== Some(Player.P2)
    }
  }

  "Bad FEN" should {
    "have no player" in {
      FEN("badfen").player must_== None
    }
    "have no fen when inverting" in {
      FEN("badfen").invertPlayer.flatMap(_.player) must_== None
    }
  }

  "Bad Player FEN" should {
    "have no player" in {
      FEN("badplayerfen p").player must_== None
    }
    "have no fen when inverting" in {
      FEN("badplayerfen p").invertPlayer.flatMap(_.player) must_== None
    }
  }
}
