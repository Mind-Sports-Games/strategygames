package strategygames.go
import strategygames.Player
import strategygames.go.format.FEN

import org.specs2.matcher.ValidatedMatchers
import org.specs2.mutable.Specification

class GoFenTest extends Specification with ValidatedMatchers {

  "Go 19x19 initial fen" should {
    val fen = variant.Go19x19.initialFen
    "be of size 19" in {
      fen.gameSize must_== 19
    }
    "have starting player as P1" in {
      fen.player must_== Some(Player.P1)
    }
    "have P2 as the player when inverting" in {
      fen.invertPlayer.flatMap(_.player) must_== Some(Player.P2)
    }
    "player 1 score be 0" in {
      fen.player1Score must_== 0
    }
    "player 2 score be 0" in {
      fen.player2Score must_== 75
    }
    "player 1 captures be 0" in {
      fen.player1Captures must_== 0
    }
    "player 2 captures be 0" in {
      fen.player2Captures must_== 0
    }
  }

  "Go 13x13 initial fen" should {
    val fen = variant.Go13x13.initialFen
    "be of size 13" in {
      fen.gameSize must_== 13
    }
    "have starting player as P1" in {
      fen.player must_== Some(Player.P1)
    }
    "have P2 as the player when inverting" in {
      fen.invertPlayer.flatMap(_.player) must_== Some(Player.P2)
    }
    "player 1 score be 0" in {
      fen.player1Score must_== 0
    }
    "player 2 score be 75" in {
      fen.player2Score must_== 75
    }
    "player 1 captures be 0" in {
      fen.player1Captures must_== 0
    }
    "player 2 captures be 0" in {
      fen.player2Captures must_== 0
    }
  }

  "Go 9x9 initial fen" should {
    val fen = variant.Go9x9.initialFen
    "be of size 9" in {
      fen.gameSize must_== 9
    }
    "have starting player as P1" in {
      fen.player must_== Some(Player.P1)
    }
    "have P2 as the player when inverting" in {
      fen.invertPlayer.flatMap(_.player) must_== Some(Player.P2)
    }
    "player 1 score be 0" in {
      fen.player1Score must_== 0
    }
    "player 2 score be 55" in {
      fen.player2Score must_== 55
    }
    "player 1 captures be 0" in {
      fen.player1Captures must_== 0
    }
    "player 2 captures be 0" in {
      fen.player2Captures must_== 0
    }
  }

  val fen = FEN("19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/S18/S9s8[SSSSSSSSSSssssssssss] w - 20 85 0 0 75 0 2")
  s"fen ${fen.value}" should {
    "have player as P2" in {
      fen.player must_== Some(Player.P2)
    }
    "have P1 as the player when inverting" in {
      fen.invertPlayer.flatMap(_.player) must_== Some(Player.P1)
    }
    "player 1 score be 20" in {
      fen.player1Score must_== 20
    }
    "player 2 score be 85" in {
      fen.player2Score must_== 85
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
