package strategygames.samurai
import strategygames.Player
import strategygames.samurai.format.FEN

import org.specs2.matcher.ValidatedMatchers
import org.specs2.mutable.Specification

class OwareFenTest extends Specification with ValidatedMatchers {

  "OwarestoneArray from initial fen" should {
    val fen = variant.Oware.initialFen
    "be valid" in {
      fen.owareStoneArray === Array(4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4)
    }
    "have starting player as South (P1)" in {
      fen.player === Some(Player.P1)
    }
    "have North (P2) as the player when inverting" in {
      fen.invertPlayer.flatMap(_.player) === Some(Player.P2)
    }
    "player 1 score be 0" in {
      fen.player1Score === 0
    }
    "player 2 score be 0" in {
      fen.player2Score === 0
    }
  }

  "fen 1s,1s,4s,4s,4s,4s/4s,4s,1s,1s,4s,4s 4 8 N 11" should {
    val fen = FEN("1s,1s,4s,4s,4s,4s/4s,4s,1s,1s,4s,4s 4 8 N 11")
    "player 1 score be 4" in {
      fen.player1Score === 4
    }
    "player 2 score be 8" in {
      fen.player2Score === 8
    }
    "Player turn is North (P2)" in {
      fen.player === Some(Player.P2)
    }
    "have South (P1) as the player when inverting" in {
      fen.invertPlayer.flatMap(_.player) === Some(Player.P1)
    }
    "OwarestoneArray" in {
      fen.owareStoneArray === Array(4, 4, 1, 1, 4, 4, 4, 4, 4, 4, 1, 1)
    }
  }

  "fen 5,13s/4s,1,1s,2,3s 27 2 S 50" should {
    val fen = FEN("5,13s/4s,1,1s,2,3s 27 2 S 50")
    "player 1 score be 27" in {
      fen.player1Score === 27
    }
    "player 2 score be 2" in {
      fen.player2Score === 2
    }
    "Player turn is South" in {
      fen.player === Some(Player.P1)
    }
    "OwarestoneArray" in {
      fen.owareStoneArray === Array(4, 0, 1, 0, 0, 3, 13, 0, 0, 0, 0, 0)
    }
  }

  "fen 4s,4s,4s,5s,5s,5s/4s,4s,4s,4s,1,5s 0 0 N 1" should {
    val fen = FEN("4s,4s,4s,5s,5s,5s/4s,4s,4s,4s,1,5s 0 0 N 1")
    "player 1 score be 0" in {
      fen.player1Score === 0
    }
    "player 2 score be 0" in {
      fen.player2Score === 0
    }
    "Player turn is South" in {
      fen.player === Some(Player.P2)
    }
    "OwarestoneArray" in {
      fen.owareStoneArray === Array(4, 4, 4, 4, 0, 5, 5, 5, 5, 4, 4, 4)
    }
  }

  "Bad FEN" should {
    "have no player" in {
      FEN("badfen").player === None
    }
    "have no fen when inverting" in {
      FEN("badfen").invertPlayer.flatMap(_.player) === None
    }
  }

  "Bad Player FEN" should {
    "have no player" in {
      FEN("badplayerfen p").player === None
    }
    "have no fen when inverting" in {
      FEN("badplayerfen p").invertPlayer.flatMap(_.player) === None
    }
  }
}
