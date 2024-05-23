package strategygames.togyzkumalak
import strategygames.Player

import org.specs2.matcher.ValidatedMatchers
import org.specs2.mutable.Specification

class TogyzkumalakFenTest extends Specification with ValidatedMatchers {

  "MancalastoneArray from initial fen" should {
    val fen = variant.Togyzkumalak.initialFen
    "be valid" in {
      fen.mancalaStoneArray must_== Array.fill(18)(9)
    }
  }

  "Initial fen scores, no tuzdiks and starting player" should {
    val fen = variant.Togyzkumalak.initialFen
    "player 1 score be 0" in {
      fen.player1Score must_== 0
    }
    "player 2 score be 0" in {
      fen.player2Score must_== 0
    }
    "Tuzdik pits must be empty" in {
      fen.tuzdikPits must_== Map(Player.P1 -> None, Player.P2 -> None)
    }
    "Starting player is South" in {
      fen.player must_== Some(Player.P1)
    }
  }

  val fen1 = "9S,9S,9S,9S,1,10S,10S,10S,10S/9S,9S,9S,9S,9S,1S,10S,10S,10S 10 0 N 1"
  s"fen $fen1" should {
    val fen = strategygames.togyzkumalak.format.FEN(fen1)
    "player 1 score be 10" in {
      fen.player1Score must_== 10
    }
    "player 2 score be 0" in {
      fen.player2Score must_== 0
    }
    "Player turn is North" in {
      fen.player must_== Some(Player.P2)
    }
    "Stone Array is valid" in {
      fen.mancalaStoneArray must_== Array(9, 9, 9, 9, 9, 1, 10, 10, 10, 10, 10, 10, 10, 0, 9, 9, 9, 9)
    }
  }

  val fen2 = "10S,10S,10S,1S,1,10S,10S,10S,10S/10S,10S,10S,10S,1,1S,10S,10S,10S 10 10 S 2"
  s"fen $fen2" should {
    val fen = strategygames.togyzkumalak.format.FEN(fen2)
    "player 1 score be 10" in {
      fen.player1Score must_== 10
    }
    "player 2 score be 10" in {
      fen.player2Score must_== 10
    }
    "Player turn is North" in {
      fen.player must_== Some(Player.P1)
    }
    "Stone Array is valid" in {
      fen.mancalaStoneArray must_== Array(10, 10, 10, 10, 0, 1, 10, 10, 10, 10, 10, 10, 10, 0, 1, 10, 10, 10)
    }
  }

  val fen3 = "13S,2S,12S,3S,2S,1S,1,12S,12S/12S,2S,13S,13S,t,3S,12S,12S,1S 22 15 S 4"
  s"fen $fen3" should {
    val fen = strategygames.togyzkumalak.format.FEN(fen3)
    "player 1 score be 22" in {
      fen.player1Score must_== 22
    }
    "player 2 score be 15" in {
      fen.player2Score must_== 15
    }
    "Stone Array is valid" in {
      fen.mancalaStoneArray must_== Array(12, 2, 13, 13, -1, 3, 12, 12, 1, 12, 12, 0, 1, 2, 3, 12, 2, 13)
    }
    "Tuzdik pits must be" in {
      fen.tuzdikPits(Player.P1) must_== Some(4)
      fen.tuzdikPits(Player.P2).isEmpty must_== true
    }
  }

  val fen4 = "11S,1S,7S,t,6S,4S,2S,4S,3S/2S,6S,19S,t,1S,3S,1S,3S,3S 57 29 N 10"
  s"fen $fen4" should {
    val fen = strategygames.togyzkumalak.format.FEN(fen4)
    "player 1 score be 57" in {
      fen.player1Score must_== 57
    }
    "player 2 score be 29" in {
      fen.player2Score must_== 29
    }
    "Stone Array is valid" in {
      fen.mancalaStoneArray must_== Array(2, 6, 19, -1, 1, 3, 1, 3, 3, 3, 4, 2, 4, 6, -1, 7, 1, 11)
    }
    "Tuzdik pits must be" in {
      fen.tuzdikPits(Player.P1) must_== Some(3)
      fen.tuzdikPits(Player.P2) must_== Some(5)
    }
  }

  val fen5 = "6S,1,1S,t,5S,3S,4S,3S,1S/1S,2S,2S,t,3S,2S,2S,1S,1S 82 43 N 38"
  s"fen $fen5" should {
    val fen = strategygames.togyzkumalak.format.FEN(fen5)
    "player 1 score be 82" in {
      fen.player1Score must_== 82
    }
    "player 2 score be 43" in {
      fen.player2Score must_== 43
    }
  }

}
