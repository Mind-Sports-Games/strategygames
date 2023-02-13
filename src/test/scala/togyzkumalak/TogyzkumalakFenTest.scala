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

  "Initial fen scores and starting player" should {
    val fen = variant.Togyzkumalak.initialFen
    "player 1 score be 0" in {
      fen.player1Score must_== 0
    }
    "player 2 score be 0" in {
      fen.player2Score must_== 0
    }
    "Starting player is South" in {
      fen.player must_== Some(Player.P1)
    }
  }

  //"fen 1s,1s,4s,4s,4s,4s/4s,4s,1s,1s,4s,4s 4 8 N 11" should {
  //  val fen = strategygames.togyzkumalak.format.FEN("1s,1s,4s,4s,4s,4s/4s,4s,1s,1s,4s,4s 4 8 N 11")
  //  "player 1 score be 4" in {
  //    fen.player1Score must_== 4
  //  }
  //  "player 2 score be 8" in {
  //    fen.player2Score must_== 8
  //  }
  //  "Player turn is North" in {
  //    fen.player must_== Some(Player.P2)
  //  }
  //  "TogyzkumalakstoneArray" in {
  //    fen.owareStoneArray must_== Array(4, 4, 1, 1, 4, 4, 4, 4, 4, 4, 1, 1)
  //  }
  //}

  //"fen 5,13s/4s,1,1s,2,3s 27 2 S 50" should {
  //  val fen = strategygames.togyzkumalak.format.FEN("5,13s/4s,1,1s,2,3s 27 2 S 50")
  //  "player 1 score be 27" in {
  //    fen.player1Score must_== 27
  //  }
  //  "player 2 score be 2" in {
  //    fen.player2Score must_== 2
  //  }
  //  "Player turn is South" in {
  //    fen.player must_== Some(Player.P1)
  //  }
  //  "TogyzkumalakstoneArray" in {
  //    fen.owareStoneArray must_== Array(4, 0, 1, 0, 0, 3, 13, 0, 0, 0, 0, 0)
  //  }
  //}

  //"fen 4s,4s,4s,5s,5s,5s/4s,4s,4s,4s,1,5s 0 0 N 1" should {
  //  val fen = strategygames.togyzkumalak.format.FEN("4s,4s,4s,5s,5s,5s/4s,4s,4s,4s,1,5s 0 0 N 1")
  //  "player 1 score be 0" in {
  //    fen.player1Score must_== 0
  //  }
  //  "player 2 score be 0" in {
  //    fen.player2Score must_== 0
  //  }
  //  "Player turn is South" in {
  //    fen.player must_== Some(Player.P2)
  //  }
  //  "TogyzkumalakstoneArray" in {
  //    fen.owareStoneArray must_== Array(4, 4, 4, 4, 0, 5, 5, 5, 5, 4, 4, 4)
  //  }
  //}

}
