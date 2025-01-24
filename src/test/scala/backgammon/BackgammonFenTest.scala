package strategygames.backgammon
import strategygames.{ Player, Score }
import org.specs2.matcher.ValidatedMatchers

import strategygames.backgammon.format.{ FEN, Forsyth, Uci }
import strategygames.backgammon.variant.Backgammon

class BackgammonFenTest extends BackgammonTest with ValidatedMatchers {

  "Backgammon stoneArray from initial fen" should {
    val fen    = Backgammon.initialFen
    val pieces = fen.pieces

    "be valid" in {
      fen.stoneArray must_== Array(
        "2s",
        "0",
        "0",
        "0",
        "0",
        "5S",
        "0",
        "3S",
        "0",
        "0",
        "0",
        "5s",
        "5S",
        "0",
        "0",
        "0",
        "3s",
        "0",
        "5s",
        "0",
        "0",
        "0",
        "0",
        "2S"
      )
    }
    "8 pieces" in {
      pieces.size must_== 8
    }
    "pieces at a1, e1, g1, l1, and rank 2" in {
      pieces.keys.toList.contains(Pos.A1) must_== true
      pieces.keys.toList.contains(Pos.E1) must_== true
      pieces.keys.toList.contains(Pos.G1) must_== true
      pieces.keys.toList.contains(Pos.L1) must_== true
      pieces.keys.toList.contains(Pos.A2) must_== true
      pieces.keys.toList.contains(Pos.E2) must_== true
      pieces.keys.toList.contains(Pos.G2) must_== true
      pieces.keys.toList.contains(Pos.L2) must_== true
    }
    "have no pieces in the pockets" in {
      fen.pocketData must_== Some(PocketData.init)
    }
    "have no unusedDice" in {
      fen.unusedDice.isEmpty must_== true
    }
    "have no cubeData" in {
      fen.cubeData must_== None
    }
  }

  "Initial fen starting player" should {
    val fen = Backgammon.initialFen
    "Starting player is white/p1" in {
      fen.player must_== Some(Player.P1)
    }
  }

  val fenStr1 = "6,2s,2s,2s,2s,2s,2s/6,5S,5[2S] - - b 8 3 - 10"
  s"fen $fenStr1" should {
    val fen    = FEN(fenStr1)
    val pieces = fen.pieces

    "Player turn is black/p2" in {
      fen.player must_== Some(Player.P2)
    }
    "stoneArray" in {
      fen.stoneArray must_== Array(
        "0",
        "0",
        "0",
        "0",
        "0",
        "5S",
        "0",
        "0",
        "0",
        "0",
        "0",
        "0",
        "0",
        "0",
        "0",
        "0",
        "0",
        "0",
        "2s",
        "2s",
        "2s",
        "2s",
        "2s",
        "2s"
      )
    }
    "7 pieces" in {
      pieces.size must_== 7
    }
    "pieces at g1, h1, i1, j1, k1, l1, g2" in {
      pieces.keys.toList.contains(Pos.G2) must_== true
      pieces.keys.toList.contains(Pos.H2) must_== true
      pieces.keys.toList.contains(Pos.I2) must_== true
      pieces.keys.toList.contains(Pos.J2) must_== true
      pieces.keys.toList.contains(Pos.K2) must_== true
      pieces.keys.toList.contains(Pos.L2) must_== true
      pieces.keys.toList.contains(Pos.G1) must_== true
    }
    "have the correct number of pieces in the pockets" in {
      fen.pocketData.map(_.pockets(Player.P1).roles.size) must_== Some(2)
      fen.pocketData.map(_.pockets(Player.P2).roles.size) must_== Some(0)
    }
    "score" in {
      fen.player1Score must_== 8
      fen.player2Score must_== 3
    }
  }

  "gameplay" should {
    "match expected fen" in {
      val actionStrs = List(
        "1/5",
        "l2k2",
        "e1j1",
        "endturn",
        "2/4",
        "l1j1",
        "g2k2",
        "endturn",
        "2/1",
        "s@k2"
      )
      playActionStrs(actionStrs) must beValid.like { g =>
        val fen = Forsyth.>>(g)
        fen must_== FEN("5S,3,3s,1,4s,3,1S,1S/5s,3,2S,1,5S,2,1s,1,1s[1S,1s] 1 2 w 0 0 - 2")
        fen.pocketData.map(_.pockets(Player.P1).roles.size) must_== Some(1)
        fen.pocketData.map(_.pockets(Player.P2).roles.size) must_== Some(1)
      }
    }
  }

  // also use fen test to check gin position
  "Both P1 and P2 are just 4 points from the end with 2 pieces" should {
    val fen   = FEN("9,1s,1,1s/9,1S,1,1S[] - - w 13 13 - 99")
    val board = Board(fen.pieces, History(), variant.Backgammon, fen.pocketData)
    "this is a gin position for neither player" in {
      Situation(board, Player.P1).opponentHasInsufficientMaterial must_== false
      Situation(board, Player.P2).opponentHasInsufficientMaterial must_== false
      Situation(board, Player.P1).opponentHasInsufficientMaterialForGammon must_== false
      Situation(board, Player.P2).opponentHasInsufficientMaterialForGammon must_== false
      Situation(board, Player.P1).opponentHasInsufficientMaterialForBackgammon must_== false
      Situation(board, Player.P2).opponentHasInsufficientMaterialForBackgammon must_== false
    }
  }

  "P1 is just 3 points from the end with 2 pieces" should {
    val fen   = FEN("4,3s,1,5s,5/5s,9,1S,1S[] - - w 13 2 - 99")
    val board = Board(fen.pieces, History(), variant.Backgammon, fen.pocketData)
    "this is a gin position for P1" in {
      Situation(board, Player.P1).opponentHasInsufficientMaterial must_== true
      Situation(board, Player.P2).opponentHasInsufficientMaterial must_== false
    }
  }

  "Both P1 and P2 are just 3 points from the end with 3 pieces" should {
    val fen   = FEN("11,3s/11,3S[] - - w 12 12 - 99")
    val board = Board(fen.pieces, History(), variant.Backgammon, fen.pocketData)
    "this is a gin position for neither player" in {
      Situation(board, Player.P1).opponentHasInsufficientMaterial must_== false
      Situation(board, Player.P2).opponentHasInsufficientMaterial must_== false
    }
  }

  "P1 is just 4 points from the end with 2 pieces, P2 has 4 pieces on the 6 point" should {
    val fen   = FEN("6,4s,5/9,1S,1,1S[] - - w 13 11 - 99")
    val board = Board(fen.pieces, History(), variant.Backgammon, fen.pocketData)
    "this is a gin position for neither player" in {
      Situation(board, Player.P1).opponentHasInsufficientMaterial must_== false
      Situation(board, Player.P2).opponentHasInsufficientMaterial must_== false
    }
  }

  "P1 is just 4 points from the end with 2 pieces, P2 has 4 pieces on the 6 point but the player has rolled 6/6" should {
    val fen   = FEN("6,4s,5/9,1S,1,1S[] 6/6/6/6 - b 13 11 - 99")
    val board = Board(
      fen.pieces,
      History(
        lastTurn = List(Uci.EndTurn()),
        currentTurn = List(Uci.DiceRoll(List(6, 6))),
        score = Score(13, 11)
      ),
      variant.Backgammon,
      fen.pocketData,
      List(6, 6, 6, 6)
    )
    "this is a gin position for both players with this roll" in {
      Situation(board, Player.P1).opponentHasInsufficientMaterial must_== true
      Situation(board, Player.P2).opponentHasInsufficientMaterial must_== true
    }
  }

  "P1 is just 4 points from the end with 2 pieces, P2 has 4 pieces on the 6 point but the player has rolled 6/6" should {
    val fen   = FEN("5,2s,2s,5/5,2S,2S,5[] 6/6/6/6 - w 11 11 - 99")
    val board = Board(
      fen.pieces,
      History(
        lastTurn = List(Uci.EndTurn()),
        currentTurn = List(Uci.DiceRoll(List(6, 6))),
        score = Score(11, 11)
      ),
      variant.Backgammon,
      fen.pocketData,
      List(6, 6, 6, 6)
    )
    "this is a gin position for both players with this roll as it moves them a turn away from winning" in {
      Situation(board, Player.P1).opponentHasInsufficientMaterial must_== true
      Situation(board, Player.P2).opponentHasInsufficientMaterial must_== true
    }
  }

  "when opponent has piece on bar but we have an even number on 1 point" should {
    val fen   = FEN("4,3s,1,5s,5/5s,10,2S[1s] - - w 13 1 - 99")
    val board = Board(fen.pieces, History(), variant.Backgammon, fen.pocketData)
    "this is a gin position for P1" in {
      Situation(board, Player.P1).opponentHasInsufficientMaterial must_== true
      Situation(board, Player.P2).opponentHasInsufficientMaterial must_== false
    }
  }
  "when opponent has piece on bar but we have an even number on 1 point" should {
    val fen   = FEN("4,3s,1,5s,5/5s,10,2S[1s] - - w 13 1 - 99")
    val board = Board(fen.pieces, History(), variant.Backgammon, fen.pocketData)
    "this is a gin position for P1" in {
      Situation(board, Player.P1).opponentHasInsufficientMaterial must_== true
      Situation(board, Player.P2).opponentHasInsufficientMaterial must_== false
    }
  }
  "when opponent has piece on bar but we have an even number on 1 point" should {
    val fen   = FEN("4,3s,1,5s,5/5s,10,2S[1s] - - w 13 1 - 99")
    val board = Board(fen.pieces, History(), variant.Backgammon, fen.pocketData)
    "this is a gin position for P1" in {
      Situation(board, Player.P1).opponentHasInsufficientMaterial must_== true
      Situation(board, Player.P2).opponentHasInsufficientMaterial must_== false
    }
  }
  "when opponent has piece on bar but we have an even number on 1 point" should {
    val fen   = FEN("4,3s,1,5s,5/5s,10,2S[1s] - - w 13 1 - 99")
    val board = Board(fen.pieces, History(), variant.Backgammon, fen.pocketData)
    "this is a gin position for P1" in {
      Situation(board, Player.P1).opponentHasInsufficientMaterial must_== true
      Situation(board, Player.P2).opponentHasInsufficientMaterial must_== false
    }
  }
  "when opponent has piece on bar but we have an even number on 1 point" should {
    val fen   = FEN("4,3s,1,5s,5/5s,10,2S[1s] - - w 13 1 - 99")
    val board = Board(fen.pieces, History(), variant.Backgammon, fen.pocketData)
    "this is a gin position for P1" in {
      Situation(board, Player.P1).opponentHasInsufficientMaterial must_== true
      Situation(board, Player.P2).opponentHasInsufficientMaterial must_== false
    }
  }
  "when opponent has piece on bar but we have an even number on 1 point" should {
    val fen   = FEN("4,3s,1,5s,5/5s,10,2S[1s] - - w 13 1 - 99")
    val board = Board(fen.pieces, History(), variant.Backgammon, fen.pocketData)
    "this is a gin position for P1" in {
      Situation(board, Player.P1).opponentHasInsufficientMaterial must_== true
      Situation(board, Player.P2).opponentHasInsufficientMaterial must_== false
    }
  }
  "when opponent has piece on bar but we have an even number on 1 point" should {
    val fen   = FEN("4,3s,1,5s,5/5s,10,2S[1s] - - w 13 1 - 99")
    val board = Board(fen.pieces, History(), variant.Backgammon, fen.pocketData)
    "this is a gin position for P1" in {
      Situation(board, Player.P1).opponentHasInsufficientMaterial must_== true
      Situation(board, Player.P2).opponentHasInsufficientMaterial must_== false
    }
  }
  "when opponent has piece on bar but we have an even number on 1 point" should {
    val fen   = FEN("4,3s,1,5s,5/5s,10,2S[1s] - - w 13 1 - 99")
    val board = Board(fen.pieces, History(), variant.Backgammon, fen.pocketData)
    "this is a gin position for P1" in {
      Situation(board, Player.P1).opponentHasInsufficientMaterial must_== true
      Situation(board, Player.P2).opponentHasInsufficientMaterial must_== false
    }
  }

  "when opponent has piece on bar but we have an odd number on 1 point" should {
    val fen   = FEN("4,3s,1,5s,5/5s,10,3S[1s] - - w 12 1 - 99")
    val board = Board(fen.pieces, History(), variant.Backgammon, fen.pocketData)
    "this is not a gin position" in {
      Situation(board, Player.P1).opponentHasInsufficientMaterial must_== false
      Situation(board, Player.P2).opponentHasInsufficientMaterial must_== false
    }
  }

  "when opponent has just one piece on bar but we have six pieces on 1 point" should {
    val fen   = FEN("12/11,6S[1s] - - w 9 14 - 99")
    val board = Board(fen.pieces, History(), variant.Backgammon, fen.pocketData)
    "this is not a gin position" in {
      Situation(board, Player.P1).opponentHasInsufficientMaterial must_== false
      Situation(board, Player.P2).opponentHasInsufficientMaterial must_== false
    }
  }

  "P1 is just 3 points from the end with 2 pieces and opponent hasn't scored" should {
    val fen   = FEN("4,3s,1,5s,5/7s,9,1S,1S[] - - w 13 0 - 99")
    val board = Board(fen.pieces, History(), variant.Backgammon, fen.pocketData)
    "this is a gammon gin position for P1" in {
      Situation(board, Player.P1).opponentHasInsufficientMaterialForGammon must_== true
      Situation(board, Player.P1).opponentHasInsufficientMaterial must_== true
      Situation(board, Player.P2).opponentHasInsufficientMaterialForGammon must_== false
    }
  }

  "P1 is just 3 points from the end with 2 pieces and opponent has scored" should {
    val fen   = FEN("4,3s,1,5s,5/6s,9,1S,1S[] - - w 13 1 - 99")
    val board = Board(
      fen.pieces,
      History(
        score = Score(13, 1)
      ),
      variant.Backgammon,
      fen.pocketData
    )
    "this is not a gammon gin position for P1" in {
      Situation(board, Player.P1).opponentHasInsufficientMaterialForGammon must_== false
      Situation(board, Player.P2).opponentHasInsufficientMaterialForGammon must_== false
    }
  }

  "P1 is 4 points from the end with 2 pieces and opponent is one turn away from scoring" should {
    val fen   = FEN("4,3s,1,12s,5/9,1S,1,1S[] - - w 13 0 - 99")
    val board = Board(fen.pieces, History(), variant.Backgammon, fen.pocketData)
    "this is not a gammon gin position for P1" in {
      Situation(board, Player.P1).opponentHasInsufficientMaterialForGammon must_== false
      Situation(board, Player.P2).opponentHasInsufficientMaterialForGammon must_== false
    }
  }

  "P1 is 4 points from the end with 2 pieces and opponent is two turns away from scoring" should {
    val fen   = FEN("4,4s,1,11s,5/9,1S,1,1S[] - - w 13 0 - 99")
    val board = Board(fen.pieces, History(), variant.Backgammon, fen.pocketData)
    "this is a gammon gin position for P1" in {
      Situation(board, Player.P1).opponentHasInsufficientMaterialForGammon must_== true
      Situation(board, Player.P1).opponentHasInsufficientMaterial must_== true
      Situation(board, Player.P2).opponentHasInsufficientMaterialForGammon must_== false
    }
  }

  "P1 is 4 points from the end with 2 pieces and opponent is one turn away from scoring" should {
    val fen   = FEN("6,14s,5/8,1s,1S,1,1S[] - - w 13 0 - 99")
    val board = Board(fen.pieces, History(), variant.Backgammon, fen.pocketData)
    "this is not a gammon gin position for P1" in {
      Situation(board, Player.P1).opponentHasInsufficientMaterialForGammon must_== false
      Situation(board, Player.P2).opponentHasInsufficientMaterialForGammon must_== false
    }
  }

  "P1 is 4 points from the end with 2 pieces and opponent is two turns away from scoring" should {
    val fen   = FEN("5,1s,13s,5/8,1s,1S,1,1S[] - - w 13 0 - 99")
    val board = Board(fen.pieces, History(), variant.Backgammon, fen.pocketData)
    "this is a gammon gin position for P1" in {
      Situation(board, Player.P1).opponentHasInsufficientMaterialForGammon must_== true
      Situation(board, Player.P1).opponentHasInsufficientMaterial must_== true
      Situation(board, Player.P2).opponentHasInsufficientMaterialForGammon must_== false
    }
  }

  "P1 is just 4 points from the end with 2 pieces, P2 has 4 pieces on the 6 point but the player has rolled 6/6" should {
    val fen   = FEN("5,4s,11s,5/5,2S,2S,5[] 6/6/6/6 - w 11 0 - 99")
    val board = Board(
      fen.pieces,
      History(
        lastTurn = List(Uci.EndTurn()),
        currentTurn = List(Uci.DiceRoll(List(6, 6))),
        score = Score(11, 0)
      ),
      variant.Backgammon,
      fen.pocketData,
      List(6, 6, 6, 6)
    )
    "this is a gammon gin position for P1 with this roll as it moves them a turn away from winning" in {
      Situation(board, Player.P1).opponentHasInsufficientMaterialForGammon must_== true
      Situation(board, Player.P1).opponentHasInsufficientMaterial must_== true
    }
  }

  "P1 is just 3 points from the end with 2 pieces and opponent has pieces in opponents home" should {
    val fen   = FEN("4,3s,1,5s,5/5s,8,2s,1S,1S[] - - w 13 0 - 99")
    val board = Board(fen.pieces, History(), variant.Backgammon, fen.pocketData)
    "this is a backgammon gin position for P1" in {
      Situation(board, Player.P1).opponentHasInsufficientMaterialForBackgammon must_== true
      Situation(board, Player.P1).opponentHasInsufficientMaterial must_== true
      Situation(board, Player.P2).opponentHasInsufficientMaterialForBackgammon must_== false
    }
  }

  "P1 is just 3 points from the end with 2 pieces and opponent has scored" should {
    val fen   = FEN("4,3s,1,4s,5/5s,8,2s,1S,1S[] - - w 13 1 - 99")
    val board = Board(
      fen.pieces,
      History(
        score = Score(13, 1)
      ),
      variant.Backgammon,
      fen.pocketData
    )
    "this is not a gammon gin position for P1" in {
      Situation(board, Player.P1).opponentHasInsufficientMaterialForBackgammon must_== false
      Situation(board, Player.P2).opponentHasInsufficientMaterialForBackgammon must_== false
    }
  }

  "P1 is 4 points from the end with 2 pieces and opponent is one turn away from fleeing home" should {
    val fen   = FEN("5,12s,5/8,3s,1S,1,1S[] - - w 13 0 - 99")
    val board = Board(fen.pieces, History(), variant.Backgammon, fen.pocketData)
    "this is not a backgammon gin position for P1" in {
      Situation(board, Player.P1).opponentHasInsufficientMaterialForBackgammon must_== false
      Situation(board, Player.P2).opponentHasInsufficientMaterialForBackgammon must_== false
    }
  }

  "P1 is 4 points from the end with 2 pieces and opponent is two turns away from scoring" should {
    val fen   = FEN("5,10s,5/8,5s,1S,1,1S[] - - w 13 0 - 99")
    val board = Board(fen.pieces, History(), variant.Backgammon, fen.pocketData)
    "this is a backgammon gin position for P1" in {
      Situation(board, Player.P1).opponentHasInsufficientMaterialForBackgammon must_== true
      Situation(board, Player.P1).opponentHasInsufficientMaterial must_== true
      Situation(board, Player.P2).opponentHasInsufficientMaterialForBackgammon must_== false
    }
  }

  "P1 is 4 points from the end with 2 pieces and opponent is one turn away from scoring" should {
    val fen   = FEN("6,10s,5/8,4s,1S,1,1S[] - - w 13 0 - 99")
    val board = Board(fen.pieces, History(), variant.Backgammon, fen.pocketData)
    "this is not a backgammon gin position for P1" in {
      Situation(board, Player.P1).opponentHasInsufficientMaterialForBackgammon must_== false
      Situation(board, Player.P2).opponentHasInsufficientMaterialForBackgammon must_== false
    }
  }

  "P1 is 4 points from the end with 2 pieces and opponent is two turns away from scoring" should {
    val fen   = FEN("5,1s,9s,5/8,5s,1S,1,1S[] - - w 13 0 - 99")
    val board = Board(fen.pieces, History(), variant.Backgammon, fen.pocketData)
    "this is a backgammon gin position for P1" in {
      Situation(board, Player.P1).opponentHasInsufficientMaterialForBackgammon must_== true
      Situation(board, Player.P1).opponentHasInsufficientMaterial must_== true
      Situation(board, Player.P2).opponentHasInsufficientMaterialForBackgammon must_== false
    }
  }

  "P1 is just 4 points from the end with 2 pieces, P2 has 4 pieces on the 6 point but the player has rolled 6/6" should {
    val fen   = FEN("6,10s,5/10,5s,6S[] 6/6/6/6 - w 9 0 - 99")
    val board = Board(
      fen.pieces,
      History(
        lastTurn = List(Uci.EndTurn()),
        currentTurn = List(Uci.DiceRoll(List(6, 6))),
        score = Score(9, 0)
      ),
      variant.Backgammon,
      fen.pocketData,
      List(6, 6, 6, 6)
    )
    "this is a backgammon gin position for P1 with this roll as it moves them a turn away from winning" in {
      Situation(board, Player.P1).opponentHasInsufficientMaterialForBackgammon must_== true
      Situation(board, Player.P1).opponentHasInsufficientMaterial must_== true
    }
  }

  "Gammon position isnt backgammon" should {
    val fen   = FEN("1s,1s,2,1s,4,1s,1,4s/1s,1s,1,1s,1,4s,4,1S,1[] 3/4 - w 14 0 - 99")
    val board = Board(
      fen.pieces,
      History(
        lastTurn = List(Uci.EndTurn()),
        currentTurn = List(Uci.DiceRoll(List(4, 3))),
        score = Score(14, 0)
      ),
      variant.Backgammon,
      fen.pocketData,
      List(4, 3)
    )
    "this is a gammon gin position for P1 and not backgammon" in {
      Situation(board, Player.P1).opponentHasInsufficientMaterialForBackgammon must_== false
      Situation(board, Player.P1).opponentHasInsufficientMaterialForGammon must_== true
      Situation(board, Player.P1).opponentHasInsufficientMaterial must_== true
    }
  }

  // cube fen tests
  "Starting fen with a cube" should {
    val fenStr = "5S,3,3s,1,5s,4,2S/5s,3,3S,1,5S,4,2s[] - - w 0 0 0 1"
    val fen    = FEN(fenStr)
    val board  = Board(
      fen.pieces,
      History(),
      variant.Backgammon,
      fen.pocketData,
      fen.unusedDice,
      fen.cubeData
    )
    "have an initial cube" in {
      board.cubeData must_== Some(CubeData.init)
      board.cubeData.map(_.value) must_== Some(1)
      board.cubeData.map(_.owner) must_== Some(None)
      board.cubeData.map(_.underOffer) must_== Some(false)
      board.cubeData.map(_.rejected) must_== Some(false)
      Forsyth.>>(Situation(board, fen.player.getOrElse(Player.P1))).value must_== fenStr
    }
  }

  "Fen with an initial cube offer from white" should {
    val fenStr = "5S,3,3s,1,5s,4,2S/5s,3,3S,1,5S,4,2s[] - - b 0 0 0w 1"
    val fen    = FEN(fenStr)
    val board  = Board(
      fen.pieces,
      History(),
      variant.Backgammon,
      fen.pocketData,
      fen.unusedDice,
      fen.cubeData
    )
    "have white as an owner and be under offer" in {
      board.cubeData.map(_.value) must_== Some(1)
      board.cubeData.map(_.owner) must_== Some(Some(Player.P1))
      board.cubeData.map(_.underOffer) must_== Some(true)
      board.cubeData.map(_.rejected) must_== Some(false)
      Forsyth.>>(Situation(board, fen.player.getOrElse(Player.P1))).value must_== fenStr
    }
  }

  "Fen with an initial cube offer from black" should {
    val fenStr = "5S,3,3s,1,5s,4,2S/5s,3,3S,1,5S,4,2s[] - - w 0 0 0b 1"
    val fen    = FEN(fenStr)
    val board  = Board(
      fen.pieces,
      History(),
      variant.Backgammon,
      fen.pocketData,
      fen.unusedDice,
      fen.cubeData
    )
    "have white as an owner and be under offer" in {
      board.cubeData.map(_.value) must_== Some(1)
      board.cubeData.map(_.owner) must_== Some(Some(Player.P2))
      board.cubeData.map(_.underOffer) must_== Some(true)
      board.cubeData.map(_.rejected) must_== Some(false)
      Forsyth.>>(Situation(board, fen.player.getOrElse(Player.P1))).value must_== fenStr
    }
  }

  "Fen with an accepted cube offer from white" should {
    val fenStr = "5S,3,3s,1,5s,4,2S/5s,3,3S,1,5S,4,2s[] - - w 0 0 1W 1"
    val fen    = FEN(fenStr)
    val board  = Board(
      fen.pieces,
      History(),
      variant.Backgammon,
      fen.pocketData,
      fen.unusedDice,
      fen.cubeData
    )
    "have white as an owner and be under offer" in {
      board.cubeData.map(_.value) must_== Some(2)
      board.cubeData.map(_.owner) must_== Some(Some(Player.P1))
      board.cubeData.map(_.underOffer) must_== Some(false)
      board.cubeData.map(_.rejected) must_== Some(false)
      Forsyth.>>(Situation(board, fen.player.getOrElse(Player.P1))).value must_== fenStr
    }
  }

  "Fen with an accepted cube offer from black" should {
    val fenStr = "5S,3,3s,1,5s,4,2S/5s,3,3S,1,5S,4,2s[] - - w 0 0 1B 1"
    val fen    = FEN(fenStr)
    val board  = Board(
      fen.pieces,
      History(),
      variant.Backgammon,
      fen.pocketData,
      fen.unusedDice,
      fen.cubeData
    )
    "have white as an owner and be under offer" in {
      board.cubeData.map(_.value) must_== Some(2)
      board.cubeData.map(_.owner) must_== Some(Some(Player.P2))
      board.cubeData.map(_.underOffer) must_== Some(false)
      board.cubeData.map(_.rejected) must_== Some(false)
      Forsyth.>>(Situation(board, fen.player.getOrElse(Player.P1))).value must_== fenStr
    }
  }

  "Fen with an initial cube offer from white" should {
    val fenStr = "5S,3,3s,1,5s,4,2S/5s,3,3S,1,5S,4,2s[] - - b 0 0 1w 1"
    val fen    = FEN(fenStr)
    val board  = Board(
      fen.pieces,
      History(),
      variant.Backgammon,
      fen.pocketData,
      fen.unusedDice,
      fen.cubeData
    )
    "have white as an owner and be under offer" in {
      board.cubeData.map(_.value) must_== Some(2)
      board.cubeData.map(_.owner) must_== Some(Some(Player.P1))
      board.cubeData.map(_.underOffer) must_== Some(true)
      board.cubeData.map(_.rejected) must_== Some(false)
      Forsyth.>>(Situation(board, fen.player.getOrElse(Player.P1))).value must_== fenStr
    }
  }

  "Fen with an initial cube offer from black" should {
    val fenStr = "5S,3,3s,1,5s,4,2S/5s,3,3S,1,5S,4,2s[] - - w 0 0 1b 1"
    val fen    = FEN(fenStr)
    val board  = Board(
      fen.pieces,
      History(),
      variant.Backgammon,
      fen.pocketData,
      fen.unusedDice,
      fen.cubeData
    )
    "have white as an owner and be under offer" in {
      board.cubeData.map(_.value) must_== Some(2)
      board.cubeData.map(_.owner) must_== Some(Some(Player.P2))
      board.cubeData.map(_.underOffer) must_== Some(true)
      board.cubeData.map(_.rejected) must_== Some(false)
      Forsyth.>>(Situation(board, fen.player.getOrElse(Player.P1))).value must_== fenStr
    }
  }

  "Fen with an rejected cube offer" should {
    val fenStr = "5S,3,3s,1,5s,4,2S/5s,3,3S,1,5S,4,2s[] - - w 0 0 1bx 1"
    val fen    = FEN(fenStr)
    val board  = Board(
      fen.pieces,
      History(),
      variant.Backgammon,
      fen.pocketData,
      fen.unusedDice,
      fen.cubeData
    )
    val situation = Situation(
      board,
      fen.player.getOrElse(Player.P1)
    )
    "has a rejected cube and game is over" in {
      board.cubeData.map(_.rejected) must_== Some(true)
      Forsyth.>>(situation).value must_== fenStr
      situation.end must_== true
    }
  }

}
