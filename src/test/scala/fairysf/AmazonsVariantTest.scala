package strategygames.fairysf

import strategygames.format.{ FEN => StratFen, Forsyth => StratForsyth, Uci => StratUci }
import strategygames.variant.{ Variant => StratVariant }
import variant.Amazons
import strategygames.fairysf.format.{ FEN, Forsyth }

import strategygames.{ Player, Status }

class AmazonsVariantTest extends FairySFTest {

  val initialFen = FEN(
    "3q2q3/10/10/q8q/10/10/Q8Q/10/10/3Q2Q3[PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPpppppppppppppppppppppppppppppppppppppppppppppp] w - - 0 1"
  )

  val amazonsGame = Vector(
    "d1d6",
    "P@g9",
    "d10f10",
    "P@e10",
    "a4f9",
    "P@e9",
    "a7a10",
    "P@b10",
    "d6a9",
    "P@b9",
    "j7j10",
    "P@h10",
    "j4j9",
    "P@h9",
    "j10i10",
    "P@i9" // then after j9j10,P@j9 p2 has no moves
  ).map(Vector(_))

  def fromMoves(moves: Vector[String]): Game =
    Replay.gameMoveWhileValid(moves, initialFen, variant.Amazons)._2.last._1

  "Amazons" should {

    /*"not have winner from start position" in {
      val game = fenToGame(initialFen, Amazons)
      game must beValid.like {
        case game => {
          game.situation.end must beFalse
          game.situation.status == None must beTrue
          game.situation.winner == None must beTrue
        }
      }
    }

    "80 move from start pos" in {
      // val game = fenToGame(initialFen, Amazons)
      val game = Game(Amazons)
      game.situation.moves.size must_== 4
      game.situation.moves.toList(0)._2.size must_== 20
      game.situation.moves.toList.map(_._2.size).sum must_== 80
      // strategygames.fairysf.Game.apply(strategygames.fairysf.variant.Amazons).situation.moves.toList.map(_._2.size).sum
    }

    "P1 win in example game" in {
      val replay  = Replay.gameWithUciWhileValid(amazonsGame, initialFen, variant.Amazons)
      val game    = replay._2.last._1
      game.situation.moves.keys.size must_== 4
      game.situation.moves.keys.filter(_ == Pos.J9).size must_== 1
      game.situation.moves(Pos.J9).filter(_.dest == Pos.J10).size must_== 1
      val game2   = game.apply(game.situation.moves(Pos.J9).filter(_.dest == Pos.J10)(0))
      game2.situation.dropsAsDrops.size must_== 9
      game2.situation.dropsAsDrops.filter(_.pos.key == "j9").size must_== 1
      val gameEnd = game2.applyDrop(game2.situation.dropsAsDrops.filter(_.pos.key == "j9")(0))
      gameEnd.situation.board.variant.exportBoardFen(game.situation.board) must_== FEN(
        "qp2pqqpq1/QP2PQPPpQ/10/10/10/10/10/10/10/6Q3[PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPpppppppppppppppppppppppppppppppppppppppppppppp] w - - 8 5"
      )
      gameEnd.situation.end must beTrue
      gameEnd.situation.status must_== Some(Status.VariantEnd)
      gameEnd.situation.winner must_== Some(Player.P1)
    }
  }

  "Amazon Fens" should {
    "Ensure fen contains half move" in {
      val onePlyGame        = fromMoves(Vector("g1j1"))
      val onePlyFen         = Forsyth >> onePlyGame
      onePlyFen.value must contain(" ½")
      onePlyFen.value must contain(" ½g1j1")
      val onePlyGameFromFen = Game(Some(Amazons), Some(onePlyFen))
      onePlyGameFromFen.situation.lastMove must beSome.like { case move =>
        move.toUci.uci must_== "g1j1"
      }

      val twoPlyGame         = fromMoves(Vector("g1j1", "P@i2"))
      val twoPlyFen          = Forsyth >> twoPlyGame
      twoPlyFen.value must not contain " ½"
      val twoPlayGameFromFen = Game(Some(Amazons), Some(twoPlyFen))
      twoPlayGameFromFen.situation.lastMove must beNone

    }

    "Creating game from fen after move one should allow for drops" in {
      val initialGame  = fromMoves(Vector("g1j1"))
      val afterPly1Fen = Forsyth >> initialGame
      afterPly1Fen.value must contain(" ½")
      afterPly1Fen.value must contain(" ½g1j1")
      initialGame.situation.drops must beSome.like {
        case drops => {
          drops == List() must beFalse
          val game = Game(Some(Amazons), Some(afterPly1Fen))
          game.situation.drops must beSome.like {
            case dropsFromFEN => {
              dropsFromFEN must_== drops
            }
          }
        }
      }
    }*/

    /*"FEN parsing turn numbers numbers" in {
      val game0 = Game(Amazons)
      println(s"--> 0: ${(Forsyth >> game0).value}")
      (Forsyth >> game0).value must contain("0 1")

      val game1 = fromMoves(Vector("g1j1"))
      game1.turns must_== 1
      println(s"--> 1: ${game1.turns}")
      println(s"--> 1: ${(Forsyth >> game1).value}")
      (Forsyth >> game1).value must contain("0 1")
      (Forsyth >> game1).value must contain("½g1j1")

      val game2 = fromMoves(Vector("g1j1", "P@i2"))
      game2.turns must_== 2
      println(s"--> 2: ${game2.turns}")
      println(s"--> 2: ${(Forsyth >> game2).value}")
      (Forsyth >> game2).value must contain("1 1")

      val game3 = fromMoves(Vector("g1j1", "P@i2", "j7i6"))
      game3.turns must_== 3
      println(s"--> 3: ${game3.turns}")
      println(s"--> 3: ${(Forsyth >> game3).value}")
      (Forsyth >> game3).value must contain("1 1")
      (Forsyth >> game3).value must contain("½j7i6")

      val game4 = fromMoves(Vector("g1j1", "P@i2", "j7i6", "P@i5"))
      game4.turns must_== 4
      println(s"--> 4: ${game4.turns}")
      println(s"--> 4: ${(Forsyth >> game4).value}")
      (Forsyth >> game4).value must contain("2 2")

      val game5 = fromMoves(Vector("g1j1", "P@i2", "j7i6", "P@i5", "j1g1"))
      game5.turns must_== 5
      println(s"--> 5: ${game5.turns}")
      println(s"--> 5: ${(Forsyth >> game5).value}")
      (Forsyth >> game5).value must contain("2 2")

      val game6 = fromMoves(Vector("g1j1", "P@i2", "j7i6", "P@i5", "j1g1", "P@g2"))
      game6.turns must_== 6
      println(s"--> 6: ${game6.turns}")
      println(s"--> 6: ${(Forsyth >> game6).value}")
      (Forsyth >> game6).value must contain("3 2")
    }*/

    "Ensuring fens keep pawns when using them to create games further in" in {
      val initialGame  = fromMoves(Vector("g1j1", "P@i2"))
      initialGame.turns must_== 2
      val afterPly2Fen = Forsyth >> initialGame
      println(afterPly2Fen)
      afterPly2Fen.value.toLowerCase() must contain("8p1")
      val gameFromFen  = Game(Some(Amazons), Some(afterPly2Fen))
      gameFromFen.turns must_== 2
      gameFromFen.situation.destinations must_== initialGame.situation.destinations

      val afterPly2FenAgain = Forsyth >> gameFromFen
      afterPly2FenAgain.value.toLowerCase() must contain("8p1")
      afterPly2Fen must_== afterPly2FenAgain

      val replay2              = Replay.gameMoveWhileValid(Vector("j7i6"), afterPly2Fen, variant.Amazons)
      val gameFromFenAfterMove = replay2._2.last._1
      gameFromFenAfterMove.turns must_== 3
      val afterPly3Fen         = Forsyth >> gameFromFenAfterMove
      afterPly3Fen.value.toLowerCase() must contain("8p1")
      afterPly3Fen.value.toLowerCase() must contain(" ½j7i6")

      val replay3               = Replay.gameMoveWhileValid(Vector("P@i5"), afterPly3Fen, variant.Amazons)
      val gameFromFenAfterMove3 = replay3._2.last._1
      gameFromFenAfterMove3.turns must_== 4
      val afterPly4Fen          = Forsyth >> gameFromFenAfterMove3
      afterPly4Fen.value.toLowerCase() must not contain "½j7i6"
      afterPly4Fen.value.toLowerCase() must contain("8p1")
    }
  }

}

class AmazonsVariantTestIsometry extends strategygames.chess.ChessTest {
  "Test Every move can be loaded from fen" in {
    val gameFamily   = Amazons.gameFamily
    val lib          = gameFamily.gameLogic
    val stratVariant = StratVariant(lib, Amazons.key).get

    isometryTest(lib).testEveryMoveLoadFenIsometry(StratFen(lib, Amazons.initialFen.value), stratVariant)(
      List(
        "d1d6",
        "P@g9",
        "d10f10",
        "P@e10",
        "a4f9",
        "P@e9",
        "a7a10",
        "P@b10",
        "d6a9",
        "P@b9",
        "j7j10",
        "P@h10",
        "j4j9",
        "P@h9",
        "j10i10",
        "P@i9" // then after j9j10,P@j9 p2 has no moves
      ).map(uciStr => StratUci(lib, gameFamily, uciStr).get)
    ) must beValid.like(gameData => {
      val fen1 = StratForsyth.>>(lib, gameData.game)
      val fen2 = StratForsyth.>>(lib, gameData.fenGame)
      fen1 must_== fen2
    })
  }
}
