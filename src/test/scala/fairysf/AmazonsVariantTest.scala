package strategygames.fairysf

import strategygames.{ Player, Status }
import strategygames.format.{ FEN => StratFen, Forsyth => StratForsyth, Uci => StratUci }
import strategygames.variant.{ Variant => StratVariant }
import strategygames.GameFamily
import variant.Amazons
import format.{ FEN, Forsyth, Uci }

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

  def makeMove(game: Game, uci: String): Option[Game] = {
    Uci(GameFamily.Amazons(), uci).flatMap(uci => game.apply(uci).toOption.map(_._1))
  }

  "Amazons" should {

    "not have winner from start position" in {
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
        "qp2pqqpq1/Qp2pQpppQ/10/10/10/10/10/10/10/6Q3[PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPpppppppppppppppppppppppppppppppppppppppppppppp] w - - 8 5"
      )
      gameEnd.situation.end must beTrue
      gameEnd.situation.status must_== Some(Status.VariantEnd)
      gameEnd.situation.winner must_== Some(Player.P1)
    }
  }

  "Amazon Fens" should {
    "Ensure fen contains half move" in {
      val emptyGame         = fenToGame(Amazons.initialFen, Amazons).toOption.get
      val onePlyGame        = makeMove(emptyGame, "g1j1").get
      val onePlyFen         = Forsyth.>>(onePlyGame)
      onePlyFen.value must contain(" ½")
      onePlyFen.value must contain(" ½g1j1")
      val onePlyGameFromFen = Game(Some(Amazons), Some(onePlyFen))
      onePlyGameFromFen.situation.lastMove must beSome.like { case move =>
        move.toUci.uci must_== "g1j1"
      }

      val twoPlyGame         = makeMove(onePlyGame, "P@i2").get
      val twoPlyFen          = Forsyth.>>(twoPlyGame)
      twoPlyFen.value must not contain " ½"
      val twoPlayGameFromFen = Game(Some(Amazons), Some(twoPlyFen))
      twoPlayGameFromFen.situation.lastMove must beNone

    }

    "Creating game from fen after move one should allow for drops" in {
      val emptyGame    = fenToGame(Amazons.initialFen, Amazons).toOption.get
      val onePlyGame   = makeMove(emptyGame, "g1j1").get
      val afterPly1Fen = Forsyth.>>(onePlyGame)
      afterPly1Fen.value must contain(" ½")
      afterPly1Fen.value must contain(" ½g1j1")
      onePlyGame.situation.drops must beSome.like {
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
    }

    "Ensuring fens keep pawns when using them to create games further in" in {
      val emptyGame    = fenToGame(Amazons.initialFen, Amazons).toOption.get
      val gamePly1     = makeMove(emptyGame, "d1d6").get
      gamePly1.turnCount must_== 0
      gamePly1.plies must_== 1
      val afterPly1Fen = Forsyth.>>(gamePly1)
      afterPly1Fen.value.toLowerCase() must contain("d1d6")
      val gameFromFen  = Game(Some(Amazons), Some(afterPly1Fen))
      gameFromFen.turnCount must_== 0
      gameFromFen.plies must_== 1
      gameFromFen.situation.destinations must_== gamePly1.situation.destinations

      val afterPly1FenAgain = Forsyth.>>(gameFromFen)
      afterPly1FenAgain.value.toLowerCase() must contain("d1d6")
      afterPly1Fen must_== afterPly1FenAgain

      val gamePly2     = makeMove(gamePly1, "P@g9").get
      gamePly2.turnCount must_== 1
      gamePly2.plies must_== 2
      val afterPly2Fen = Forsyth.>>(gamePly2)
      afterPly2Fen.value.toLowerCase() must not contain "8p1"
      afterPly2Fen.value.toLowerCase() must not contain " ½"

      val gamePly3     = makeMove(gamePly2, "d10f10").get
      gamePly3.turnCount must_== 1
      gamePly3.plies must_== 3
      val afterPly3Fen = Forsyth.>>(gamePly3)
      afterPly3Fen.value.toLowerCase() must contain("½d10f10")

      val gamePly4     = makeMove(gamePly3, "P@f9").get
      gamePly4.turnCount must_== 2
      gamePly4.plies must_== 4
      val afterPly4Fen = Forsyth.>>(gamePly4)
      afterPly4Fen.value.toLowerCase() must not contain "½d10f10"
    }
  }

}

class AmazonsVariantTestIsometry extends strategygames.chess.ChessTest {
  "Test Every move can be loaded from fen" in {
    val gameFamily   = Amazons.gameFamily
    val lib          = gameFamily.gameLogic
    val stratVariant = StratVariant(lib, Amazons.key).get

    _testEveryMoveLoadFenIsometry(lib, StratFen(lib, Amazons.initialFen.value), stratVariant)(
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
        "P@i9"
      ).map(uciStr => StratUci(lib, gameFamily, uciStr).get)
    ) must beValid.like(gameData => {
      val fen1 = StratForsyth.>>(lib, gameData.game)
      val fen2 = StratForsyth.>>(lib, gameData.fenGame)
      fen1 must_== fen2
    })
  }
}
