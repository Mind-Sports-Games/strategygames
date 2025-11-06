package strategygames.fairysf

import strategygames.Player
import strategygames.fairysf.format.FEN

class FlipelloVariantTest extends FairySFTest {

  // https://playstrategy.dev/4eCFkjwt
  val flipelloGameEndsEarly = List(
    "P@e3",
    "P@f5",
    "P@e6",
    "P@d7",
    "P@c6",
    "P@d3",
    "P@f4",
    "P@d6",
    "P@c5",
    "P@b5",
    "P@b7",
    "P@b6",
    "P@c7",
    "P@a8",
    "P@f6",
    "P@e7",
    "P@a7",
    "P@b8",
    "P@f7",
    "P@c8", // 10 moves each (20 ply)
    "P@c3",
    "P@a6",
    "P@a5",
    "P@a4",
    "c3c3", // first pass for P1 (1-1)
    "P@d8",
    "c3c3", // another pass for P1 (1-2)
    "P@b4",
    "P@a3",
    "P@a2",
    "c3c3", // second set of passing for P1 (2-1)
    "P@b3",
    "c3c3", // second set of passing for P1 (2-2)
    "P@c4",
    "c3c3", // second set of passing for P1 (2-3)
    "P@c2",
    "P@b2",
    "P@f3",
    "P@b1",
    "P@a1", // 20 moves each (40 ply)
    "P@g3",
    "P@c1",
    "d3d3", // third set of passing for P1 (3-1)
    "P@d2",
    "P@d1",
    "P@e1",
    "d3d3", // fourth set of passing for P1 (4-1)
    "P@e2",
    "e3e3", // fourth set of passing for P1 (4-2)
    "P@f2",
    "P@f1",
    "P@g1",
    "f3f3", // fifth set of passing for P1 (5-1)
    "P@h3",
    "P@g2",
    "P@h1",
    "P@h2",
    "P@g4",
    "P@h4",
    "P@h5", // 30 moves each (60 ply)
    "P@g5",
    "P@h6",
    "e5e5", // sixth set of passing for P1 (6-1)
    "P@g6",
    "P@g7",
    "P@h8",
    "P@h7",
    "P@f8",
    "a1a1", // P2 has moves but only has a valid move to pass for some reason!?
    "e5e5"  // P1 then has no moves and has to pass ending the game
  )

  "Flipello" should {

    "Initialise correctly" in {
      val position =
        FEN("8/8/8/3pP3/3Pp3/8/8/8[PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPpppppppppppppppppppppppppppppp] w 0 1")
      val game     = fenToGame(position, variant.Flipello)
      game must beValid.like {
        case game => {
          game.situation.winner == None must beTrue
        }
      }
    }

    "have legal moves even after passing" in {
      val position  = Api.positionFromVariant(variant.Flipello)
      val position2 = position.makeMoves(flipelloGameEndsEarly.dropRight(5))
      // just before P2 move (which engine says to pass incorrectly)

      position2.legalMoves.size must_== 5
      position2.gameEnd must_== false
    }

    "P2 win from move list" in {
      val position  = Api.positionFromVariant(variant.Flipello)
      val position2 = position.makeMoves(flipelloGameEndsEarly.dropRight(2))
      val game      = fenToGame(position2.fen, variant.Flipello)

      game must beValid.like {
        case game => {
          game.situation.end must beTrue
          game.situation.winner must_== Some(Player.P2)
        }
      }
    }

    "P1 wins antiflipello from same move list" in {
      val position  = Api.positionFromVariant(variant.AntiFlipello)
      val position2 = position.makeMoves(flipelloGameEndsEarly.dropRight(2))
      val game      = fenToGame(position2.fen, variant.AntiFlipello)

      game must beValid.like {
        case game => {
          game.situation.end must beTrue
          game.situation.winner must_== Some(Player.P1)
        }
      }
    }

    "Should detect pending double passes as a game end" in {
      val position = FEN(
        "3P4/3P3P/3PPPPP/1PPPPPPP/3PPPPP/1p1PPPPP/2pp1P2/1ppp4[PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPpppppppppppppppppppppppppppppppppppppppppppppppppppppppppppp] b - - 0 15"
      )
      val game     = fenToGame(position, variant.Flipello)
      game must beValid
        .like {
          case game => {
            game.situation.end must beTrue
          }
        }
    }

  }
  "Octagon Flipello variant" should {
    "offers pass when no moves available" in {
      //https://playstrategy.dev/VpVK65rI
      val actionStrs = List(
        "P@g5",
        "P@g6",
        "P@h7",
        "P@h6",
        "P@d7",
        "P@d6",
        "P@f7",
        "P@f4",
        "P@f3",
        "P@e4",
        "P@h4",
        "P@h5",
        "P@c6",
        "P@e7",
        "P@f8",
        "P@g4",
        "P@i4",
        "P@j4",
        "P@i6",
        "P@j6",
        "P@j7",
        "P@j8",
        "P@g3",
        "P@e3",
        "P@i5",
        "P@j5",
        "P@e2",
        "P@e1",
        "P@d1",
        "P@c1",
        "P@i3",
        "P@j3",
        "P@h3",
        "P@i2",
        "P@d5",
        "P@i7",
        "P@d3",
        "P@g7",
        "P@i8",
        "P@i9",
        "P@g8",
        "P@h8",
        "P@h9",
        "P@f2",
        "P@g1",
        "P@f1",
        "P@h2",
        "P@h1",
        "P@g2",
        "P@h10"
      )

      playActionStrs(actionStrs, variant = Some(variant.OctagonFlipello)) must beValid.like { g =>
        g.situation.end must_== false
        g.situation.board.apiPosition.legalMoves must_== Array("e2e2")
        g.situation.dropsAsDrops.pp("drops").size must_== 0
        g.situation.moves.pp("moves").size must_== 1
      }

    }
  }
}
