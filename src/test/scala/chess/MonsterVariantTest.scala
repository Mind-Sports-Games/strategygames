package strategygames.chess

import cats.syntax.option._

import strategygames.{ P1, P2, Player, Status }
import strategygames.chess.variant.Monster
import strategygames.chess.format.FEN

class MonsterVariantTest extends ChessTest {

  "Monster chess" should {

    "Black be in check two moves away" in {
      import Pos._
      "from init" in {
        val game        = fenToGame(Monster.initialFen, Monster)
        val successGame = game flatMap (_.playMoves(
          E2 -> E4,
          E4 -> E5,
          F7 -> F6,
          E1 -> E2,
          E5 -> F6
        ))
        successGame must beValid.like { case game =>
          // normally 21 moves are available to due to the kingSafety in Monster its only 4
          game.situation.moves.values.flatten.size must_== 4
        }
      }
    }

    "White have to work towards getting out of check if it requires two moves" in {
      import Pos._
      "from init" in {
        val game        = fenToGame(Monster.initialFen, Monster)
        val successGame = game flatMap (_.playMoves(
          F2 -> F4,
          E1 -> F1,
          G7 -> G5,
          F4 -> G5,
          F1 -> G1,
          E7 -> E6,
          G5 -> G6,
          G6 -> H7,
          D8 -> G5,
          G1 -> H1,
          E2 -> E3,
          H8 -> H7
        ))
        successGame must beValid.like { case game =>
          // without kingSafety checks 8 moves are available
          game.situation.moves.values.flatten.size must_== 2
        }
      }
    }
  }
}
