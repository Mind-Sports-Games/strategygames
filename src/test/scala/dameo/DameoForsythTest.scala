package strategygames.dameo

import org.specs2.matcher.ValidatedMatchers
import cats.data.Validated.Valid

import format.{ FEN, Forsyth }

class DameoForsythTest extends DameoTest with ValidatedMatchers {

  "situationFromFen" should {
    val fenFromVariant   = variant.Dameo.initialFen
    val situationFromFen = Forsyth.<<(fenFromVariant)

    "create the starting setup Situation" in {
      val board = Board(fenFromVariant.pieces, variant.Dameo)
      situationFromFen.get.board.pieces === board.pieces
      situationFromFen.get.player === P1
    }

    "export the starting Situation correctly" in {
      Forsyth.>>(situationFromFen.get).value === variant.Dameo.initialFen.value
    }
  }

  "situationPlusFromFen" should {
    val fenFromVariant       = variant.Dameo.initialFen
    val situationPlusFromFen = Forsyth.<<<(fenFromVariant)

    "create the starting setup as SituationPlus" in {
      val board = Board(fenFromVariant.pieces, variant.Dameo)
      situationPlusFromFen.get.situation.board.pieces === board.pieces
      situationPlusFromFen.get.situation.player === P1

    }
  }

  "fen with ghosts and kings" should {
    val fen = FEN("W:WKa5,Kb2,c4:BGa8,Pe8,f6,Kf7:H0:F1")

    "count the ghosts" in {
      Forsyth.countGhosts(fen) === 2
    }

    "count the kings" in {
      Forsyth.countKings(fen) === 3
    }

    "export the Situation correctly" in {
      val situation = Forsyth.<<(fen)
      Forsyth.>>(situation.get).value === fen.value
    }
  }

  "fen from situationPlus" should {
    "have the correct clocks" in {

      val board     = Board(variant.Dameo.initialFen.pieces, variant.Dameo)
      val situation = Situation(board, P1)
      var game      = Game(situation)

      /* Starting situation */
      game.plies === 0
      game.turnCount === 0
      var fen = Forsyth.>>(game)
      fen.fullMove.get === 1

      /* Single-action turn P1 */
      game = applyGameMove(game, Pos.D3, Pos.D4)
      game.plies === 1
      game.turnCount === 1
      fen = Forsyth.>>(game)
      fen.fullMove.get === 1

      /* Single-action turn P2 */
      game = applyGameMove(game, Pos.D6, Pos.D5)
      game.plies === 2
      game.turnCount === 2
      fen = Forsyth.>>(game)
      fen.fullMove.get === 2

      /* Multi-action turn P1 */
      game = applyGameMove(game, Pos.D4, Pos.D6)
      game.plies === 3
      game.turnCount === 2
      fen = Forsyth.>>(game)
      fen.fullMove.get === 2

      game = applyGameMove(game, Pos.D6, Pos.B6) // 2nd action
      game.plies === 4
      game.turnCount === 3
      fen = Forsyth.>>(game)
      fen.fullMove.get === 2

      /* Single-action turn P2 */
      game = applyGameMove(game, Pos.B7, Pos.B5)
      game.plies === 5
      game.turnCount === 4
      fen = Forsyth.>>(game)
      fen.fullMove.get === 3
    }
  }

  def applyGameMove(game: Game, from: Pos, to: Pos): Game = {
    val move: Move = (game.apply(from, to) match {
      case Valid((_, mv: Move)) => Some(mv)
      case _                    => None
    }).get
    game.apply(move)
  }
}
