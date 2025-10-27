package strategygames.dameo

import org.specs2.matcher.ValidatedMatchers

import format.{ FEN, Forsyth }
import cats.data.Validated.Valid

class DameoForsythTest extends DameoTest with ValidatedMatchers {

  "situationFromFen" should {
    val fenFromVariant   = variant.Dameo.initialFen
    val situationFromFen = Forsyth.<<(fenFromVariant)

    "create the starting setup Situation" in {
      val board = Board(fenFromVariant.pieces, variant.Dameo)
      situationFromFen.get.board.pieces must_== board.pieces
      situationFromFen.get.player must_== P1
    }

    "export the starting Situation correctly" in {
      Forsyth.>>(situationFromFen.get).value must_== variant.Dameo.initialFen.value
    }
  }

  "situationPlusFromFen" should {
    val fenFromVariant       = variant.Dameo.initialFen
    val situationPlusFromFen = Forsyth.<<<(fenFromVariant)

    "create the starting setup as SituationPlus" in {
      val board = Board(fenFromVariant.pieces, variant.Dameo)
      situationPlusFromFen.get.situation.board.pieces must_== board.pieces
      situationPlusFromFen.get.situation.player must_== P1
    }
  }

  "fen with ghosts and kings" should {
    val fen = FEN("W:WKa5,Kb2,c4:BGa8,Pe8,f6,Kf7:H0:F1:P0")

    "count the ghosts" in {
      Forsyth.countGhosts(fen) must_== 2
    }

    "count the kings" in {
      Forsyth.countKings(fen) must_== 3
    }

    "export the Situation correctly" in {
      val situation = Forsyth.<<(fen)
      Forsyth.>>(situation.get).value must_== fen.value
    }
  }

  "fen from situationPlus" should {
    "have the correct clocks" in {
      val board = Board(variant.Dameo.initialFen.pieces, variant.Dameo)
      val situation = Situation(board, P1)
      var game = Game(situation)

      /* Starting situation */
      game.plies must_== 0
      game.turnCount must_== 0
      var fen = Forsyth.>>(game)
      fen.plies.get must_== 0
      fen.fullMove.get must_== 1

      /* Single-action turn P1 */
      game = applyGameMove(game, Pos.D3, Pos.D4)
      game.plies must_== 1
      game.turnCount must_== 1
      fen = Forsyth.>>(game)
      fen.plies.get must_== 1
      fen.fullMove.get must_== 1

      /* Single-action turn P2 */
      game = applyGameMove(game, Pos.D6, Pos.D5)
      game.plies must_== 2
      game.turnCount must_== 2
      fen = Forsyth.>>(game)
      fen.plies.get must_== 2
      fen.fullMove.get must_== 2

      /* Multi-action turn P1 */
      game = applyGameMove(game, Pos.D4, Pos.D6)
      game.plies must_== 3
      game.turnCount must_== 2
      fen = Forsyth.>>(game)
      fen.plies.get must_== 3
      fen.fullMove.get must_== 2

      game = applyGameMove(game, Pos.D6, Pos.B6) // 2nd action
      game.plies must_== 4
      game.turnCount must_== 3
      fen = Forsyth.>>(game)
      fen.plies.get must_== 4
      fen.fullMove.get must_== 2

      /* Single-action turn P2 */
      game = applyGameMove(game, Pos.B7, Pos.B5)
      game.plies must_== 5
      game.turnCount must_== 4
      fen = Forsyth.>>(game)
      fen.plies.get must_== 5
      fen.fullMove.get must_== 3
    }
  }

  def applyGameMove(game: Game, from: Pos, to: Pos): Game = {
    val move: Move = (game.apply(from, to) match
      {
        case Valid((_, mv: Move)) => Some(mv)
        case _ => None
      }).get
      game.apply(move)
  }
}
