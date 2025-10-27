package strategygames.dameo

import org.specs2.matcher.ValidatedMatchers
import strategygames.dameo.format.Uci

import cats.data.Validated.Valid

class DameoHistoryTest extends DameoTest with ValidatedMatchers {

  "history" should {
    "Populate lastTurn and currentTurn correctly" in {
      val board     = Board(variant.Dameo.initialFen.pieces, variant.Dameo)
      val situation = Situation(board, P1)
      var game      = Game(situation)

      /* Starting situation */
      game.board.history.lastTurn must_== List()
      game.board.history.currentTurn must_== List()

      /* Single-action move */
      game = applyGameMove(game, Pos.B1, Pos.E4)
      game.board.history.lastTurn must_== List(Uci.Move(Pos.B1, Pos.E4))
      game.board.history.currentTurn must_== List()

      /* Single-action move */
      game = applyGameMove(game, Pos.E6, Pos.E5)
      game.board.history.lastTurn must_== List(Uci.Move(Pos.E6, Pos.E5))
      game.board.history.currentTurn must_== List()

      /* Multi-action move */
      game = applyGameMove(game, Pos.E4, Pos.E6)
      game.board.history.lastTurn must_== List(Uci.Move(Pos.E6, Pos.E5))
      game.board.history.currentTurn must_== List(Uci.Move(Pos.E4, Pos.E6))

      game = applyGameMove(game, Pos.E6, Pos.G6)
      game.board.history.lastTurn must_== List(Uci.Move(Pos.E4, Pos.E6), Uci.Move(Pos.E6, Pos.G6))
      game.board.history.currentTurn must_== List()

      /* Single-action move */
      game = applyGameMove(game, Pos.G7, Pos.G5)
      game.board.history.lastTurn must_== List(Uci.Move(Pos.G7, Pos.G5))
      game.board.history.currentTurn must_== List()

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
