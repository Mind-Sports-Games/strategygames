import ornicar.scalalib
import draughts._

package object strategy extends scalalib.Common with scalalib.OrnicarOption with scalalib.OrnicarBoolean {
  def White(lib: GameLib) = lib match {
    case GameLib.Draughts => Color.Draughts(draughts.White)
    case GameLib.Chess => Color.Chess(chess.White)
  }

  def Black(lib: GameLib) = lib match {
    case GameLib.Draughts => Color.Draughts(draughts.Black)
    case GameLib.Chess => Color.Chess(chess.Black)
  }

}
