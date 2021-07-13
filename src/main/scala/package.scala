import ornicar.scalalib

package object strategygames extends scalalib.Common with scalalib.OrnicarOption with scalalib.OrnicarBoolean {

  def White(lib: GameLib) = lib match {
    case GameLib.Draughts() => Color.Draughts(draughts.White)
    case GameLib.Chess()    => Color.Chess(chess.White)
  }

  def Black(lib: GameLib) = lib match {
    case GameLib.Draughts() => Color.Draughts(draughts.Black)
    case GameLib.Chess()    => Color.Chess(chess.Black)
  }

  type PieceMap = Map[Pos, Piece]

  type PositionHash = Array[Byte]

  type MoveOrDrop = Either[Move, chess.Drop]

}
