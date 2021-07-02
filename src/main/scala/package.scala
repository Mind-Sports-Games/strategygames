import ornicar.scalalib
import chess._
import draughts._

package object strategy extends scalalib.Common with scalalib.OrnicarOption with scalalib.OrnicarBoolean {



  val White = Option[(chess.White, draughts.White)]
  val Black = Option[(chess.Black, draughts.Black)]

  type Direction  = Option[(chess.Direction, draughts.Direction)]
  type Directions = Option[(chess.Directions, draughts.Directions)]

  type PieceMap   = Option[(chess.PieceMap, draughts.PieceMap)]

  type PositionHash = Option[(chess.PositionHash, draughts.PositionHash)]

  //chess only
  //type MoveOrDrop = Either[Move, Drop]
}
