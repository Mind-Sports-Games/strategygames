package strategygames.backgammon.format

import strategygames.Player
import strategygames.backgammon.{ Piece, PieceMap, Pos, Role }

final case class FEN(value: String) extends AnyVal {

  override def toString = value

  def board: String = removePockets(value.takeWhile(_ != ' '))

  def player: Option[Player] =
    value.split(' ').lift(3) flatMap (_.headOption) flatMap Player.apply

  def player1Score: Int = intFromFen(4).getOrElse(0)

  def player2Score: Int = intFromFen(5).getOrElse(0)

  def fullMove: Option[Int] = intFromFen(FEN.fullMoveIndex)

  def ply: Option[Int] =
    fullMove map { fm =>
      fm * 2 - (if (player.exists(_.p1)) 2 else 1)
    }

  private def intFromFen(index: Int): Option[Int] =
    value.split(' ').lift(index).flatMap(_.toIntOption)

  private def stoneArray: Array[String] =
    (board.split('/')(1).split(',').reverse ++ board.split('/')(0).split(','))
      .map(c =>
        c.toString() match {
          case x if 1 to 12 map (_.toString) contains x => Array.fill(x.toInt)("0")
          case x                                        => Array(x)
        }
      )
      .flatten
      .toArray

  def pieces: PieceMap = stoneArray.zipWithIndex
    .filterNot { case (s, _) => s == "0" }
    .map { case (pieceString, index) =>
      (Pos(index), (Role.defaultRole, pieceString))
    }
    .map { case (Some(pos), (r, ps)) =>
      ((pos, (Piece(
        if (ps.takeRight(1) == "S") Player.P1 else Player.P2,
        r
      ),
      ps.dropRight(1).toInt)))
    }
    .toMap

  private def removePockets(fen: String): String = {
    val start = fen.indexOf("[", 0)
    val end   = fen.indexOf("]", start)
    if (start > 0 && end > 0)
      fen.substring(0, start) + fen.substring(end + 1, fen.length)
    else fen
  }

  def initial = value == Forsyth.initial.value

}

object FEN {

  def clean(source: String): FEN = FEN(source.replace("_", " ").trim)

  def fullMoveIndex: Int = 6

}
