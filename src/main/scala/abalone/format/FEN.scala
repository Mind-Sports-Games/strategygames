package strategygames.abalone.format

import strategygames.Player
import strategygames.abalone.variant.Variant
import strategygames.abalone.{BoardType, P1, P2, Piece, PieceMap, Role}

final case class FEN(value: String) extends AnyVal {
  override def toString = value

  // Notice cells are described from bottom left to top right in the FEN
  def pieces(boardType: BoardType): PieceMap = value
    .split(' ')(0)
    .split('/')
    .flatMap {
      _.toCharArray
    }
    .flatMap {
      case c if c.isDigit => Array.fill(c.asDigit)('1')
      case c              => Array(c)
    }
    .zip(boardType.cellList)
    .flatMap {
      case (piece, pos) if piece == Role.defaultRole.forsythUpper => Some((pos, Piece(P1, Role.defaultRole)))
      case (piece, pos) if piece == Role.defaultRole.forsyth      => Some((pos, Piece(P2, Role.defaultRole)))
      case _                                                      => None
    }
    .toMap

  def player1Score: Int = intFromFen(1).getOrElse(0)

  def player2Score: Int = intFromFen(2).getOrElse(0)

  def prevPlayer(variant: Variant): Option[Player] = {
    if (!variant.hasPrevPlayer) return None

    value
      .split(' ')
      .lift(4)
      .flatMap(_.headOption)
      .flatMap(Player.apply)
      .map(!_)
  }

  def player: Option[Player] = value
    .split(' ')
    .lift(3)
    .flatMap(_.headOption)
    .flatMap(Player.apply)
    .map(!_)

  def halfMovesSinceLastCapture(variant: Variant): Option[Int] = intFromFen(
    if (variant.hasPrevPlayer) 5 else 4
  )

  def fullMove(variant: Variant): Option[Int] = intFromFen(if (variant.hasPrevPlayer) 6 else 5)

  def ply(variant: Variant): Option[Int] =
    fullMove(variant) map { fm =>
      fm * 2 - (if (player.exists(_.p1)) 2 else 1)
    }

  def initial = value == Forsyth.initial.value

  private def intFromFen(index: Int): Option[Int] =
    value.split(' ').lift(index).flatMap(_.toIntOption)
}

object FEN {
  def clean(source: String): FEN = FEN(source.replace("_", " ").trim)
}
