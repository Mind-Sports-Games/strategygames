package strategygames.abalone.format

import strategygames.Player
import strategygames.abalone.variant.Variant
import strategygames.abalone.{P1, P2, Piece, PieceMap, Role}

final case class FEN(value: String) extends AnyVal {
  override def toString = value

  // Notice cells are described from bottom left to top right in the FEN
  def pieces(variant: Variant): PieceMap = value
    .split(' ')(0)
    .split('/')
    .flatMap(FEN.decodeFenRow)
    .zip(variant.boardType.cellList)
    .flatMap {
      case (piece, pos) if piece == Role.defaultRole.forsythUpper => Some((pos, Piece(P1, Role.defaultRole)))
      case (piece, pos) if piece == Role.defaultRole.forsyth      => Some((pos, Piece(P2, Role.defaultRole)))
      case _                                                      => None
    }
    .toMap

  def player1Score: Int = intFromFen(1).getOrElse(0)

  def player2Score: Int = intFromFen(2).getOrElse(0)

  def player: Option[Player] = value
    .split(' ')
    .lift(3)
    .flatMap(_.headOption)
    .flatMap(Player.apply)
    .map(!_)

  def halfMovesSinceLastCapture: Option[Int] = intFromFen(4)
  def fullMove: Option[Int]                  = intFromFen(5)
  def pliesRemainingThisTurn: Option[Int]    = intFromFen(6)

  private def intFromFen(index: Int): Option[Int] =
    value.split(' ').lift(index).flatMap(_.toIntOption)

  def initial = value == Forsyth.initial.value
}

object FEN {
  def clean(source: String): FEN = FEN(source.replace("_", " ").trim)

  private def decodeFenRow(row: String): Array[Char] =
    "[0-9]+|[^0-9]".r
      .findAllIn(row)
      .flatMap {
        case s if s.head.isDigit => Array.fill(s.toInt)('1')
        case s                   => s.toCharArray
      }
      .toArray
}
