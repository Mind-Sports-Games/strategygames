package strategygames.abalone.format

import strategygames.Player
import strategygames.abalone.File
import strategygames.abalone.Piece
import strategygames.abalone.PieceMap
import strategygames.abalone.Pos
import strategygames.abalone.Stone

final case class FEN(value: String) extends AnyVal {
  // squares are described from topLeft to bottomRight in the FEN :
  // eg. Belgian Daisy: SS1ss/SSSsss/1SS1ss1/8/9/8/1ss1SS1/sssSSS/ss1SS
  // Snakes: sssss/s5/s6/s1SSSSS1/1s5S1/1sssss1S/6S/5S/SSSSS

  override def toString = value

  def pieces: PieceMap = squares.zipWithIndex.map {
      case (pieces, row) => pieces.zipWithIndex.map[Option[(Option[strategygames.abalone.Pos], strategygames.abalone.Piece)]] { 
        case (piece, index) => piece match {
          case Stone.forsyth => Some((Pos.at(index, (File.all.size - 1 - row)), new Piece(strategygames.abalone.P1, Stone)))
          case Stone.forsythUpper => Some((Pos.at(index, (File.all.size - 1 - row)), new Piece(strategygames.abalone.P2, Stone)))
          case _ => None
        }
      }.flatMap {
        case Some((Some(pos), piece)) => Some(pos -> piece)
        case _ => None
      }
    }.flatten.toMap

  def player1Score: Int = intFromFen(1).getOrElse(0)

  def player2Score: Int = intFromFen(2).getOrElse(0)

  def player: Option[Player] = value.split(' ').lift(3).flatMap(_.headOption).flatMap(Player.apply).map( !_ )

  def halfMovesSinceLastCapture: Option[Int] = intFromFen(4)

  def fullMove: Option[Int] = intFromFen(5)

  def ply: Option[Int] =
    fullMove map { fm =>
      fm * 2 - (if (player.exists(_.p1)) 2 else 1)
    }

  def initial = value == Forsyth.initial.value

  private def intFromFen(index: Int): Option[Int] =
    value.split(' ').lift(index).flatMap(_.toIntOption)

  // represents pieces on the Board, and '0' or '1' depending if the empty square is in the hexagon or not
  private def squares: Array[Array[Char]] = value.split(' ')(0).split('/').zipWithIndex.map {
      case (pieces, row) if (row < (File.all.size / 2 + 1)) => { Array.fill(Math.abs(row + (File.all.size / 2 + 1) - File.all.size)){'0'} ++ pieces.toArray }
      case (pieces, row) => { pieces.toArray ++ Array.fill(Math.abs((File.all.size / 2) - row)){'0'} }
    }.flatten.flatMap {
        case (d) if (d.isDigit && d.asDigit > 0) => Array.fill(d.asDigit){Array('1')}        
        case (d) => Array.fill(1){Array(d)}
    }.flatten.grouped(File.all.size).toArray
}

object FEN {

  def clean(source: String): FEN = FEN(source.replace("_", " ").trim)
}
