package strategygames.entropy.format

import strategygames.Player
import strategygames.entropy.{ File, Piece, PieceMap, Pos, Rank, Role }

final case class FEN(value: String) extends AnyVal {

  override def toString = value

  private def parts: Array[String] = value.split(' ')

  private def field(index: Int): Option[String] = parts.lift(index)

  private def intField(index: Int): Option[Int] = field(index).flatMap(_.toIntOption)

  // the board, without the pocket that trails it
  def boardPart: String = field(0).getOrElse("").takeWhile(_ != '[')

  def pocketPart: Option[String] =
    field(0).flatMap { f =>
      val open = f.indexOf('[')
      if (open < 0) None else Some(f.substring(open + 1).takeWhile(_ != ']'))
    }

  def player: Option[Player] =
    field(1) flatMap (_.headOption) flatMap Player.apply

  def player1Score: Int = intField(2).getOrElse(0)

  def player2Score: Int = intField(3).getOrElse(0)

  // an empty board cannot say by itself whether it is the start of round one or round two
  def round: Int = intField(4).getOrElse(1)

  def fullMove: Option[Int] = intField(5)

  def ply: Option[Int] =
    fullMove map { fm =>
      fm * 2 - (if (player.exists(_.p1)) 2 else 1)
    }

  def pieces: PieceMap =
    boardPart
      .split('/')
      .zipWithIndex
      .flatMap { case (rankStr, rankIndexFromTop) =>
        val rankIndex = Rank.allSize - 1 - rankIndexFromTop
        var fileIndex = 0
        rankStr.flatMap { c =>
          if (c.isDigit) {
            fileIndex += c.asDigit
            None
          } else {
            val placed =
              for {
                file  <- File(fileIndex)
                rank  <- Rank(rankIndex)
                piece <- Piece.fromChar(c)
              } yield Pos(file, rank) -> piece
            fileIndex += 1
            placed
          }
        }
      }
      .toMap

  def pocketRoles: List[(Player, Role)] =
    pocketPart.getOrElse("").toList.flatMap { c =>
      Piece.fromChar(c).map(p => p.player -> p.role)
    }

}
