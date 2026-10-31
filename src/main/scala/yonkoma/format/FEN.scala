package strategygames.yonkoma.format

import strategygames.Player
import strategygames.yonkoma.{ File, Piece, PieceMap, Pos, Rank }

// "<board> <player> <full move number>", the board written rank by rank from the top,
// as in chess, with a run of unoccupied squares written as its length (which may have
// two digits on these boards), 'C' for a red checker and 'c' for a blue one
final case class FEN(value: String) extends AnyVal {

  override def toString = value

  private def parts: Array[String] = value.split(' ')

  private def field(index: Int): Option[String] = parts.lift(index)

  private def intField(index: Int): Option[Int] = field(index).flatMap(_.toIntOption)

  def boardPart: String = field(0).getOrElse("")

  def player: Option[Player] =
    field(1) flatMap (_.headOption) flatMap Player.apply

  def fullMove: Option[Int] = intField(2)

  // the starting position of either board size
  def initial: Boolean = strategygames.yonkoma.variant.Variant.all.exists(_.initialFen.value == value)

  def ply: Option[Int] =
    fullMove map { fm =>
      fm * 2 - (if (player.exists(_.p1)) 2 else 1)
    }

  // the ranks are listed from the top, so the number of ranks on the board is the
  // number of rank strings, and the first string is the highest rank
  def pieces: PieceMap = {
    val rankStrs = boardPart.split('/')
    rankStrs.zipWithIndex.flatMap { case (rankStr, rankIndexFromTop) =>
      val rankIndex = rankStrs.size - 1 - rankIndexFromTop
      var fileIndex = 0
      var run       = 0
      val placed    = scala.collection.mutable.ListBuffer.empty[(Pos, Piece)]
      rankStr.foreach { c =>
        if (c.isDigit) run = run * 10 + c.asDigit
        else {
          fileIndex += run
          run = 0
          for {
            file  <- File(fileIndex)
            rank  <- Rank(rankIndex)
            piece <- Piece.fromChar(c)
          } placed += (Pos(file, rank) -> piece)
          fileIndex += 1
        }
      }
      placed.toList
    }.toMap
  }

}
