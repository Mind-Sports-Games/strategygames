package strategygames.yonkoma.format

import strategygames.Player
import strategygames.yonkoma.{ Board, File, Piece, PieceMap, Pos, Rank }

// "<board> <player> <full move number>", the board written rank by rank from the top,
// as in chess, with a run of unoccupied squares written as its length (which may have
// two digits on these boards), 'C' for a P1 (white) checker and 'c' for a P2 (black) one
final case class FEN(value: String) extends AnyVal {

  override def toString = value

  private def parts: Array[String] = value.split(' ')

  private def field(index: Int): Option[String] = parts.lift(index)

  private def intField(index: Int): Option[Int] = field(index).flatMap(_.toIntOption)

  def boardPart: String = field(0).getOrElse("")

  // the ranks from the top, an empty string for an empty rank; the board is square, so
  // their number is the board size
  def ranks: Array[String] = boardPart.split("/", -1)

  // the squares a rank string describes, or None when it holds anything but runs of
  // unoccupied squares (written without a leading zero) and checkers
  private def rankWidth(rankStr: String): Option[Int] = {
    var width = 0
    var run   = 0
    var ok    = rankStr.nonEmpty
    rankStr.foreach { c =>
      if (c.isDigit) {
        if (run == 0 && c == '0') ok = false
        run = run * 10 + c.asDigit
      } else if (Piece.fromChar(c).isDefined) {
        width += run + 1
        run = 0
      } else ok = false
    }
    if (ok) Some(width + run) else None
  }

  // the board size, when the rank count is one and every rank describes that many squares
  def boardSize: Option[Board.BoardSize] =
    Board.BoardSize.all
      .find(_.height == ranks.size)
      .filter(size => ranks.forall(rankWidth(_).contains(size.width)))

  // the player to move, 'w' or 'b'; None for anything else
  def player: Option[Player] =
    field(1)
      .filter(_.length == 1)
      .flatMap(_.headOption)
      .filter(c => c == 'w' || c == 'b')
      .flatMap(Player.apply)

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
    val rankStrs = ranks
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
