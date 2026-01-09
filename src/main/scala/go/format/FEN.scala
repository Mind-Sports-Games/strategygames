package strategygames.go.format

import strategygames.Player
import strategygames.go.variant.Variant

final case class FEN(value: String) extends AnyVal {

  override def toString = value

  def board: String = removePockets(value.takeWhile(_ != ' '))

  // Use inverseApply because black goes first in Go
  def player: Option[Player] =
    value.split(' ').lift(FEN.playerIndex).flatMap(_.headOption).flatMap(Player.inverseApply)

  def invertPlayer: Option[FEN] =
    // This is safe because player function ensures there is an element at playerIndex when doing split(' ')
    // Dont flip player because of inverted colours
    player.map { p => FEN(value.split(' ').updated(FEN.playerIndex, p.letter.toString).mkString(" ")) }

  def player1Score: Int = intFromFen(3).getOrElse(0)

  def player2Score: Int = intFromFen(4).getOrElse(0)

  def player1Captures: Int = intFromFen(5).getOrElse(0)

  def player2Captures: Int = intFromFen(6).getOrElse(0)

  def komi: Double = intFromFen(7).getOrElse(0) / 10.0

  // need to account for old style of fen without pass info due to studies and setup info
  def oldFenSytle: Boolean = value.split(' ').length == 9

  // Consecutive Pass count. Capped at 2, 3 is reserved for end game (after "ss:")
  def fenPassCount: Int = if (oldFenSytle) 0 else intFromFen(8).getOrElse(0)

  def fullMove: Option[Int] = if (oldFenSytle) intFromFen(8) else intFromFen(9)

  def ply: Option[Int] =
    fullMove map { fm =>
      fm * 2 - (if (player.exists(_.p1)) 2 else 1)
    }

  def handicap: Option[Int] = if (fullMove == Some(1)) Some(board.count(_ == 'S')) else None

  def engineFen: String =
    s"""${board
        .replace("S", "X")
        .replace("s", "O")
        .replace("19", "199") // goengine cant handle double digits
        .replace("18", "189")
        .replace("17", "179")
        .replace("16", "169")
        .replace("15", "159")
        .replace("14", "149")
        .replace("13", "139")
        .replace("12", "129")
        .replace("11", "119")
        .replace("10", "109")} ${value.split(' ').drop(1).take(2).mkString(" ")}"""

  private def removePockets(fen: String): String = {
    val start = fen.indexOf("[", 0)
    val end   = fen.indexOf("]", start)
    if (start > 0 && end > 0)
      fen.substring(0, start) + fen.substring(end + 1, fen.length)
    else fen
  }

  def gameSize: Int = value.split(' ').lift(0).map(_.split('/').length).getOrElse(0)

  def variant: Variant = gameSize match {
    case 9  => strategygames.go.variant.Go9x9
    case 13 => strategygames.go.variant.Go13x13
    case 19 => strategygames.go.variant.Go19x19
    case _  => sys.error(s"not given correct gameSize for go ${gameSize}")
  }

  private def intFromFen(index: Int): Option[Int] =
    value.split(' ').lift(index).flatMap(_.toIntOption)

  def initial = value == Forsyth.initial.value
}

object FEN {

  def clean(source: String): FEN = FEN(source.replace("_", " ").trim)

  def playerIndex: Int = 1

}
