package strategygames.dameo
package format
package pdn

import strategygames.Player
import strategygames.format.pgn.{ Glyphs, Tag, Tags }

import scala._
import cats.implicits._

case class Pdn(
    tags: Tags,
    turns: List[Turn],
    initial: Initial = Initial.empty
) {

  def updateTurn(fullMove: Int, f: Turn => Turn) = {
    val index = fullMove - 1
    (turns lift index).fold(this) { turn =>
      copy(turns = turns.updated(index, f(turn)))
    }
  }
  def updatePly(ply: Int, f: Move => Move)       = {
    val fullMove = (ply + 1) / 2
    val player   = Player(ply % 2 == 1)
    updateTurn(fullMove, _.update(player, f))
  }
  def updateLastPly(f: Move => Move)             = updatePly(nbPlies, f)

  def nbPlies = turns.foldLeft(0)(_ + _.count)

  def moves = turns.flatMap { t =>
    List(t.p1, t.p2).flatten
  }

  def withEvent(title: String) = copy(
    tags = tags + Tag(_.Event, title)
  )

  def render: String = {
    val initStr =
      if (initial.comments.nonEmpty) initial.comments.mkString("{ ", " } { ", " }\n")
      else ""
    val turnStr = turns mkString " "
    val endStr  = tags(_.Result) | ""
    s"$tags\n\n$initStr$turnStr $endStr"
  }.trim + "\n"

  override def toString = render
}

case class Initial(comments: List[String] = Nil)

object Initial {
  val empty = Initial(Nil)
}

case class Turn(
    number: Int,
    p1: Option[Move],
    p2: Option[Move]
) {

  def update(player: Player, f: Move => Move) = player.fold(
    copy(p1 = p1 map f),
    copy(p2 = p2 map f)
  )

  def updateLast(f: Move => Move) = {
    p2.map(m => copy(p2 = f(m).some)) orElse
      p1.map(m => copy(p1 = f(m).some))
  } | this

  def isEmpty = p1.isEmpty && p2.isEmpty

  def plyOf(player: Player) = number * 2 - player.fold(1, 0)

  def count = List(p1, p2) count (_.isDefined)

  override def toString = {
    val text = (p1, p2) match {
      case (Some(w), Some(b)) if w.isLong => s" $w $number... $b"
      case (Some(w), Some(b))             => s" $w $b"
      case (Some(w), None)                => s" $w"
      case (None, Some(b))                => s".. $b"
      case _                              => ""
    }
    s"$number.$text"
  }
}

object Turn {

  def fromMoves(moves: List[Move], ply: Int): List[Turn] = {
    moves.foldLeft((List[Turn](), ply)) {
      case ((turns, p), move) if p % 2 == 1 =>
        (Turn((p + 1) / 2, move.some, none) :: turns) -> (p + 1)
      case ((Nil, p), move) =>
        (Turn((p + 1) / 2, none, move.some) :: Nil) -> (p + 1)
      case ((t :: tt, p), move) =>
        (t.copy(p2 = move.some) :: tt) -> (p + 1)
    }
  }._1.reverse
}

case class Move(
    san: String,
    // the player who played the move
    turn: Player,
    comments: List[String] = Nil,
    glyphs: Glyphs = Glyphs.empty,
    opening: Option[String] = None,
    result: Option[String] = None,
    variations: List[List[Turn]] = Nil,
    // time left for the p1, p2 player, after the move is made
    secondsLeft: (Option[Int], Option[Int]) = (None, None)
) {

  def isLong = comments.nonEmpty || variations.nonEmpty

  private def canPrintTime = secondsLeft._1.isDefined && (secondsLeft._2.isDefined || turn.p1)

  private def clockString: Option[String] =
    if (canPrintTime)
      s"[%clock ${turn.fold("w", "W")}${Move.formatPdnSeconds(secondsLeft._1.get)} ${turn.fold("B", "b")}${Move
          .formatPdnSeconds(secondsLeft._2.getOrElse(0))}]".some
    else none

  override def toString = {
    val glyphStr        = glyphs.toList.map {
      case glyph if glyph.id <= 6 => glyph.symbol
      case glyph                  => s" $$${glyph.id}"
    }.mkString
    val commentsOrTime  =
      if (comments.nonEmpty || canPrintTime || opening.isDefined || result.isDefined)
        List(clockString, opening, result).flatten
          .:::(comments map Move.noDoubleLineBreak)
          .map { text =>
            s" {$text}"
          }
          .mkString
      else ""
    val variationString =
      if (variations.isEmpty) ""
      else variations.map(_.mkString(" (", " ", ")")).mkString(" ")
    s"$san$glyphStr$commentsOrTime$variationString"
  }
}

object Move {

  private val noDoubleLineBreakRegex = "(\r?\n){2,}".r

  private def noDoubleLineBreak(txt: String) =
    noDoubleLineBreakRegex.replaceAllIn(txt, "\n")

  private def formatPdnSeconds(t: Int) = periodFormatter.print(
    org.joda.time.Duration.standardSeconds(t).toPeriod
  )

  private[this] val periodFormatter = new org.joda.time.format.PeriodFormatterBuilder().printZeroAlways
    .minimumPrintedDigits(1)
    .appendHours
    .appendSeparator(":")
    .minimumPrintedDigits(2)
    .appendMinutes
    .appendSeparator(":")
    .appendSeconds
    .toFormatter

}
