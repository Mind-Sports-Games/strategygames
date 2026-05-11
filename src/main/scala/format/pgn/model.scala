package strategygames
package format
package pgn

import cats.implicits._
import scalalib.extensions.*

case class Pgn(
    tags: Tags,
    fullTurns: List[FullTurn],
    initial: Initial = Initial.empty
) {

  def updateFullTurn(fullTurnNumber: Int, f: FullTurn => FullTurn) = {
    val index = fullTurnNumber - 1
    (fullTurns lift index).fold(this) { fullTurn =>
      copy(fullTurns = fullTurns.updated(index, f(fullTurn)))
    }
  }

  def updateTurnCount(turnCount: Int, f: Turn => Turn) = {
    val fullTurnNumber = (turnCount + 1) / 2
    val player         = Player.fromTurnCount(turnCount - 1)
    updateFullTurn(fullTurnNumber, _.update(player, f))
  }

  def updateLastTurnCount(f: Turn => Turn) = updateTurnCount(nbTurns, f)

  def nbTurns = fullTurns.foldLeft(0)(_ + _.count)

  def turns =
    fullTurns.flatMap { fullTurn =>
      List(fullTurn.p1, fullTurn.p2).flatten
    }

  def withEvent(title: String) =
    copy(
      tags = tags + Tag(_.Event, title)
    )

  def render: String = {
    val initStr     =
      if (initial.comments.nonEmpty) initial.comments.mkString("{ ", " } { ", " }\n")
      else ""
    val fullTurnStr = fullTurns mkString " "
    val resultStr   = tags(_.Result) | ""
    val endStr      =
      if (fullTurnStr.nonEmpty) s" $resultStr"
      else resultStr
    s"$tags\n\n$initStr$fullTurnStr$endStr"
  }.trim

  override def toString = render
}

case class Initial(comments: List[String] = Nil)

object Initial {
  val empty = Initial(Nil)
}

case class FullTurn(
    fullTurnNumber: Int,
    p1: Option[Turn],
    p2: Option[Turn]
) {

  def update(player: Player, f: Turn => Turn) =
    player.fold(
      copy(p1 = p1 map f),
      copy(p2 = p2 map f)
    )

  def updateLast(f: Turn => Turn) = {
    p2.map(m => copy(p2 = f(m).some)) orElse
      p1.map(m => copy(p1 = f(m).some))
  } | this

  def isEmpty = p1.isEmpty && p2.isEmpty

  def turnOf(player: Player) = fullTurnNumber * 2 - player.fold(1, 0)

  def count = List(p1, p2) count (_.isDefined)

  override def toString = {
    val text = (p1, p2) match {
      case (Some(w), Some(b)) if w.isLong => s" $w $fullTurnNumber... $b"
      case (Some(w), Some(b))             => s" $w $b"
      case (Some(w), None)                => s" $w"
      case (None, Some(b))                => s".. $b"
      case _                              => ""
    }
    s"$fullTurnNumber.$text"
  }
}

object FullTurn {

  def fromTurns(turns: List[Turn], turnCount: Int): List[FullTurn] = {
    turns.foldLeft((List[FullTurn](), turnCount)) {
      case ((fullTurns, t), turn) if t % 2 == 1 =>
        (FullTurn((t + 1) / 2, turn.some, none) :: fullTurns) -> (t + 1)
      case ((Nil, t), turn) =>
        (FullTurn((t + 1) / 2, none, turn.some) :: Nil) -> (t + 1)
      case ((ft :: tt, t), turn) =>
        (ft.copy(p2 = turn.some) :: tt) -> (t + 1)
    }
  }._1.reverse
}

//san should contain a combination of actions for a single turn
case class Turn(
    san: String,
    comments: List[String] = Nil,
    glyphs: Glyphs = Glyphs.empty,
    opening: Option[String] = None,
    result: Option[String] = None,
    variations: List[List[FullTurn]] = Nil,
    // time left for the user who made the turn, after he made it
    secondsLeft: Option[Int] = None
) {

  def isLong = comments.nonEmpty || variations.nonEmpty || secondsLeft.isDefined

  private def clockString: Option[String] =
    secondsLeft.map(seconds => "[%clk " + Turn.formatPgnSeconds(seconds) + "]")

  override def toString = {
    val glyphStr        = glyphs.toList.map {
      case glyph if glyph.id <= 6 => glyph.symbol
      case glyph                  => s" $$${glyph.id}"
    }.mkString
    val commentsOrTime  =
      if (comments.nonEmpty || secondsLeft.isDefined || opening.isDefined || result.isDefined)
        List(clockString, opening, result).flatten
          .:::(comments map Turn.noDoubleLineBreak)
          .map { text =>
            s" { $text }"
          }
          .mkString
      else ""
    val variationString =
      if (variations.isEmpty) ""
      else variations.map(_.mkString(" (", " ", ")")).mkString(" ")
    s"$san$glyphStr$commentsOrTime$variationString"
  }
}

object Turn {

  private val noDoubleLineBreakRegex = "(\r?\n){2,}".r

  private def noDoubleLineBreak(txt: String) =
    noDoubleLineBreakRegex.replaceAllIn(txt, "\n")

  private def formatPgnSeconds(t: Int) =
    periodFormatter.print(
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
