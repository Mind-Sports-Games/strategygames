package draughts
package format.pdn

import cats.data.Validated
import cats.data.Validated.{ invalid, valid }
import cats.syntax.option._

case class ParsedPdn(
    initialPosition: InitialPosition,
    tags: Tags,
    sans: Sans
)

case class Sans(value: List[San]) extends AnyVal

object Sans {
  val empty = Sans(Nil)
}

// Standard Algebraic Notation
sealed trait San {

  def apply(
      situation: Situation,
      iteratedCapts: Boolean = false,
      forbiddenUci: Option[List[String]] = None
  ): Validated[String, draughts.Move]

  def metas: Metas

  def withMetas(m: Metas): San

  def withSuffixes(s: Suffixes): San = withMetas(metas withSuffixes s)

  def withComments(s: List[String]): San = withMetas(metas withComments s)

  def withVariations(s: List[Sans]): San = withMetas(metas withVariations s)

  def mergeGlyphs(glyphs: Glyphs): San = withMetas(
    metas.withGlyphs(metas.glyphs merge glyphs)
  )
}

case class Std(
    fields: List[Pos],
    capture: Boolean,
    metas: Metas = Metas.empty
) extends San {

  def apply(situation: Situation, iteratedCapts: Boolean = false, forbiddenUci: Option[List[String]] = None) =
    move(situation, iteratedCapts, forbiddenUci)

  def move(
      situation: Situation,
      iteratedCapts: Boolean = false,
      forbiddenUci: Option[List[String]] = None,
      captures: Option[List[Pos]] = None
  ): Validated[String, draughts.Move] = {
    val src         = fields.head
    val dest        = fields.last
    val capturePath = if (capture) fields.tail.reverse else Nil
    situation.board.pieces.foldLeft(none[draughts.Move]) {
      case (None, (pos, piece)) if piece.color == situation.color && pos == src =>
        val a = Actor(piece, situation.board.posAt(pos), situation.board)
        // TODO: technically we should check situation.hasCaptures instead of actor
        val validMoves = if (a.captures.nonEmpty) a.captures else a.noncaptures
        validMoves.find { m =>
          m.dest == dest && (!iteratedCapts || m.situationAfter.ghosts == 0)
        } match {
          case None if capture && iteratedCapts =>
            a.capturesFinal.find { m =>
              m.dest == dest &&
              captures.fold(true)(m.capture.contains) &&
              !forbiddenUci.fold(false)(_.contains(m.toUci.uci)) &&
              (capturePath.length <= 1 || m.capture.contains(capturePath))
            }
          case m @ _ => m
        }
      case (m, _) => m
    } match {
      case None       => invalid(s"No move found: $this\n$situation")
      case Some(move) => valid(move)
    }
  }

  def withMetas(m: Metas) = copy(metas = m)
}

case class InitialPosition(
    comments: List[String]
)

case class Metas(
    checkmate: Boolean,
    comments: List[String],
    glyphs: Glyphs,
    variations: List[Sans]
) {

  def withSuffixes(s: Suffixes) = copy(
    checkmate = s.checkmate,
    glyphs = s.glyphs
  )

  def withGlyphs(g: Glyphs) = copy(glyphs = g)

  def withComments(c: List[String]) = copy(comments = c)

  def withVariations(v: List[Sans]) = copy(variations = v)
}

object Metas {
  val empty = Metas(false, Nil, Glyphs.empty, Nil)
}

case class Suffixes(
    checkmate: Boolean,
    glyphs: Glyphs
)
