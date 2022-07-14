package strategygames.mancala
package format.pgn

import strategygames.mancala.variant.Variant
import strategygames.mancala.format.Uci

import strategygames.format.pgn.{
  Glyph,
  Glyphs,
  InitialPosition,
  Metas,
  ParsedPgn,
  San,
  Sans,
  Suffixes,
  Tag,
  Tags
}
import strategygames.{ Role => ChessRole }

import scala.util.parsing.combinator._
import cats.data.Validated
import cats.data.Validated.{ invalid, valid }
import cats.implicits._
import scala.util.matching.Regex

// http://www.saremba.de/chessgml/standards/pgn/pgn-complete.htm
object Parser {

  def pgnMovesToUciMoves(pgnMoves: Iterable[String]): List[String] =
    pgnMoves.toList.map(
      _ match {
        case Uci.Move.moveP(orig, dest, promotion) =>
          promotion match {
            case "" => s"${orig}${dest}"
            case _  => s"${orig}${dest}+"
          }
        case s: String                             => s
      }
    )

  case class StrMove(
      san: String,
      glyphs: Glyphs,
      comments: List[String],
      variations: List[List[StrMove]]
  )

  def full(pgn: String): Validated[String, ParsedPgn] =
    Validated.invalid(s"Not implemented full: ${pgn}") // TODO: ???

  def moves(str: String, variant: Variant): Validated[String, Sans] =
    Validated.invalid(s"Not implemented moves: ${str}") // TODO: ???
  def moves(strMoves: Iterable[String], variant: Variant): Validated[String, Sans] =
    Validated.invalid(s"Not implemented iterable moves: ${strMoves}") // TODO: ???
  private def objMoves(strMoves: List[StrMove], variant: Variant): Validated[String, Sans] =
    Validated.invalid("Not implemented objMoves") // TODO: ???

  trait Logging { self: Parsers =>
    protected val loggingEnabled                                 = false
    protected def as[T](msg: String)(p: => Parser[T]): Parser[T] =
      if (loggingEnabled) log(p)(msg) else p
  }

  object MovesParser extends RegexParsers with Logging {

    override val whiteSpace = """(\s|\t|\r?\n)+""".r

    def apply(pgn: String): Validated[String, (InitialPosition, List[StrMove], Option[Tag])] =
      Validated.invalid("Not implemented MovesParser") // TODO: ???

    // def strMoves: Parser[(InitialPosition, List[StrMove], Option[String])] = //TODO: ???

    val moveRegex =
      """(?:(?:0\-0(?:\-0|)[\+\#]?)|[PQKRBNOoa-h@][QKRBNa-h1-8xOo\-=\+\#\@]{1,6})[\?!□]{0,2}""".r

  }

}
