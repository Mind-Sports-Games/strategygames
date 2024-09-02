package strategygames.backgammon
package format.pgn

import strategygames.backgammon.variant.Variant

import strategygames.format.pgn.{ Glyphs, InitialPosition, ParsedPgn, Sans, Tag }

import scala.util.parsing.combinator._
import cats.data.Validated

// http://www.saremba.de/chessgml/standards/pgn/pgn-complete.htm
object Parser {

  case class StrMove(
      san: String,
      glyphs: Glyphs,
      comments: List[String],
      variations: List[List[StrMove]]
  )

  def full(pgn: String): Validated[String, ParsedPgn] =
    Validated.invalid(s"Not implemented full: ${pgn}") // TODO: ???

  def sans(str: String, variant: Variant): Validated[String, Sans] =
    Validated.invalid(s"Not implemented moves: ${str} for variant: ${variant}") // TODO: ???
  def sans(strMoves: Iterable[String], variant: Variant): Validated[String, Sans] =
    Validated.invalid(s"Not implemented iterable moves: ${strMoves} for variant: ${variant}") // TODO: ???
  // private def objMoves(strMoves: List[StrMove], variant: Variant): Validated[String, Sans] =
  //  Validated.invalid("Not implemented objMoves") // TODO: ???

  trait Logging { self: Parsers =>
    protected val loggingEnabled                                 = false
    protected def as[T](msg: String)(p: => Parser[T]): Parser[T] =
      if (loggingEnabled) log(p)(msg) else p
  }

  object MovesParser extends RegexParsers with Logging {

    override val whiteSpace = """(\s|\t|\r?\n)+""".r

    def apply(pgn: String): Validated[String, (InitialPosition, List[StrMove], Option[Tag])] =
      Validated.invalid(s"Not implemented MovesParser. pgn: ${pgn}") // TODO: ???

    // def strMoves: Parser[(InitialPosition, List[StrMove], Option[String])] = //TODO: ???

    val moveRegex =
      """(?:(?:0\-0(?:\-0|)[\+\#]?)|[PQKRBNOoa-h@][QKRBNa-h1-8xOo\-=\+\#\@]{1,6})[\?!□]{0,2}""".r

  }

}
