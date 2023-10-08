package strategygames.go
package format.pgn

import scala.annotation.nowarn

import strategygames.go.variant.Variant
import strategygames.go.format.Uci

import strategygames.format.pgn.{ Glyphs, InitialPosition, ParsedPgn, Sans, Tag }

import scala.util.parsing.combinator._
import cats.data.Validated

// http://www.saremba.de/chessgml/standards/pgn/pgn-complete.htm
object Parser {

  def pgnMovesToUciMoves(pgnMoves: Iterable[String]): List[String] =
    pgnMoves.toList.map(
      _ match {
        case Uci.Drop.dropR(role, dest)          => s"${role}@${dest}"
        case Uci.Pass.passR()                    => "pass"
        case Uci.SelectSquares.selectSquaresR(s) => s"ss:${s}"
        case s: String                           => s
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

  def moves(str: String, @nowarn variant: Variant): Validated[String, Sans] =
    Validated.invalid(s"Not implemented moves: ${str}") // TODO: ???
  def moves(strMoves: Iterable[String], @nowarn variant: Variant): Validated[String, Sans] =
    Validated.invalid(s"Not implemented iterable moves: ${strMoves}") // TODO: ???

  trait Logging { self: Parsers =>
    protected val loggingEnabled                                 = false
    protected def as[T](msg: String)(p: => Parser[T]): Parser[T] =
      if (loggingEnabled) log(p)(msg) else p
  }

  object MovesParser extends RegexParsers with Logging {

    override val whiteSpace = """(\s|\t|\r?\n)+""".r

    def apply(@nowarn pgn: String): Validated[String, (InitialPosition, List[StrMove], Option[Tag])] =
      Validated.invalid("Not implemented MovesParser") // TODO: ???

    // def strMoves: Parser[(InitialPosition, List[StrMove], Option[String])] = //TODO: ???

    val moveRegex =
      """(?:(?:0\-0(?:\-0|)[\+\#]?)|[PQKRBNOoa-h@][QKRBNa-h1-8xOo\-=\+\#\@]{1,6})[\?!□]{0,2}""".r

  }

}
