package strategygames.fairysf
package format.pgn

import scala.annotation.nowarn

import strategygames.fairysf.variant.Variant
import strategygames.fairysf.format.Uci

import strategygames.format.pgn.{ Glyphs, InitialPosition, ParsedPgn, Sans, Tag }

import scala.util.parsing.combinator._
import cats.data.Validated

// http://www.saremba.de/chessgml/standards/pgn/pgn-complete.htm
object Parser {

  def pgnMovesToUciMoves(pgnMoves: Iterable[String], doubleMoveFormat: Boolean = false): List[String] =
    if (doubleMoveFormat)
      pgnMoves.toList
        .sliding(2, 2)
        .toList
        .map(
          _ match {
            case List(Uci.Move.moveP(orig, dest, _), Uci.Drop.dropR(_, dest2)) =>
              Some(s"${orig}${dest},${dest}${dest2}")
            case _                                                             => None
          }
        )
        .flatten
    else
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
