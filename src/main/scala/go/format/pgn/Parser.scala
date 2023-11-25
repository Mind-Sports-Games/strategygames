package strategygames.go
package format.pgn

import scala.annotation.nowarn

import strategygames.go.variant.Variant
import strategygames.go.format.Uci

import strategygames.format.pgn.{ Glyphs, ParsedPgn, Sans }

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

  def sans(str: String, @nowarn variant: Variant): Validated[String, Sans] =
    Validated.invalid(s"Not implemented moves: ${str}") // TODO: ???
  def sans(strMoves: Iterable[String], @nowarn variant: Variant): Validated[String, Sans] =
    Validated.invalid(s"Not implemented iterable moves: ${strMoves}") // TODO: ???

  // StrMove use to exist here in a copied 'TODO ???' version
  // but it was never used because everything looked at chess.format.pgn.Parser directly
}
