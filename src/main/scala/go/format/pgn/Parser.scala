package strategygames.go
package format.pgn

import strategygames.go.variant.Variant
import strategygames.go.format.Uci

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

  def full(pgn: String): Validated[String, ParsedPgn] =
    Validated.invalid(s"Not implemented full: ${pgn}") // TODO: ???

  def sans(str: String, variant: Variant): Validated[String, Sans] =
    Validated.invalid(s"Not implemented moves: ${str}") // TODO: ???
  def sans(strMoves: Iterable[String], variant: Variant): Validated[String, Sans] =
    Validated.invalid(s"Not implemented iterable moves: ${strMoves}") // TODO: ???

  // StrMove use to exist here in a copied 'TODO ???' version
  // but it was never used because everything looked at chess.format.pgn.Parser directly
}
