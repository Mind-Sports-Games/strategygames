package strategygames.fairysf
package format.pgn

import strategygames.fairysf.variant.Variant
import strategygames.fairysf.format.Uci

import strategygames.format.pgn.{ Glyphs, InitialPosition, ParsedPgn, Sans, Tag }

import scala.util.parsing.combinator._
import cats.data.Validated

// http://www.saremba.de/chessgml/standards/pgn/pgn-complete.htm
object Parser {

  // This doesnt support multiaction properly, but it correctly handles a game like Amazons
  // which is condensed into a single action per turn. if a different type of multiaction
  // game became available from Fairy we might need to update this
  def pliesToFairyUciMoves(plies: Seq[String], doubleMoveFormat: Boolean = false): List[String] =
    if (doubleMoveFormat)
      // not sure this branch is ever used
      plies.toList
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
      plies.toList.map(
        _ match {
          case Uci.Move.moveP(orig, dest, promotion) =>
            promotion match {
              case "" => s"${orig}${dest}"
              case _  => s"${orig}${dest}+"
            }
          case s: String                             => s
        }
      )

  def full(pgn: String): Validated[String, ParsedPgn] =
    Validated.invalid(s"Not implemented full: ${pgn}") // TODO: ???
  def sans(str: String, variant: Variant): Validated[String, Sans] =
    Validated.invalid(s"Not implemented moves: ${str}") // TODO: ???
  def sans(strMoves: Iterable[String], variant: Variant): Validated[String, Sans] =
    Validated.invalid(s"Not implemented iterable moves: ${strMoves}") // TODO: ???

  // StrMove use to exist here in a copied 'TODO ???' version
  // but it was never used because everything looked at chess.format.pgn.Parser directly

}
