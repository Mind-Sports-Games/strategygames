package strategygames.abalone.opening

import cats.syntax.option._
import strategygames.ActionStrs
import strategygames.abalone._
import strategygames.abalone.format.Forsyth

final class Ecopening(
                       val eco: Ecopening.ECO,
                       val variantGrouping: String,
                       val family: Ecopening.FamilyName,
                       val name: String,
                       val moves: String,
                       val fen: Ecopening.FEN,
                       val lastMoveUci: String
                     ) extends Ordered[Ecopening] {
  lazy val moveList = moves.split(' ').toList

  def firstMove = moveList.headOption

  lazy val size = moveList.size

  lazy val formattedMoves: String =
    moveList
      .grouped(2)
      .zipWithIndex
      .map {
        case (List(w, b), i) => s"${i + 1}. $w $b"
        case (List(w), i)    => s"${i + 1}. $w"
        case _               => ""
      }
      .mkString(" ")

  def ecoName = s"$eco $name"

  override def compare(other: Ecopening) = eco compare other.eco

  override def toString = s"$ecoName ($moves)"
}

object Ecopening {
  type FamilyName = String
  type ECO        = String
  type FEN        = String

  case class Family(name: FamilyName, ecos: List[FEN])

  def makeFamilies(ops: Iterable[Ecopening]): Map[FamilyName, Family] =
    ops.foldLeft(Map.empty[FamilyName, Family]) { case (fams, op) =>
      fams + (op.family -> fams.get(op.family).fold(Family(op.family, List(op.eco))) { existing =>
        existing.copy(ecos = op.eco :: existing.ecos)
      })
    }

  def fromGame(actionStrs: ActionStrs): Option[Ecopening] = Replay
    .boards(
      actionStrs = actionStrs take EcopeningDB.MAX_TURNS,
      initialFen = None,
      variant = variant.Variant.default
    )
    .toOption flatMap matchChronoBoards

  private def matchChronoBoards(boards: List[Board]): Option[Ecopening] =
    boards.reverse.foldLeft(none[Ecopening]) { case (acc, board) =>
      acc orElse {
        EcopeningDB.allByFen get Forsyth.exportBoard(board)
      }
    }
}