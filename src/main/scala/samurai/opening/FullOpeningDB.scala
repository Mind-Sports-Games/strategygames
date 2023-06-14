package strategygames.samurai.opening

import cats.syntax.option._

import strategygames.samurai.format.FEN
import strategygames.Actions

object FullOpeningDB {

  private val SEARCH_MAX_TURNS = 40

  def findByFen(fen: FEN): Option[FullOpening] = None // TODO: ???

  // assumes standard initial FEN and variant
  def search(actions: Actions): Option[FullOpening.AtPly] = None // TODO: ???

  def searchInFens(fens: Vector[FEN]): Option[FullOpening] = None // TODO: ???
}
