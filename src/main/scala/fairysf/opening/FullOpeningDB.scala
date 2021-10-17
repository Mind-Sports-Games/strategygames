package strategygames.fairysf.opening

import cats.syntax.option._

import strategygames.fairysf.format.FEN

object FullOpeningDB {

  def findByFen(fen: FEN): Option[FullOpening] = ???

  val SEARCH_MAX_PLIES = 40

  // assumes standard initial FEN and variant
  def search(moveStrs: Iterable[String]): Option[FullOpening.AtPly] = ???

  def searchInFens(fens: Vector[FEN]): Option[FullOpening] = ???
}
