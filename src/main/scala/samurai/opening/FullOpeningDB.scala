package strategygames.samurai.opening

import cats.syntax.option._

import strategygames.samurai.format.FEN

object FullOpeningDB {

  def findByFen(fen: FEN): Option[FullOpening] = None // TODO: ???

  val SEARCH_MAX_PLIES = 40

  // assumes standard initial FEN and variant
  def search(moveStrs: Iterable[String]): Option[FullOpening.AtPly] = None // TODO: ???

  def searchInFens(fens: Vector[FEN]): Option[FullOpening] = None // TODO: ???
}
