package strategygames.go.opening

import scala.annotation.nowarn

import strategygames.go.format.FEN

object FullOpeningDB {

  def findByFen(@nowarn fen: FEN): Option[FullOpening] = None // TODO: ???

  val SEARCH_MAX_PLIES = 40

  // assumes standard initial FEN and variant
  def search(@nowarn moveStrs: Iterable[String]): Option[FullOpening.AtPly] = None // TODO: ???

  def searchInFens(@nowarn fens: Vector[FEN]): Option[FullOpening] = None // TODO: ???
}
