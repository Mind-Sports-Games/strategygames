package strategygames.backgammon.opening

import strategygames.backgammon.format.FEN
import strategygames.ActionStrs

import scala.annotation.nowarn

object FullOpeningDB {

  private val SEARCH_MAX_TURNS = 40

  def findByFen(fen: FEN): Option[FullOpening] = None // TODO: ???

  // assumes standard initial FEN and variant
  @nowarn def search(actionStrs: ActionStrs): Option[FullOpening.AtPly] = None // TODO: ???

  @nowarn def searchInFens(fens: Vector[FEN]): Option[FullOpening] = None // TODO: ???
}
