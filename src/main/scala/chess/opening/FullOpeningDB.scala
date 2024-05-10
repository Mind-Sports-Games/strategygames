package strategygames.chess.opening

import cats.syntax.option._

import strategygames.chess.format.FEN
import strategygames.ActionStrs

object FullOpeningDB {

  private val SEARCH_MAX_TURNS = 40

  private lazy val byFen: collection.Map[String, FullOpening] = {
    FullOpeningPartA.db ++ FullOpeningPartB.db ++ FullOpeningPartC.db ++ FullOpeningPartD.db ++ FullOpeningPartE.db
  }.view.map { o =>
    o.fen -> o
  }.toMap

  def findByFen(fen: FEN): Option[FullOpening] =
    fen.value.split(' ').take(4) match {
      case Array(boardPocket, turn, castle, ep) =>
        val board =
          if (boardPocket.contains('[')) boardPocket.takeWhile('[' !=)
          else if (boardPocket.count('/' ==) == 8) boardPocket.split('/').take(8).mkString("/")
          else boardPocket
        byFen get List(board, turn, castle, ep).mkString(" ")
      case _                                    => None
    }

  // retain original logic: pgnMoves.take(SEARCH_MAX_PLIES).takeWhile(san => !san.contains('@'))
  private def searchActionStrs(actionStrs: ActionStrs): ActionStrs = {
    val a    = actionStrs.toList
      .take(SEARCH_MAX_TURNS)
      .map(t => (t.takeWhile(san => !san.contains('@')), t.filter(san => san.contains('@')).size > 0))
      .takeWhile(!_._2)
      .map(_._1)
    val last =
      if (a.size < actionStrs.size) {
        val lastActionStrs = actionStrs.toList(a.size).takeWhile(san => !san.contains('@'))
        if (lastActionStrs == Vector()) List() else List(lastActionStrs)
      } else List()
    a ++ last
  }

  // assumes standard initial FEN and variant
  def search(actionStrs: ActionStrs): Option[FullOpening.AtPly] =
    strategygames.chess.Replay
      .situations(
        searchActionStrs(actionStrs),
        None,
        strategygames.chess.variant.Standard
      )
      .toOption
      .flatMap {
        _.zipWithIndex.drop(1).foldRight(none[FullOpening.AtPly]) {
          case ((situation, ply), None) =>
            val fen = strategygames.chess.format.Forsyth.exportStandardPositionTurnCastlingEp(situation)
            byFen get fen map (_ atPly ply)
          case (_, found)               => found
        }
      }

  def searchInFens(fens: Vector[FEN]): Option[FullOpening] =
    fens.foldRight(none[FullOpening]) {
      case (fen, None) => findByFen(fen)
      case (_, found)  => found
    }
}
