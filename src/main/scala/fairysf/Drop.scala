package strategygames.fairysf
import strategygames.MoveMetrics

import strategygames.fairysf.format.Uci

import cats.syntax.option._

case class Drop(
    piece: Piece,
    pos: Pos,
    situationBefore: Situation,
    after: Board,
    metrics: MoveMetrics = MoveMetrics()
) {

  private def before = situationBefore.board

  private def nextMoveIsPass: Boolean = before.variant.passUci match {
    case Some(passUci) => after.apiPosition.legalMoves.sameElements(Array(passUci))
    case None          => false
  }

  def situationAfter =
    Situation(finalizeAfter, if (nextMoveIsPass) piece.player else !piece.player)

  def finalizeAfter: Board = before.variant.passUci match {
    case Some(passUci) if nextMoveIsPass =>
      val newPosition = after.apiPosition.makeMoves(List(passUci))
      after.copy(
        pieces = newPosition.pieceMap, //will also == after.pieces
        uciMoves = after.uciMoves :+ passUci,
        pocketData = newPosition.pocketData,
        position = newPosition.some
      )
    case _ => after
  }

  def player = piece.player

  def withMetrics(m: MoveMetrics) = copy(metrics = m)

  def toUci = Uci.Drop(piece.role, pos)

  override def toString = toUci.uci

}
