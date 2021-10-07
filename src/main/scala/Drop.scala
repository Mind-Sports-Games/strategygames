package strategygames

import cats.syntax.option.none

import strategygames.format.Uci

abstract sealed class Drop(
    piece: Piece,
    pos: Pos,
    situationBefore: Situation,
    after: Board,
    metrics: MoveMetrics = MoveMetrics()
) {

  def before = situationBefore.board

  def situationAfter: Situation

  def finalizeAfter: Board

  def withHistory(h: History): Drop

  def afterWithLastMove: Board

  def color = piece.color

  def withAfter(newBoard: Board): Drop

  def withMetrics(m: MoveMetrics): Drop

  def toUci: Uci.Drop

  override def toString: String

  // TODO: Yup, still not type safe. :D
  def toChess: chess.Drop
  def toFairySF: fairysf.Drop
}

object Drop {

  final case class Chess(d: chess.Drop) extends Drop(
    Piece.Chess(d.piece),
    Pos.Chess(d.pos),
    Situation.Chess(d.situationBefore),
    Board.Chess(d.after),
    d.metrics
  ){

    def situationAfter: Situation = Situation.Chess(d.situationAfter)
    def finalizeAfter: Board = d.finalizeAfter

    def withHistory(h: History): Drop = h match {
      case History.Chess(h) => Chess(d.withHistory(h))
      case _ => sys.error("Not passed Chess objects")
    }

    def afterWithLastMove: Board = Board.Chess(d.afterWithLastMove)

    def withAfter(newBoard: Board): Drop = newBoard match {
      case Board.Chess(newBoard) => Chess(d.withAfter(newBoard))
      case _ => sys.error("Not passed Chess objects")
    }

    def withMetrics(mm: MoveMetrics): Drop = Drop.Chess(d.withMetrics(mm))

    def toUci: Uci.Drop = Uci.ChessDrop(d.toUci)

    val unwrap = d
    def toChess = d
    def toFairySF = sys.error("Can't make a fairysf drop from a chess drop")

  }

  final case class FairySF(d: fairysf.Drop) extends Drop(
    Piece.FairySF(d.piece),
    Pos.FairySF(d.pos),
    Situation.FairySF(d.situationBefore),
    Board.FairySF(d.after),
    d.metrics
  ){

    def situationAfter: Situation = Situation.FairySF(d.situationAfter)
    def finalizeAfter: Board = d.finalizeAfter

    def withHistory(h: History): Drop = h match {
      case History.FairySF(h) => FairySF(d.withHistory(h))
      case _ => sys.error("Not passed FairySF objects")
    }

    def afterWithLastMove: Board = Board.FairySF(d.afterWithLastMove)

    def withAfter(newBoard: Board): Drop = newBoard match {
      case Board.FairySF(newBoard) => FairySF(d.withAfter(newBoard))
      case _ => sys.error("Not passed FairySF objects")
    }

    def withMetrics(mm: MoveMetrics): Drop = Drop.FairySF(d.withMetrics(mm))

    def toUci: Uci.Drop = Uci.FairySFDrop(d.toUci)

    val unwrap = d
    def toChess = sys.error("Can't make a chess drop from a fairysf drop")
    def toFairySF = d

  }

  def wrap(d: chess.Drop): Drop = Drop.Chess(d)
  def wrap(d: fairysf.Drop): Drop = Drop.FairySF(d)

  //def wrap(d: chess.MoveOrDrop): MoveOrDrop = d match {
  //  case Left(move) => Left(Move.Chess(move))
  //  case Right(drop) => Right(Drop.Chess(drop))
  //}

  //def wrap(d: fairysf.MoveOrDrop): MoveOrDrop = d match {
  //  case Left(move) => Left(Move.FairySF(move))
  //  case Right(drop) => Right(Drop.FairySF(drop))
  //}

  def toChess(moveOrDrop: MoveOrDrop): chess.MoveOrDrop = moveOrDrop.left.map(_.toChess).right.map(_.toChess)
  //probably not type safe
  def toDraughts(moveOrDrop: MoveOrDrop): draughts.Move = moveOrDrop.left.map(_.toDraughts).left.get
  def toFairySF(moveOrDrop: MoveOrDrop): fairysf.MoveOrDrop = moveOrDrop.left.map(_.toFairySF).right.map(_.toFairySF)
}
