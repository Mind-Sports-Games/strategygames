package strategygames

import cats.syntax.option.none

import strategygames.format.Uci

sealed abstract class Drop(
    val piece: Piece,
    val pos: Pos,
    val situationBefore: Situation,
    val after: Board,
    val metrics: MoveMetrics = MoveMetrics()
) {

  def situationAfter: Situation

  def finalizeAfter: Board

  def player = piece.player

  def toUci: Uci.Drop

  override def toString: String

  // TODO: Yup, still not type safe. :D
  def toChess: chess.Drop
  def toFairySF: fairysf.Drop
}

object Drop {

  final case class Chess(d: chess.Drop)
      extends Drop(
        Piece.Chess(d.piece),
        Pos.Chess(d.pos),
        Situation.Chess(d.situationBefore),
        Board.Chess(d.after),
        d.metrics
      ) {

    def situationAfter: Situation = Situation.Chess(d.situationAfter)
    def finalizeAfter: Board      = d.finalizeAfter

    def toUci: Uci.Drop = Uci.ChessDrop(d.toUci)

    val unwrap    = d
    def toChess   = d
    def toFairySF = sys.error("Can't make a fairysf drop from a chess drop")

  }

  final case class FairySF(d: fairysf.Drop)
      extends Drop(
        Piece.FairySF(d.piece),
        Pos.FairySF(d.pos),
        Situation.FairySF(d.situationBefore),
        Board.FairySF(d.after),
        d.metrics
      ) {

    def situationAfter: Situation = Situation.FairySF(d.situationAfter)
    def finalizeAfter: Board      = d.finalizeAfter

    def toUci: Uci.Drop = Uci.FairySFDrop(d.toUci)

    val unwrap    = d
    def toChess   = sys.error("Can't make a chess drop from a fairysf drop")
    def toFairySF = d

  }

  def wrap(d: chess.Drop): Drop   = Drop.Chess(d)
  def wrap(d: fairysf.Drop): Drop = Drop.FairySF(d)

  def toChess(moveOrDrop: MoveOrDrop): chess.MoveOrDrop     = moveOrDrop.left.map(_.toChess).right.map(_.toChess)
  // probably not type safe
  def toDraughts(moveOrDrop: MoveOrDrop): draughts.Move     = moveOrDrop.left.map(_.toDraughts).left.get
  def toFairySF(moveOrDrop: MoveOrDrop): fairysf.MoveOrDrop =
    moveOrDrop.left.map(_.toFairySF).right.map(_.toFairySF)
  def toSamurai(moveOrDrop: MoveOrDrop): samurai.Move       = moveOrDrop.left.map(_.toSamurai).left.get
  def toTogyzkumalak(moveOrDrop: MoveOrDrop): togyzkumalak.Move =
    moveOrDrop.left.map(_.toTogyzkumalak).left.get

}
