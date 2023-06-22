package strategygames

import cats.syntax.option.none

import strategygames.format.Uci

sealed abstract class Pass(
    val situationBefore: Situation,
    val after: Board,
    val metrics: MoveMetrics = MoveMetrics()
) {

  def situationAfter: Situation

  def finalizeAfter: Board

  // def player = piece.player

  def toUci: Uci.Pass

  override def toString: String

  // TODO: Yup, still not type safe. :D
  def toGo: go.Pass
}

object Pass {

  final case class Go(p: go.Pass)
      extends Pass(
        Situation.Go(p.situationBefore),
        Board.Go(p.after),
        p.metrics
      ) {

    def situationAfter: Situation = Situation.Go(p.situationAfter)
    def finalizeAfter: Board      = p.finalizeAfter

    def toUci: Uci.Pass = Uci.GoPass(p.toUci)

    val unwrap = p
    def toGo   = p

  }

  def wrap(p: go.Pass): Pass = Pass.Go(p)

  // todo not sure what to set here yet...
  def toChess(action: Pass)        = None
  // probably not type safe
  def toDraughts(action: Pass)     = None
  def toFairySF(action: Pass)      = None
  def toSamurai(action: Pass)      = None
  def toTogyzkumalak(action: Pass) = None
  def toGo(action: Pass): go.Pass  = action.toGo

}
