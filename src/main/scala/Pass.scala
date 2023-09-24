package strategygames

import strategygames.format.Uci

sealed abstract class Pass(
    val situationBefore: Situation,
    val after: Board,
    val metrics: MoveMetrics = MoveMetrics()
) extends Action(situationBefore) {

  def situationAfter: Situation

  def finalizeAfter: Board

  def player: Player = situationBefore.player

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

    def toFairySF      = sys.error("Can't make a fairysf pass from a go pass")
    def toChess        = sys.error("Can't make a chess pass from a go pass")
    def toDraughts     = sys.error("Can't make a draughts pass from a go pass")
    def toSamurai      = sys.error("Can't make a samurai pass from a go pass")
    def toTogyzkumalak = sys.error("Can't make a togyzkumalak pass from a go pass")
    def toGo           = p

  }

  def wrap(p: go.Pass): Pass = Pass.Go(p)

}
