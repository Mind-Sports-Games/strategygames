package strategygames

import strategygames.format.Uci

sealed abstract class DrawCounter(
    val role: Role,
    val situationBefore: Situation,
    val after: Board,
    val autoEndTurn: Boolean,
    val metrics: MoveMetrics = MoveMetrics()
) extends Action(situationBefore) {
  def situationAfter: Situation

  def finalizeAfter: Board

  def player: Player = situationBefore.player

  def toUci: Uci.DrawCounter

  override def toString: String

  def toEntropy: entropy.DrawCounter
}

object DrawCounter {

  final case class Entropy(dc: entropy.DrawCounter)
      extends DrawCounter(
        Role.EntropyRole(dc.role),
        Situation.Entropy(dc.situationBefore),
        Board.Entropy(dc.after),
        false,
        dc.metrics
      ) {

    def situationAfter: Situation = Situation.Entropy(dc.situationAfter)
    def finalizeAfter: Board      = dc.finalizeAfter

    def toUci: Uci.DrawCounter = Uci.EntropyDrawCounter((dc.toUci: entropy.format.Uci.DrawCounter))

    val unwrap = dc

    def toChess        = sys.error("Can't make a chess drawCounter from an entropy drawCounter")
    def toDraughts     = sys.error("Can't make a draughts drawCounter from an entropy drawCounter")
    def toFairySF      = sys.error("Can't make a fairysf drawCounter from an entropy drawCounter")
    def toSamurai      = sys.error("Can't make a samurai drawCounter from an entropy drawCounter")
    def toTogyzkumalak = sys.error("Can't make a togyzkumalak drawCounter from an entropy drawCounter")
    def toGo           = sys.error("Can't make a go drawCounter from an entropy drawCounter")
    def toBackgammon   = sys.error("Can't make a backgammon drawCounter from an entropy drawCounter")
    def toAbalone      = sys.error("Can't make an abalone drawCounter from an entropy drawCounter")
    def toDameo        = sys.error("Can't make a dameo drawCounter from an entropy drawCounter")
    def toEntropy      = dc

    override def toString = toUci.uci
  }

}
