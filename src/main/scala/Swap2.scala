package strategygames

import strategygames.format.Uci

sealed abstract class Swap2(
    val situationBefore: Situation,
    val after: Board,
    val autoEndTurn: Boolean,
    val metrics: MoveMetrics = MoveMetrics()
) extends Action(situationBefore) {
  def situationAfter: Situation

  def finalizeAfter: Board

  def player: Player = situationBefore.player

  def toUci: Uci.Swap2

  override def toString: String

  def toFiveInARow: fiveinarow.Swap2
}

object Swap2 {

  final case class FiveInARow(s: fiveinarow.Swap2)
      extends Swap2(
        Situation.FiveInARow(s.situationBefore),
        Board.FiveInARow(s.after),
        s.autoEndTurn,
        s.metrics
      ) {

    def situationAfter: Situation = Situation.FiveInARow(s.situationAfter)
    def finalizeAfter: Board      = s.finalizeAfter

    def toUci: Uci.Swap2 = Uci.FiveInARowSwap2(s.toUci)

    val unwrap = s

    def toChess        = sys.error("Can't make a chess swap2 from a fiveinarow swap2")
    def toDraughts     = sys.error("Can't make a draughts swap2 from a fiveinarow swap2")
    def toFairySF      = sys.error("Can't make a fairysf swap2 from a fiveinarow swap2")
    def toSamurai      = sys.error("Can't make a samurai swap2 from a fiveinarow swap2")
    def toTogyzkumalak = sys.error("Can't make a togyzkumalak swap2 from a fiveinarow swap2")
    def toGo           = sys.error("Can't make a go swap2 from a fiveinarow swap2")
    def toBackgammon   = sys.error("Can't make a backgammon swap2 from a fiveinarow swap2")
    def toAbalone      = sys.error("Can't make an abalone swap2 from a fiveinarow swap2")
    def toDameo        = sys.error("Can't make a dameo swap2 from a fiveinarow swap2")
    def toEntropy      = sys.error("Can't make an entropy swap2 from a fiveinarow swap2")
    def toFiveInARow   = s

    override def toString = toUci.uci
  }

}
