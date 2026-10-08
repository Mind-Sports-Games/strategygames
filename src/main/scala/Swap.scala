package strategygames

import strategygames.format.Uci

sealed abstract class Swap(
    val situationBefore: Situation,
    val after: Board,
    val autoEndTurn: Boolean,
    val metrics: MoveMetrics = MoveMetrics()
) extends Action(situationBefore) {
  def situationAfter: Situation

  def finalizeAfter: Board

  def player: Player = situationBefore.player

  def toUci: Uci.Swap

  override def toString: String

  def toFiveInARow: fiveinarow.Swap
}

object Swap {

  final case class FiveInARow(s: fiveinarow.Swap)
      extends Swap(
        Situation.FiveInARow(s.situationBefore),
        Board.FiveInARow(s.after),
        s.autoEndTurn,
        s.metrics
      ) {

    def situationAfter: Situation = Situation.FiveInARow(s.situationAfter)
    def finalizeAfter: Board      = s.finalizeAfter

    def toUci: Uci.Swap = Uci.FiveInARowSwap(s.toUci)

    val unwrap = s

    def toChess        = sys.error("Can't make a chess swap from a fiveinarow swap")
    def toDraughts     = sys.error("Can't make a draughts swap from a fiveinarow swap")
    def toFairySF      = sys.error("Can't make a fairysf swap from a fiveinarow swap")
    def toSamurai      = sys.error("Can't make a samurai swap from a fiveinarow swap")
    def toTogyzkumalak = sys.error("Can't make a togyzkumalak swap from a fiveinarow swap")
    def toGo           = sys.error("Can't make a go swap from a fiveinarow swap")
    def toBackgammon   = sys.error("Can't make a backgammon swap from a fiveinarow swap")
    def toAbalone      = sys.error("Can't make an abalone swap from a fiveinarow swap")
    def toDameo        = sys.error("Can't make a dameo swap from a fiveinarow swap")
    def toEntropy      = sys.error("Can't make an entropy swap from a fiveinarow swap")
    def toFiveInARow   = s

    override def toString = toUci.uci
  }

}
