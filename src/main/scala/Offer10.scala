package strategygames

import strategygames.format.Uci

sealed abstract class Offer10(
    val situationBefore: Situation,
    val after: Board,
    val autoEndTurn: Boolean,
    val metrics: MoveMetrics = MoveMetrics()
) extends Action(situationBefore) {
  def situationAfter: Situation

  def finalizeAfter: Board

  def player: Player = situationBefore.player

  def toUci: Uci.Offer10

  override def toString: String

  def toFiveInARow: fiveinarow.Offer10
}

object Offer10 {

  final case class FiveInARow(s: fiveinarow.Offer10)
      extends Offer10(
        Situation.FiveInARow(s.situationBefore),
        Board.FiveInARow(s.after),
        s.autoEndTurn,
        s.metrics
      ) {

    def situationAfter: Situation = Situation.FiveInARow(s.situationAfter)
    def finalizeAfter: Board      = s.finalizeAfter

    def toUci: Uci.Offer10 = Uci.FiveInARowOffer10(s.toUci)

    val unwrap = s

    def toChess        = sys.error("Can't make a chess offer10 from a fiveinarow offer10")
    def toDraughts     = sys.error("Can't make a draughts offer10 from a fiveinarow offer10")
    def toFairySF      = sys.error("Can't make a fairysf offer10 from a fiveinarow offer10")
    def toSamurai      = sys.error("Can't make a samurai offer10 from a fiveinarow offer10")
    def toTogyzkumalak = sys.error("Can't make a togyzkumalak offer10 from a fiveinarow offer10")
    def toGo           = sys.error("Can't make a go offer10 from a fiveinarow offer10")
    def toBackgammon   = sys.error("Can't make a backgammon offer10 from a fiveinarow offer10")
    def toAbalone      = sys.error("Can't make an abalone offer10 from a fiveinarow offer10")
    def toDameo        = sys.error("Can't make a dameo offer10 from a fiveinarow offer10")
    def toEntropy      = sys.error("Can't make an entropy offer10 from a fiveinarow offer10")
    def toFiveInARow   = s

    override def toString = toUci.uci
  }

}
