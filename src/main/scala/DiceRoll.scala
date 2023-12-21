package strategygames

import cats.syntax.option.none

import strategygames.format.Uci

sealed abstract class DiceRoll(
    val diceRoll: List[Int],
    val situationBefore: Situation,
    val after: Board,
    val autoEndTurn: Boolean,
    val metrics: MoveMetrics = MoveMetrics()
) extends Action(situationBefore, after, metrics) {

  def situationAfter: Situation

  def finalizeAfter: Board

  def player: Player = situationBefore.player

  def toUci: Uci.DiceRoll

  override def toString: String

  // TODO: Yup, still not type safe. :D
  def toBackgammon: backgammon.DiceRoll
}

object DiceRoll {

  final case class Backgammon(dr: backgammon.DiceRoll)
      extends DiceRoll(
        dr.dice,
        Situation.Backgammon(dr.situationBefore),
        Board.Backgammon(dr.after),
        dr.autoEndTurn,
        dr.metrics
      ) {

    def situationAfter: Situation = Situation.Backgammon(dr.situationAfter)
    def finalizeAfter: Board      = dr.finalizeAfter

    def toUci: Uci.DiceRoll = Uci.BackgammonDiceRoll(dr.toUci)

    val unwrap = dr

    def toFairySF      = sys.error("Can't make a fairysf DiceRoll from a backgammon DiceRoll")
    def toChess        = sys.error("Can't make a chess DiceRoll from a backgammon DiceRoll")
    def toDraughts     = sys.error("Can't make a draughts DiceRoll from a backgammon DiceRoll")
    def toSamurai      = sys.error("Can't make a samurai DiceRoll from a backgammon DiceRoll")
    def toTogyzkumalak = sys.error("Can't make a togyzkumalak DiceRoll from a backgammon DiceRoll")
    def toGo           = sys.error("Can't make a go DiceRoll from a backgammon DiceRoll")
    def toBackgammon   = dr

  }

  def wrap(dr: backgammon.DiceRoll): DiceRoll = DiceRoll.Backgammon(dr)

}
