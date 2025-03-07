package strategygames

import strategygames.format.Uci

sealed abstract class DiceRoll(
    val dice: List[Int],
    val situationBefore: Situation,
    val after: Board,
    val autoEndTurn: Boolean,
    val metrics: MoveMetrics = MoveMetrics()
) extends Action(situationBefore) {

  def situationAfter: Situation

  def finalizeAfter: Board

  def player: Player = situationBefore.player

  def toUci: Uci.DiceRoll

  override def toString: String

  // TODO: Yup, still not type safe. :D
  def toChess: chess.DiceRoll
  def toBackgammon: backgammon.DiceRoll
}

object DiceRoll {

  final case class Chess(dr: chess.DiceRoll)
      extends DiceRoll(
        dr.dice,
        Situation.Chess(dr.situationBefore),
        Board.Chess(dr.after),
        dr.autoEndTurn,
        dr.metrics
      ) {

    def situationAfter: Situation = Situation.Chess(dr.situationAfter)
    def finalizeAfter: Board      = dr.finalizeAfter

    def toUci: Uci.DiceRoll = Uci.ChessDiceRoll(dr.toUci)

    val unwrap = dr

    def toChess        = dr
    def toDraughts     = sys.error("Can't make a draughts DiceRoll from a chess DiceRoll")
    def toFairySF      = sys.error("Can't make a fairysf DiceRoll from a chess DiceRoll")
    def toSamurai      = sys.error("Can't make a samurai DiceRoll from a chess DiceRoll")
    def toTogyzkumalak = sys.error("Can't make a togyzkumalak DiceRoll from a chess DiceRoll")
    def toGo           = sys.error("Can't make a go DiceRoll from a chess DiceRoll")
    def toBackgammon   = sys.error("Can't make a backgammon DiceRoll from a chess DiceRoll")
    def toAbalone      = sys.error("Can't make a abalone DiceRoll from a chess DiceRoll")
    def toDameo        = sys.error("Can't make a dameo DiceRoll from a chess DiceRoll")

  }

  final case class Backgammon(dr: backgammon.DiceRoll)
      extends DiceRoll(
        dr.dice,
        Situation.Backgammon(dr.situationBefore),
        Board.Backgammon(dr.after),
        false,
        dr.metrics
      ) {

    def situationAfter: Situation = Situation.Backgammon(dr.situationAfter)
    def finalizeAfter: Board      = dr.finalizeAfter

    def toUci: Uci.DiceRoll = Uci.BackgammonDiceRoll(dr.toUci)

    val unwrap = dr

    def toChess        = sys.error("Can't make a chess DiceRoll from a backgammon DiceRoll")
    def toDraughts     = sys.error("Can't make a draughts DiceRoll from a backgammon DiceRoll")
    def toFairySF      = sys.error("Can't make a fairysf DiceRoll from a backgammon DiceRoll")
    def toSamurai      = sys.error("Can't make a samurai DiceRoll from a backgammon DiceRoll")
    def toTogyzkumalak = sys.error("Can't make a togyzkumalak DiceRoll from a backgammon DiceRoll")
    def toGo           = sys.error("Can't make a go DiceRoll from a backgammon DiceRoll")
    def toBackgammon   = dr
    def toAbalone      = sys.error("Can't make a abalone DiceRoll from a backgammon DiceRoll")
    def toDameo        = sys.error("Can't make a dameo DiceRoll from a backgammon DiceRoll")

  }

  def wrap(dr: chess.DiceRoll): DiceRoll      = DiceRoll.Chess(dr)
  def wrap(dr: backgammon.DiceRoll): DiceRoll = DiceRoll.Backgammon(dr)

}
