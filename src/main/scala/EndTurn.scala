package strategygames

import strategygames.format.Uci

sealed abstract class EndTurn(
    val situationBefore: Situation,
    val after: Board,
    val metrics: MoveMetrics = MoveMetrics()
) extends Action(situationBefore) {

  def situationAfter: Situation

  def finalizeAfter: Board

  def player: Player = situationBefore.player

  def toUci: Uci.EndTurn

  override def toString: String

  // TODO: Yup, still not type safe. :D
  def toBackgammon: backgammon.EndTurn
}

object EndTurn {

  final case class Backgammon(et: backgammon.EndTurn)
      extends EndTurn(
        Situation.Backgammon(et.situationBefore),
        Board.Backgammon(et.after),
        et.metrics
      ) {

    def situationAfter: Situation = Situation.Backgammon(et.situationAfter)
    def finalizeAfter: Board      = et.finalizeAfter

    def toUci: Uci.EndTurn = Uci.BackgammonEndTurn(et.toUci)

    val unwrap = et

    def toFairySF      = sys.error("Can't make a fairysf endturn from a backgammon endturn")
    def toChess        = sys.error("Can't make a chess endturn from a backgammon endturn")
    def toDraughts     = sys.error("Can't make a draughts endturn from a backgammon endturn")
    def toSamurai      = sys.error("Can't make a samurai endturn from a backgammon endturn")
    def toTogyzkumalak = sys.error("Can't make a togyzkumalak endturn from a backgammon endturn")
    def toGo           = sys.error("Can't make a go endturn from a backgammon endturn")
    def toBackgammon   = et
    def toAbalone      = sys.error("Can't make a abalone endturn from a backgammon endturn")
    def toDameo        = sys.error("Can't make a dameo endturn from a backgammon endturn")

  }

  def wrap(et: backgammon.EndTurn): EndTurn = EndTurn.Backgammon(et)

}
