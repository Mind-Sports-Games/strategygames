package strategygames

import strategygames.format.Uci

sealed abstract class Lift(
    val pos: Pos,
    val situationBefore: Situation,
    val after: Board,
    val autoEndTurn: Boolean,
    val metrics: MoveMetrics = MoveMetrics()
) extends Action(situationBefore) {

  def situationAfter: Situation

  def finalizeAfter: Board

  def player = situationBefore.player

  def toUci: Uci.Lift

  override def toString: String

  // TODO: Yup, still not type safe. :D
  def toBackgammon: backgammon.Lift
}

object Lift {

  final case class Backgammon(l: backgammon.Lift)
      extends Lift(
        Pos.Backgammon(l.pos),
        Situation.Backgammon(l.situationBefore),
        Board.Backgammon(l.after),
        false,
        l.metrics
      ) {

    def situationAfter: Situation = Situation.Backgammon(l.situationAfter)
    def finalizeAfter: Board      = l.finalizeAfter

    def toUci: Uci.Lift = Uci.BackgammonLift(l.toUci)

    val unwrap         = l
    def toChess        = sys.error("Can't make a chess lift from a backgammon lift")
    def toFairySF      = sys.error("Can't make a fairysf lift from a backgammon lift")
    def toDraughts     = sys.error("Can't make a draughts lift from a backgammon lift")
    def toSamurai      = sys.error("Can't make a samurai lift from a backgammon lift")
    def toTogyzkumalak = sys.error("Can't make a togy lift from a backgammon lift")
    def toGo           = sys.error("Can't make a go lift from a backgammon lift")
    def toBackgammon   = l
    def toAbalone      = sys.error("Can't make an abalone lift from a backgammon lift")
    def toDameo        = sys.error("Can't make a dameo lift from a backgammon lift")

  }

  def wrap(l: backgammon.Lift): Lift = Lift.Backgammon(l)

}
