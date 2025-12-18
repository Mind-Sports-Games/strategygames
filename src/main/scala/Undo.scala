package strategygames

import strategygames.format.Uci

sealed abstract class Undo(
    val situationBefore: Situation,
    val after: Board,
    val metrics: MoveMetrics = MoveMetrics()
) extends Action(situationBefore) {

  def situationAfter: Situation

  def finalizeAfter: Board

  def player: Player = situationBefore.player

  def toUci: Uci.Undo

  override def toString: String

  // TODO: Yup, still not type safe. :D
  def toBackgammon: backgammon.Undo
}

object Undo {

  final case class Backgammon(u: backgammon.Undo)
      extends Undo(
        Situation.Backgammon(u.situationBefore),
        Board.Backgammon(u.after),
        u.metrics
      ) {

    def situationAfter: Situation = Situation.Backgammon(u.situationAfter)
    def finalizeAfter: Board      = Board.Backgammon(u.finalizeAfter)

    def toUci: Uci.Undo = Uci.BackgammonUndo(u.toUci)

    val unwrap = u

    def toFairySF      = sys.error("Can't make a fairysf undo from a backgammon undo")
    def toChess        = sys.error("Can't make a chess undo from a backgammon undo")
    def toDraughts     = sys.error("Can't make a draughts undo from a backgammon undo")
    def toSamurai      = sys.error("Can't make a samurai undo from a backgammon undo")
    def toTogyzkumalak = sys.error("Can't make a togyzkumalak undo from a backgammon undo")
    def toGo           = sys.error("Can't make a go undo from a backgammon undo")
    def toBackgammon   = u
    def toAbalone      = sys.error("Can't make a abalone undo from a backgammon undo")
    def toDameo        = sys.error("Can't make a dameo undo from a backgammon undo")

  }

  def wrap(u: backgammon.Undo): Undo = Undo.Backgammon(u)

}
