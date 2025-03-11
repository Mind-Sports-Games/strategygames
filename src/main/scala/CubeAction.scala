package strategygames

import strategygames.format.Uci

sealed abstract class CubeAction(
    val interaction: CubeInteraction,
    val situationBefore: Situation,
    val after: Board,
    val autoEndTurn: Boolean,
    val metrics: MoveMetrics = MoveMetrics()
) extends Action(situationBefore) {

  def situationAfter: Situation

  def finalizeAfter: Board

  def player: Player = situationBefore.player

  def toUci: Uci.CubeAction

  override def toString: String

  // TODO: Yup, still not type safe. :D
  def toBackgammon: backgammon.CubeAction
}

object CubeAction {

  final case class Backgammon(c: backgammon.CubeAction)
      extends CubeAction(
        CubeInteraction.Backgammon(c.interaction),
        Situation.Backgammon(c.situationBefore),
        Board.Backgammon(c.after),
        true,
        c.metrics
      ) {

    def situationAfter: Situation = Situation.Backgammon(c.situationAfter)
    def finalizeAfter: Board      = c.finalizeAfter

    def toUci: Uci.CubeAction = Uci.BackgammonCubeAction(c.toUci)

    val unwrap = c

    def toChess        = sys.error("Can't make a chess cubeaction from a backgammon cubeaction")
    def toDraughts     = sys.error("Can't make a draughts cubeaction from a backgammon cubeaction")
    def toFairySF      = sys.error("Can't make a fairysf cubeaction from a backgammon cubeaction")
    def toSamurai      = sys.error("Can't make a samurai cubeaction from a backgammon cubeaction")
    def toTogyzkumalak = sys.error("Can't make a togyzkumalak cubeaction from a backgammon cubeaction")
    def toGo           = sys.error("Can't make a go cubeaction from a backgammon cubeaction")
    def toBackgammon   = c
    def toAbalone      = sys.error("Can't make a abalone cubeaction from a backgammon cubeaction")
    def toDameo        = sys.error("Can't make a dameo cubeaction from a backgammon cubeaction")

  }

  def wrap(c: backgammon.CubeAction): CubeAction = CubeAction.Backgammon(c)

}

sealed abstract class CubeInteraction(
    val index: Int,
    val name: String,
    val char: Char
)

object CubeInteraction {

  final case class Backgammon(c: backgammon.CubeInteraction)
      extends CubeInteraction(
        c.index,
        c.name,
        c.char
      ) {

    val unwrap = c

    def toChess        = sys.error("Can't make a chess cubeinteraction from a backgammon cubeinteraction")
    def toDraughts     = sys.error("Can't make a draughts cubeinteraction from a backgammon cubeinteraction")
    def toFairySF      = sys.error("Can't make a fairysf cubeinteraction from a backgammon cubeinteraction")
    def toSamurai      = sys.error("Can't make a samurai cubeinteraction from a backgammon cubeinteraction")
    def toTogyzkumalak =
      sys.error("Can't make a togyzkumalak cubeinteraction from a backgammon cubeinteraction")
    def toGo           = sys.error("Can't make a go cubeinteraction from a backgammon cubeinteraction")
    def toBackgammon   = c
    def toAbalone      = sys.error("Can't make a abalone cubeinteraction from a backgammon cubeinteraction")
    def toDameo        = sys.error("Can't make a dameo cubeinteraction from a backgammon cubeinteraction")

  }

  def wrap(c: backgammon.CubeInteraction): CubeInteraction = CubeInteraction.Backgammon(c)

}
