package strategygames.backgammon
import strategygames.MoveMetrics

import strategygames.backgammon.format.Uci

case class CubeAction(
    interaction: CubeInteraction,
    situationBefore: Situation,
    after: Board,
    metrics: MoveMetrics = MoveMetrics()
) extends Action(situationBefore, after, metrics) {

  def player = situationBefore.player

  def situationAfter =
    Situation(finalizeAfter, player)

  def finalizeAfter: Board = after updateHistory { h =>
    h.copy(
      currentTurn = h.currentTurn :+ toUci,
      forcedTurn = false,
      justUsedUndo = false
    )
  }

  def lazySituationAfter = situationAfter

  def withMetrics(m: MoveMetrics) = copy(metrics = m)

  def toUci = Uci.CubeAction(interaction)

  override def toString = toUci.uci

}

sealed trait CubeInteraction {
  val index: Int
  val name: String
  val char: Char
}

case object OfferDouble extends CubeInteraction {
  val index = 0
  val name  = "offer"
  val char  = 'o'
}

case object AcceptDouble extends CubeInteraction {
  val index = 1
  val name  = "accept"
  val char  = 'y'
}

case object RejectDouble extends CubeInteraction {
  val index = 2
  val name  = "reject"
  val char  = 'n'
}

object CubeInteraction {

  val all: List[CubeInteraction] = List(OfferDouble, AcceptDouble, RejectDouble)

}
