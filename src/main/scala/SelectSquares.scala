package strategygames

import strategygames.format.Uci

sealed abstract class SelectSquares(
    val squares: List[Pos],
    val situationBefore: Situation,
    val after: Board,
    val autoEndTurn: Boolean,
    val metrics: MoveMetrics = MoveMetrics()
) extends Action(situationBefore) {

  def situationAfter: Situation

  def finalizeAfter: Board

  def player: Player = situationBefore.player

  def toUci: Uci.SelectSquares

  override def toString: String

  // TODO: Yup, still not type safe. :D
  def toGo: go.SelectSquares
}

object SelectSquares {

  final case class Go(ss: go.SelectSquares)
      extends SelectSquares(
        ss.squares.map(Pos.Go(_)),
        Situation.Go(ss.situationBefore),
        Board.Go(ss.after),
        ss.autoEndTurn,
        ss.metrics
      ) {

    def situationAfter: Situation = Situation.Go(ss.situationAfter)
    def finalizeAfter: Board      = ss.finalizeAfter

    def toUci: Uci.SelectSquares = Uci.GoSelectSquares(ss.toUci)

    val unwrap = ss

    def toFairySF      = sys.error("Can't make a fairysf SelectSquares from a go SelectSquares")
    def toChess        = sys.error("Can't make a chess SelectSquares from a go SelectSquares")
    def toDraughts     = sys.error("Can't make a draughts SelectSquares from a go SelectSquares")
    def toSamurai      = sys.error("Can't make a samurai SelectSquares from a go SelectSquares")
    def toTogyzkumalak = sys.error("Can't make a togyzkumalak SelectSquares from a go SelectSquares")
    def toGo           = ss
    def toBackgammon   = sys.error("Can't make a backgammon SelectSquares from a go SelectSquares")
    def toAbalone      = sys.error("Can't make an abalone SelectSquares from a go SelectSquares")
    def toDameo        = sys.error("Can't make a dameo SelectSquares from a go SelectSquares")

  }

  def wrap(ss: go.SelectSquares): SelectSquares = SelectSquares.Go(ss)

}
