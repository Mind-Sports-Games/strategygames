package strategygames

import cats.syntax.option.none

import strategygames.format.Uci

sealed abstract class SelectSquares(
    val squares: List[Pos],
    val situationBefore: Situation,
    val after: Board,
    val metrics: MoveMetrics = MoveMetrics()
) extends Action(situationBefore, after, metrics) {

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

  }

  def wrap(ss: go.SelectSquares): SelectSquares = SelectSquares.Go(ss)

}
