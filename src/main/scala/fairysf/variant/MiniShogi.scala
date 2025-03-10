package strategygames.fairysf
package variant

import strategygames.fairysf._
import strategygames.GameFamily

case object MiniShogi
    extends Variant(
      id = 5,
      key = "minishogi",
      name = "Mini Shogi",
      standardInitialPosition = true,
      fishnetKey = "minishogi",
      boardSize = Board.Dim5x5
    ) {

  def gameFamily: GameFamily = GameFamily.Shogi()

  override def dropsVariant      = true
  override def hasDetachedPocket = true

  override def useFairyOptionalGameEnd = true

  def perfIcon: Char = ''
  def perfId: Int    = 202

  override val kingPiece: Option[Role] = Some(ShogiKing)

  // cache this rather than checking with the API everytime
  override def initialFen = format.FEN("rbsgk/4p/5/P4/KGSBR[] w - - 0 1")

  // manually calculated where might put king in mate
  // this was done for optimisation but could go back to just checking the api lots?
  override def validDrops(situation: Situation): List[Drop] =
    super
      .validDrops(situation)
      .filterNot(d =>
        d.piece.role == ShogiPawn && {
          val kingPos = situation.board.posMap
            .get(
              Piece(!situation.player, ShogiKing)
            )
            .flatMap(_.headOption)
          Some(d.pos) == (situation.player match {
            case P1 => kingPos.flatMap(_.down)
            case P2 => kingPos.flatMap(_.up)
          })
        } && situation.board.apiPosition.makeMoves(List(d.toUci.uci)).gameResult == GameResult.Checkmate()
      )

}
