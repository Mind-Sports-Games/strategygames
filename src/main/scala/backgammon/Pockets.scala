package strategygames.backgammon

import strategygames.{ GameFamily, GameLogic, Piece => StratPiece, Pocket, Pockets }

case class PocketData(
    pockets: Pockets
) {

  def drop(piece: Piece): Option[PocketData] =
    pockets take StratPiece.Backgammon(piece) map { nps =>
      copy(pockets = nps)
    }

  def store(piece: Piece) =
    copy(
      pockets = pockets.store(
        GameLogic.Backgammon(),
        StratPiece.Backgammon(piece)
      )
    )

}

object PocketData {
  val init = PocketData(Pockets(Pocket(Nil), Pocket(Nil)))
}
