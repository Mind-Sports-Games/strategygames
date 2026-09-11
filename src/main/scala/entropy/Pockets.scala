package strategygames.entropy

import strategygames.{ GameLogic, Piece => StratPiece, Pocket, Pockets }

case class PocketData(
    pockets: Pockets
) {

  def drop(piece: Piece): Option[PocketData] =
    pockets take StratPiece.Entropy(piece) map { nps =>
      copy(pockets = nps)
    }

  def store(piece: Piece) =
    copy(
      pockets = pockets.returnToPocket(
        GameLogic.Entropy(),
        StratPiece.Entropy(piece)
      )
    )

}

object PocketData {
  val init = PocketData(Pockets(Pocket(Nil), Pocket(Nil)))
}
