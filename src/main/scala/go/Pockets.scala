package strategygames.go

import strategygames.{ GameLogic, Piece => StratPiece, Pocket, Pockets }

case class PocketData(
    pockets: Pockets
) {

  def drop(piece: Piece): Option[PocketData] =
    pockets take StratPiece.Go(piece) map { nps =>
      copy(pockets = nps)
    }

}

object PocketData {
  val init = PocketData(
    Pockets(
      Pocket(List(strategygames.Role.GoRole(Stone), strategygames.Role.GoRole(Stone))),
      Pocket(List(strategygames.Role.GoRole(Stone), strategygames.Role.GoRole(Stone)))
    )
  )
}
