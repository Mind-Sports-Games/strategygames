package strategygames.go

import strategygames.{ GameFamily, GameLogic, Piece => StratPiece, Pocket, Pockets }

case class PocketData(
    pockets: Pockets
) {

  def drop(piece: Piece): Option[PocketData] =
    pockets take StratPiece.Go(piece) map { nps =>
      copy(pockets = nps)
    }

  def store(piece: Piece, from: Pos) =
    copy(
      pockets = pockets.store(
        GameLogic.Go(),
        StratPiece.Go(piece)
      )
    )

}

object PocketData {
  // val init = PocketData(Pockets(Pocket(Nil), Pocket(Nil)), Set.empty)
  val init = PocketData(
    Pockets(
      Pocket(List(strategygames.Role.GoRole(Stone), strategygames.Role.GoRole(Stone))),
      Pocket(List(strategygames.Role.GoRole(Stone), strategygames.Role.GoRole(Stone)))
    )
  )
}
