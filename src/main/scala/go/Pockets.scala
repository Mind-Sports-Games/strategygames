package strategygames.go

import strategygames.{ GameLogic, Piece => StratPiece, Pocket, Pockets }

case class PocketData(
    pockets: Pockets,
    // in crazyhouse, a promoted piece becomes a pawn
    // when captured and put in the pocket.
    // there we need to remember which pieces are issued from promotions.
    // we do that by tracking their positions on the board.
    // we dont need to track this go, so it should always be empty
    promoted: Set[Pos]
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
      ),
      promoted = promoted - from
    )

  def promote(pos: Pos) = copy(promoted = promoted + pos)

  def move(orig: Pos, dest: Pos) =
    copy(
      promoted = if (promoted(orig)) promoted - orig + dest else promoted
    )

}

object PocketData {
  // val init = PocketData(Pockets(Pocket(Nil), Pocket(Nil)), Set.empty)
  val init = PocketData(
    Pockets(
      Pocket(List(strategygames.Role.GoRole(Stone), strategygames.Role.GoRole(Stone))),
      Pocket(List(strategygames.Role.GoRole(Stone), strategygames.Role.GoRole(Stone)))
    ),
    Set.empty
  )
}
