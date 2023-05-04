package strategygames.go

import strategygames.{ GameFamily, GameLogic, Piece => StratPiece, Pocket, Pockets }

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

  def gameFamily: Option[GameFamily] = pockets.p1.roles.headOption match {
    case Some(strategygames.Role.GoRole(role)) => Some(role.gameFamily)
    case None                                  =>
      pockets.p2.roles.headOption match {
        case Some(strategygames.Role.GoRole(role)) => Some(role.gameFamily)
        case None                                  => None
        case _                                     => sys.error("Not got go roles in p2 pocket")
      }
    case _                                     => sys.error("Not got go roles in p1 pocket")
  }
}

object PocketData {
  val init = PocketData(Pockets(Pocket(Nil), Pocket(Nil)), Set.empty)
}
