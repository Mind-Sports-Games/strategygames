package strategygames.chess

import strategygames.{ Player, GameLogic, Piece => StratPiece, Pocket, Pockets }

case class PocketData(
    pockets: Pockets,
    // in crazyhouse, a promoted piece becomes a pawn
    // when captured and put in the pocket.
    // there we need to remember which pieces are issued from promotions.
    // we do that by tracking their positions on the board.
    promoted: Set[Pos]
) {

  def drop(piece: Piece): Option[PocketData] =
    pockets take StratPiece.Chess(piece) map { nps =>
      copy(pockets = nps)
    }

  def store(piece: Piece, from: Pos) =
    copy(
      pockets = pockets.store(
        GameLogic.Chess(),
        StratPiece.Chess(if (promoted(from)) Piece(piece.player, Pawn) else piece)
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
  val init = PocketData(Pockets(Pocket(Nil), Pocket(Nil)), Set.empty)
}

