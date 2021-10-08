package strategygames.fairysf

import strategygames.Color

case class Pockets(white: Pocket, black: Pocket) {

  def apply(color: Color) = color.fold(white, black)

  def take(piece: Piece): Option[Pockets] =
    piece.color.fold(
      white take piece.role map { np =>
        copy(white = np)
      },
      black take piece.role map { np =>
        copy(black = np)
      }
    )

  def store(piece: Piece) =
    piece.color.fold(
      copy(black = black store piece.role),
      copy(white = white store piece.role)
    )
}

case class Pocket(roles: List[Role]) {

  def take(role: Role) =
    if (roles contains role) Option(copy(roles = roles diff List(role)))
    else None

  def store(role: Role) =
    if (Role.storableRoles contains role) copy(roles = role :: roles)
    else this
}

case class PocketData(
    pockets: Pockets,
    // in crazyhouse, a promoted piece becomes a pawn
    // when captured and put in the pocket.
    // there we need to remember which pieces are issued from promotions.
    // we do that by tracking their positions on the board.
    promoted: Set[Pos]
) {

  def drop(piece: Piece): Option[PocketData] =
    pockets take piece map { nps =>
      copy(pockets = nps)
    }

  def store(piece: Piece, from: Pos) =
    copy(
      pockets = pockets store {
        if (promoted(from)) Piece(piece.color, Pawn) else piece
      },
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

