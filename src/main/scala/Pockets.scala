package strategygames

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

  def store(lib: GameLogic, piece: Piece) =
    piece.color.fold(
      copy(black = black store(lib, piece.role)),
      copy(white = white store(lib, piece.role))
    )
}

case class Pocket(roles: List[Role]) {

  def take(role: Role) =
    if (roles contains role) Option(copy(roles = roles diff List(role)))
    else None

  def store(lib: GameLogic, role: Role) =
    if (Role.storable(lib) contains role) copy(roles = role :: roles)
    else this
}

abstract sealed class PocketData(
  val pockets: Pockets,
  // in crazyhouse, a promoted piece becomes a pawn
  // when captured and put in the pocket.
  // there we need to remember which pieces are issued from promotions.
  // we do that by tracking their positions on the board.
  val promoted: Set[Pos]
)

object PocketData {

  case class Chess(p: chess.PocketData) extends PocketData(
    p.pockets,
    p.promoted.map(Pos.Chess)
  )

  case class FairySF(p: fairysf.PocketData) extends PocketData(
    p.pockets,
    p.promoted.map(Pos.FairySF)
  )

  def init(lib: GameLogic): PocketData = lib match {
    case GameLogic.Chess()   => Chess(chess.PocketData.init)
    case GameLogic.FairySF() => FairySF(fairysf.PocketData.init)
    case _ => sys.error("Unable to initialise pocket data for non chess/fairysf lib")
  }

}

