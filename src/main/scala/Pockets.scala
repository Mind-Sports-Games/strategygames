package strategygames

import strategygames.Player

case class Pockets(p1: Pocket, p2: Pocket) {

  def apply(player: Player) = player.fold(p1, p2)

  def take(piece: Piece): Option[Pockets] =
    piece.player.fold(
      p1 take piece.role map { np =>
        copy(p1 = np)
      },
      p2 take piece.role map { np =>
        copy(p2 = np)
      }
    )

  def store(lib: GameLogic, piece: Piece) =
    piece.player.fold(
      copy(p2 = p2 store(lib, piece.role)),
      copy(p1 = p1 store(lib, piece.role))
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

