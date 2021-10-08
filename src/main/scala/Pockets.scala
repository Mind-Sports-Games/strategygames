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
) {

  def drop(piece: Piece): Option[PocketData]

  def store(piece: Piece, from: Pos): PocketData

  def promote(pos: Pos): PocketData

  def move(orig: Pos, dest: Pos): PocketData

}

object PocketData {

  case class Chess(p: chess.PocketData) extends PocketData(
    p.pockets,
    p.promoted.map(Pos.Chess)
  ) {
    
    def drop(piece: Piece): Option[PocketData] = piece match {
      case Piece.Chess(piece) => p.drop(piece).map(Chess)
      case _ => sys.error("Not passed Chess objects")
    }

    def store(piece: Piece, from: Pos): PocketData = (piece, from) match {
      case (Piece.Chess(piece), Pos.Chess(from)) => Chess(p.store(piece, from))
      case _ => sys.error("Not passed Chess objects")
    }

    def promote(pos: Pos): PocketData = pos match {
      case Pos.Chess(pos) => Chess(p.promote(pos))
      case _ => sys.error("Not passed Chess objects")
    }

    def move(orig: Pos, dest: Pos): PocketData = (orig, dest) match {
      case (Pos.Chess(orig), Pos.Chess(dest)) => Chess(p.move(orig, dest))
      case _ => sys.error("Not passed Chess objects")
    }

  }

  case class FairySF(p: fairysf.PocketData) extends PocketData(
    p.pockets,
    p.promoted.map(Pos.FairySF)
  ) {
    
    def drop(piece: Piece): Option[PocketData] = piece match {
      case Piece.FairySF(piece) => p.drop(piece).map(FairySF)
      case _ => sys.error("Not passed FairySF objects")
    }

    def store(piece: Piece, from: Pos): PocketData = (piece, from) match {
      case (Piece.FairySF(piece), Pos.FairySF(from)) => FairySF(p.store(piece, from))
      case _ => sys.error("Not passed FairySF objects")
    }

    def promote(pos: Pos): PocketData = pos match {
      case Pos.FairySF(pos) => FairySF(p.promote(pos))
      case _ => sys.error("Not passed FairySF objects")
    }

    def move(orig: Pos, dest: Pos): PocketData = (orig, dest) match {
      case (Pos.FairySF(orig), Pos.FairySF(dest)) => FairySF(p.move(orig, dest))
      case _ => sys.error("Not passed FairySF objects")
    }

  }

  def init(lib: GameLogic): PocketData = lib match {
    case GameLogic.Chess()   => Chess(chess.PocketData.init)
    case GameLogic.FairySF() => FairySF(fairysf.PocketData.init)
    case _ => sys.error("Unable to initialise pocket data for non chess/fairysf lib")
  }

}

