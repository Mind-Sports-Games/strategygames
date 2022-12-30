package strategygames

sealed abstract class Piece(val player: Player, val role: Role) {

  def is(c: Player)    = c == player
  def is(r: Role)      = r == role
  def isNot(c: Player) = c != player
  def isNot(r: Role)   = r != role

  def oneOf(rs: Set[Role]) = rs(role)

  def forsyth: Char

  override def toString = s"$player-$role".toLowerCase

}

object Piece {

  final case class Chess(p: chess.Piece)
      extends Piece(
        p.player,
        Role.ChessRole(p.role)
      ) {

    def forsyth: Char = p.forsyth

  }

  final case class Draughts(p: draughts.Piece)
      extends Piece(
        p.player,
        Role.DraughtsRole(p.role)
      ) {

    def forsyth: Char = p.forsyth

  }

  final case class FairySF(p: fairysf.Piece)
      extends Piece(
        p.player,
        Role.FairySFRole(p.role)
      ) {

    def forsyth: Char = p.forsyth

  }

  final case class Samurai(p: samurai.Piece)
      extends Piece(
        p.player,
        Role.SamuraiRole(p.role)
      ) {

    def forsyth: Char = p.forsyth

  }

  final case class Togyzkumalak(p: togyzkumalak.Piece)
      extends Piece(
        p.player,
        Role.TogyzkumalakRole(p.role)
      ) {

    def forsyth: Char = p.forsyth

  }

  def apply(lib: GameLogic, player: Player, role: Role): Piece = (lib, role) match {
    case (GameLogic.Draughts(), Role.DraughtsRole(role)) => Draughts(draughts.Piece(player, role))
    case (GameLogic.Chess(), Role.ChessRole(role))       => Chess(chess.Piece(player, role))
    case (GameLogic.FairySF(), Role.FairySFRole(role))   => FairySF(fairysf.Piece(player, role))
    case (GameLogic.Samurai(), Role.SamuraiRole(role))   => Samurai(samurai.Piece(player, role))
    case (GameLogic.Togyzkumalak(), Role.TogyzkumalakRole(role))   => Togyzkumalak(togyzkumalak.Piece(player, role))
    case _                                               => sys.error("Mismatched gamelogic types 2")
  }

  def fromChar(lib: GameLogic, gf: GameFamily, c: Char): Option[Piece] = lib match {
    case (GameLogic.Draughts()) => draughts.Piece.fromChar(c).map(Draughts)
    case (GameLogic.Chess())    => chess.Piece.fromChar(c).map(Chess)
    case (GameLogic.FairySF())  => fairysf.Piece.fromChar(gf, c).map(FairySF)
    case (GameLogic.Samurai())  => sys.error("cannot get piece from Char for samurai anymore")
    case (GameLogic.Togyzkumalak())  => sys.error("cannot get piece from Char for togyzkumalak anymore")
  }

  def chessPieceMap(pieceMap: PieceMap): chess.PieceMap = pieceMap.map {
    case (Pos.Chess(pos), (Chess(piece), _)) => (pos, piece)
  }

  def draughtsPieceMap(pieceMap: PieceMap): draughts.PieceMap = pieceMap.map {
    case (Pos.Draughts(pos), (Draughts(piece), _)) => (pos, piece)
  }

  def fairySFPieceMap(pieceMap: PieceMap): fairysf.PieceMap = pieceMap.map {
    case (Pos.FairySF(pos), (FairySF(piece), _)) => (pos, piece)
  }

  def samuraiPieceMap(pieceMap: PieceMap): samurai.PieceMap = pieceMap.map {
    case (Pos.Samurai(pos), (Samurai(piece), count)) => (pos, (piece, count))
  }

  def togyzkumalakPieceMap(pieceMap: PieceMap): togyzkumalak.PieceMap = pieceMap.map {
    case (Pos.Togyzkumalak(pos), (Togyzkumalak(piece), count)) => (pos, (piece, count))
  }

  def pieceMapForChess(pieces: strategygames.chess.PieceMap): PieceMap = pieces.map {
    case (pos, piece) => (Pos.Chess(pos), (Piece.Chess(piece), 1))
  }

}
