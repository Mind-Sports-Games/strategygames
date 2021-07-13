package strategygames


//------------------------------------------------------------------------------
// At the moment these are the main fields that I see exposed to lila.
//
// I am certain there are more, but I think it's best to do these at the same
// time as fixing up the usage of these within lila.
//------------------------------------------------------------------------------

sealed abstract class Pos

object Pos {
  final case class Chess(p: chess.Pos) extends Pos
  final case class Draughts(p: draughts.Pos) extends Pos

  def fromKey(lib: GameLib, key: String): Option[Pos] = lib match {
    case GameLib.Draughts() => chess.Pos.fromKey(key).map(DraughtsPos)
    case GameLib.Chess() => chess.Pos.posAt(key).map(ChessPos)
  }

  def at(lib: GameLib, x: Int, y: Int): Option[Pos] = lib match {
    case GameLib.Draughts() => chess.Pos.at(x, y).map(DraughtsPos)
    case GameLib.Chess() => chess.Pos.at(x, y).map(ChessPos)
  }

  def all(lib: GameLib): List[Pos] = lib match {
    case GameLib.Draughts() => chess.Pos.all().map(DraughtsPos)
    case GameLib.Chess() => chess.Pos.all().map(ChessPos)
  }
}
