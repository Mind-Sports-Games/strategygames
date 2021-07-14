package strategygames


//------------------------------------------------------------------------------
// At the moment these are the main fields that I see exposed to lila.
//
// I am certain there are more, but I think it's best to do these at the same
// time as fixing up the usage of these within lila.
//------------------------------------------------------------------------------

sealed abstract class Pos{

  val key: String

  def piotr: Char

  def piotrStr = piotr.toString

  override def toString = key

}

object Pos {

  final case class Chess(p: chess.Pos) extends Pos {

    val key: String = p.key

    def piotr: Char = p.piotr

  }

  final case class Draughts(p: draughts.Pos) extends Pos {

    val key: String = p.key

    def piotr: Char = p.piotr

  }

  //def fromKey(lib: GameLib, key: String): Option[Pos] = lib match {
  //  case GameLib.Draughts() => draughts.Pos.fromKey(key).map(Draughts)
  //  case GameLib.Chess() => chess.Pos.fromKey(key).map(Chess)
  //}

  //def at(lib: GameLib, x: Int, y: Int): Option[Pos] = lib match {
  //  case GameLib.Draughts() => draughts.Pos.at(x, y).map(Draughts)
  //  case GameLib.Chess() => chess.Pos.at(x, y).map(Chess)
  //}

}
