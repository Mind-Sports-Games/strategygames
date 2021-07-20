package strategygames


//------------------------------------------------------------------------------
// At the moment these are the main fields that I see exposed to lila.
//
// I am certain there are more, but I think it's best to do these at the same
// time as fixing up the usage of these within lila.
//------------------------------------------------------------------------------

sealed abstract class Pos {

  val key: String

  def piotr: Char

  def piotrStr = piotr.toString

  def toInt: Int

  override def toString = key

  def all: List[Pos]

}

object Pos {

  final case class Chess(p: chess.Pos) extends Pos {

    val key: String = p.key

    def piotr: Char = p.piotr

    lazy val toInt: Int = (p.file.index << 3) + p.rank.index

    lazy val all: List[Pos] = chess.Pos.all.map(Chess)

  }

  final case class Draughts(p: draughts.Pos) extends Pos {

    val key: String = p.key

    def piotr: Char = p.piotr

    // TODO: not sure this is appropriate, but I don't see why not?
    lazy val toInt: Int = p.fieldNumber

    // TODO: this only handl 8x8 boards. we should include 10x10 as well.
    //       Not sure if we need a separate type, probably?
    lazy val all: List[Pos] = draughts.Pos64.all.map(Draughts)

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
