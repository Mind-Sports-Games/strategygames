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

  final case class FairySF(p: fairysf.Pos) extends Pos {

    val key: String = p.key

    def piotr: Char = p.piotr

    lazy val toInt: Int = (p.file.index << 3) + p.rank.index

    lazy val all: List[Pos] = chess.Pos.all.map(Chess)

  }

  final case class Oware(p: oware.Pos) extends Pos {

    val key: String = p.key

    def piotr: Char = p.piotr

    lazy val toInt: Int = (p.file.index << 3) + p.rank.index

    lazy val all: List[Pos] = chess.Pos.all.map(Chess)

  }

  //need to equivalate this method for draughts probably
  //think we need to figure out a way to map into Draughts with a board size at this point
  def fromKey(lib: GameLogic, key: String): Option[Pos] = lib match {
    case GameLogic.Draughts() => sys.error("Not implemented yet for draughts")
    case GameLogic.Chess() => chess.Pos.fromKey(key).map(Chess)
    case GameLogic.FairySF() => sys.error("Not implemented yet for fairysf")
    case GameLogic.Oware() => sys.error("Not implemented yet for oware")
  }

  //def at(lib: GameLogic, x: Int, y: Int): Option[Pos] = lib match {
  //  case GameLogic.Draughts() => draughts.Pos.at(x, y).map(Draughts)
  //  case GameLogic.Chess() => chess.Pos.at(x, y).map(Chess)
  //}

}
