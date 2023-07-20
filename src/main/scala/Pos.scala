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

    lazy val toInt: Int =
      (p.file.index << 3) + p.rank.index // todo where is this used? Should be 4 for fairy boards?

    lazy val all: List[Pos] = chess.Pos.all.map(Chess)

  }

  final case class Samurai(p: samurai.Pos) extends Pos {

    val key: String = p.key

    def piotr: Char = p.piotr

    lazy val toInt: Int = (p.file.index << 3) + p.rank.index

    lazy val all: List[Pos] = samurai.Pos.all.map(Samurai)

  }

  final case class Togyzkumalak(p: togyzkumalak.Pos) extends Pos {

    val key: String = p.key

    def piotr: Char = p.piotr

    lazy val toInt: Int = (p.file.index << 3) + p.rank.index

    lazy val all: List[Pos] = togyzkumalak.Pos.all.map(Togyzkumalak)

  }

  final case class Go(p: go.Pos) extends Pos {

    val key: String = p.key

    def piotr: Char = p.piotr

    lazy val toInt: Int = (p.file.index << 5) + p.rank.index // todo where is this used?

    lazy val all: List[Pos] = go.Pos.all.map(Go)

  }

  // need to equivalate this method for draughts probably
  // think we need to figure out a way to map into Draughts with a board size at this point
  def fromKey(lib: GameLogic, key: String): Option[Pos] = lib match {
    case GameLogic.Draughts()     => sys.error("Not implemented yet for draughts")
    case GameLogic.Chess()        => chess.Pos.fromKey(key).map(Chess)
    case GameLogic.FairySF()      => fairysf.Pos.fromKey(key).map(FairySF)
    case GameLogic.Samurai()      => samurai.Pos.fromKey(key).map(Samurai)
    case GameLogic.Togyzkumalak() => togyzkumalak.Pos.fromKey(key).map(Togyzkumalak)
    case GameLogic.Go()           => go.Pos.fromKey(key).map(Go)
  }

  // def at(lib: GameLogic, x: Int, y: Int): Option[Pos] = lib match {
  //  case GameLogic.Draughts() => draughts.Pos.at(x, y).map(Draughts)
  //  case GameLogic.Chess() => chess.Pos.at(x, y).map(Chess)
  // }

}
