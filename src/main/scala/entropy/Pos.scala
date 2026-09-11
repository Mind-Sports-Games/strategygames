package strategygames.entropy

import scala.math.{ abs, max, min }

object Piotr {
  val lookup: Map[Int, Char] = Map(
    Pos.A1.index -> 'a',
    Pos.B1.index -> 'b',
    Pos.C1.index -> 'c',
    Pos.D1.index -> 'd',
    Pos.E1.index -> 'e',
    Pos.F1.index -> 'f',
    Pos.G1.index -> 'g',
    Pos.A2.index -> 'h',
    Pos.B2.index -> 'i',
    Pos.C2.index -> 'j',
    Pos.D2.index -> 'k',
    Pos.E2.index -> 'l',
    Pos.F2.index -> 'm',
    Pos.G2.index -> 'n',
    Pos.A3.index -> 'o',
    Pos.B3.index -> 'p',
    Pos.C3.index -> 'q',
    Pos.D3.index -> 'r',
    Pos.E3.index -> 's',
    Pos.F3.index -> 't',
    Pos.G3.index -> 'u',
    Pos.A4.index -> 'v',
    Pos.B4.index -> 'w',
    Pos.C4.index -> 'x',
    Pos.D4.index -> 'y',
    Pos.E4.index -> 'z',
    Pos.F4.index -> 'A',
    Pos.G4.index -> 'B',
    Pos.A5.index -> 'C',
    Pos.B5.index -> 'D',
    Pos.C5.index -> 'E',
    Pos.D5.index -> 'F',
    Pos.E5.index -> 'G',
    Pos.F5.index -> 'H',
    Pos.G5.index -> 'I',
    Pos.A6.index -> 'J',
    Pos.B6.index -> 'K',
    Pos.C6.index -> 'L',
    Pos.D6.index -> 'M',
    Pos.E6.index -> 'N',
    Pos.F6.index -> 'O',
    Pos.G6.index -> 'P',
    Pos.A7.index -> 'Q',
    Pos.B7.index -> 'R',
    Pos.C7.index -> 'S',
    Pos.D7.index -> 'T',
    Pos.E7.index -> 'U',
    Pos.F7.index -> 'V',
    Pos.G7.index -> 'W'
  )
}

case class Pos private (index: Int) extends AnyVal {

  def down: Option[Pos]  = Pos.at(file.index, rank.index - 1)
  def left: Option[Pos]  = Pos.at(file.index - 1, rank.index)
  def up: Option[Pos]    = Pos.at(file.index, rank.index + 1)
  def right: Option[Pos] = Pos.at(file.index + 1, rank.index)

  def >|(stop: Pos => Boolean): List[Pos]                   = |<>|(stop, _.right)
  def |<(stop: Pos => Boolean): List[Pos]                   = |<>|(stop, _.left)
  def |<>|(stop: Pos => Boolean, dir: Direction): List[Pos] =
    dir(this) map { p =>
      p :: (if (stop(p)) Nil else p.|<>|(stop, dir))
    } getOrElse Nil

  def ?<(other: Pos): Boolean = file < other.file
  def ?>(other: Pos): Boolean = file > other.file
  def ?+(other: Pos): Boolean = rank < other.rank
  def ?^(other: Pos): Boolean = rank > other.rank
  def ?|(other: Pos): Boolean = file == other.file
  def ?-(other: Pos): Boolean = rank == other.rank

  def <->(other: Pos): Iterable[Pos] =
    min(file.index, other.file.index) to max(file.index, other.file.index) flatMap { Pos.at(_, rank.index) }

  def touches(other: Pos): Boolean = xDist(other) <= 1 && yDist(other) <= 1

  def onSameLine(other: Pos): Boolean = ?-(other) || ?|(other)

  def xDist(other: Pos) = abs(file - other.file)
  def yDist(other: Pos) = abs(rank - other.rank)

  @inline def file = File of this
  @inline def rank = Rank of this

  def piotr: Char = Piotr.lookup.get(index).getOrElse('?')
  def piotrStr    = piotr.toString

  def sgf(numRanks: Int) = file.sgfChar.toString + rank.sgfChar(numRanks).toString

  def key               = file.toString + rank.toString
  override def toString = key

  def step(dx: Int, dy: Int): Option[Pos] =
    Pos.at(file.index + dx, rank.index + dy)
}

object Pos {
  def apply(index: Int): Option[Pos] =
    if (0 <= index && index < File.allSize * Rank.allSize) Some(new Pos(index))
    else None

  def apply(file: File, rank: Rank): Pos = new Pos(file.index + File.allSize * rank.index)

  def at(x: Int, y: Int): Option[Pos] =
    if (0 <= x && x < File.allSize && 0 <= y && y < Rank.allSize)
      Some(new Pos(x + File.allSize * y))
    else None

  def fromKey(key: String): Option[Pos] = allKeys get key

  def piotr(c: Char): Option[Pos] = allPiotrs get c

  def keyToPiotr(key: String)          = fromKey(key) map (_.piotr)
  def doubleKeyToPiotr(key: String)    =
    for {
      a <- keyToPiotr(key take 2)
      b <- keyToPiotr(key drop 2)
    } yield s"$a$b"
  def doublePiotrToKey(piotrs: String) =
    for {
      a <- piotr(piotrs.head)
      b <- piotr(piotrs(1))
    } yield s"${a.key}${b.key}"

  val A1 = new Pos(0)
  val B1 = new Pos(1)
  val C1 = new Pos(2)
  val D1 = new Pos(3)
  val E1 = new Pos(4)
  val F1 = new Pos(5)
  val G1 = new Pos(6)
  val A2 = new Pos(7)
  val B2 = new Pos(8)
  val C2 = new Pos(9)
  val D2 = new Pos(10)
  val E2 = new Pos(11)
  val F2 = new Pos(12)
  val G2 = new Pos(13)
  val A3 = new Pos(14)
  val B3 = new Pos(15)
  val C3 = new Pos(16)
  val D3 = new Pos(17)
  val E3 = new Pos(18)
  val F3 = new Pos(19)
  val G3 = new Pos(20)
  val A4 = new Pos(21)
  val B4 = new Pos(22)
  val C4 = new Pos(23)
  val D4 = new Pos(24)
  val E4 = new Pos(25)
  val F4 = new Pos(26)
  val G4 = new Pos(27)
  val A5 = new Pos(28)
  val B5 = new Pos(29)
  val C5 = new Pos(30)
  val D5 = new Pos(31)
  val E5 = new Pos(32)
  val F5 = new Pos(33)
  val G5 = new Pos(34)
  val A6 = new Pos(35)
  val B6 = new Pos(36)
  val C6 = new Pos(37)
  val D6 = new Pos(38)
  val E6 = new Pos(39)
  val F6 = new Pos(40)
  val G6 = new Pos(41)
  val A7 = new Pos(42)
  val B7 = new Pos(43)
  val C7 = new Pos(44)
  val D7 = new Pos(45)
  val E7 = new Pos(46)
  val F7 = new Pos(47)
  val G7 = new Pos(48)

  // in a1-to-g7 sweep order, which the forced drop of a flagged Chaos relies on
  val all: List[Pos] = (0 to (File.allSize * Rank.allSize) - 1).map(new Pos(_)).toList
  val allSize: Int   = all.size

  val allKeys: Map[String, Pos] = all
    .map { pos =>
      pos.key -> pos
    }
    .to(Map)

  val allPiotrs: Map[Char, Pos] = all
    .map { pos =>
      pos.piotr -> pos
    }
    .to(Map)

  val posR = "([a-g][1-7])"

}
