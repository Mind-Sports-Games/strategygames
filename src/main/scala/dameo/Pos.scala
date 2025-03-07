package strategygames.dameo

import scala.math.{ abs, max, min }

// Matches with: https://github.com/Mind-Sports-Games/lila/blob/incoming-prs/ui/chess/src/piotr.ts
object Piotr {
  val lookup: Map[Int, Char] = Map(
    Pos.A1.index  -> 'a',
    Pos.B1.index  -> 'b',
    Pos.C1.index  -> 'c',
    Pos.D1.index  -> 'd',
    Pos.E1.index  -> 'e',
    Pos.F1.index  -> 'f',
    Pos.G1.index  -> 'g',
    Pos.H1.index  -> 'h',
    Pos.A2.index  -> 'i',
    Pos.B2.index  -> 'j',
    Pos.C2.index  -> 'k',
    Pos.D2.index  -> 'l',
    Pos.E2.index  -> 'm',
    Pos.F2.index  -> 'n',
    Pos.G2.index  -> 'o',
    Pos.H2.index  -> 'p',
    Pos.A3.index  -> 'q',
    Pos.B3.index  -> 'r',
    Pos.C3.index  -> 's',
    Pos.D3.index  -> 't',
    Pos.E3.index  -> 'u',
    Pos.F3.index  -> 'v',
    Pos.G3.index  -> 'w',
    Pos.H3.index  -> 'x',
    Pos.A4.index  -> 'y',
    Pos.B4.index  -> 'z',
    Pos.C4.index  -> 'A',
    Pos.D4.index  -> 'B',
    Pos.E4.index  -> 'C',
    Pos.F4.index  -> 'D',
    Pos.G4.index  -> 'E',
    Pos.H4.index  -> 'F',
    Pos.A5.index  -> 'G',
    Pos.B5.index  -> 'H',
    Pos.C5.index  -> 'I',
    Pos.D5.index  -> 'J',
    Pos.E5.index  -> 'K',
    Pos.F5.index  -> 'L',
    Pos.G5.index  -> 'M',
    Pos.H5.index  -> 'N',
    Pos.A6.index  -> 'O',
    Pos.B6.index  -> 'P',
    Pos.C6.index  -> 'Q',
    Pos.D6.index  -> 'R',
    Pos.E6.index  -> 'S',
    Pos.F6.index  -> 'T',
    Pos.G6.index  -> 'U',
    Pos.H6.index  -> 'V',
    Pos.A7.index  -> 'W',
    Pos.B7.index  -> 'X',
    Pos.C7.index  -> 'Y',
    Pos.D7.index  -> 'Z',
    Pos.E7.index  -> '0',
    Pos.F7.index  -> '1',
    Pos.G7.index  -> '2',
    Pos.H7.index  -> '3',
    Pos.A8.index  -> '4',
    Pos.B8.index  -> '5',
    Pos.C8.index  -> '6',
    Pos.D8.index  -> '7',
    Pos.E8.index  -> '8',
    Pos.F8.index  -> '9',
    Pos.G8.index  -> '!',
    Pos.H8.index  -> '?',
    Pos.A9.index  -> '\"',
    Pos.B9.index  -> '#',
    Pos.C9.index  -> '$',
    Pos.D9.index  -> '%',
    Pos.E9.index  -> '&',
    Pos.F9.index  -> '\'',
    Pos.G9.index  -> '(',
    Pos.H9.index  -> ')',
    Pos.I9.index  -> '*',
    Pos.J9.index  -> '+',
    Pos.A10.index -> '§', // NOTE: comma is not a valid piotr due to the way lila stores analysis.
    Pos.B10.index -> '-',
    Pos.C10.index -> '.',
    Pos.D10.index -> '/',
    Pos.E10.index -> ':',
    Pos.F10.index -> '¨', // NOTE: semicolon can't be used for the same reason as above
    Pos.G10.index -> '<',
    Pos.H10.index -> '=',
    Pos.I10.index -> '>',
    Pos.J10.index -> '@',
    Pos.I1.index  -> '[',
    Pos.J1.index  -> '\\',
    Pos.I2.index  -> ']',
    Pos.J2.index  -> '^',
    Pos.I3.index  -> '_',
    Pos.J3.index  -> '`',
    Pos.I4.index  -> '{',
    Pos.J4.index  -> '|',
    Pos.I5.index  -> '}',
    Pos.J5.index  -> '~',
    // https://en.wikipedia.org/wiki/List_of_Unicode_characters#Latin_script
    // from the latin-1 script
    Pos.I6.index  -> '¡',
    Pos.J6.index  -> '¢',
    Pos.I7.index  -> '£',
    Pos.J7.index  -> '¤',
    Pos.I8.index  -> '¥',
    Pos.J8.index  -> '¦'
    // NOTE: "§" and "¨" are used above
  )
}

case class Pos private (index: Int) extends AnyVal {

  def down: Option[Pos]      = Pos.at(file.index, rank.index - 1)
  def left: Option[Pos]      = Pos.at(file.index - 1, rank.index)
  def downLeft: Option[Pos]  = Pos.at(file.index - 1, rank.index - 1)
  def downRight: Option[Pos] = Pos.at(file.index + 1, rank.index - 1)
  def up: Option[Pos]        = Pos.at(file.index, rank.index + 1)
  def right: Option[Pos]     = Pos.at(file.index + 1, rank.index)
  def upLeft: Option[Pos]    = Pos.at(file.index - 1, rank.index + 1)
  def upRight: Option[Pos]   = Pos.at(file.index + 1, rank.index + 1)

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

  def onSameDiagonal(other: Pos): Boolean =
    file.index - rank.index == other.file.index - other.rank.index || file.index + rank.index == other.file.index + other.rank.index
  def onSameLine(other: Pos): Boolean     = ?-(other) || ?|(other)

  def xDist(other: Pos) = abs(file - other.file)
  def yDist(other: Pos) = abs(rank - other.rank)

  def isLight: Boolean = (file.index + rank.index) % 2 == 1

  @inline def file = File of this
  @inline def rank = Rank of this

  def piotr: Char = Piotr.lookup.get(index).getOrElse('?')
  def piotrStr    = piotr.toString

  def sgf(numRanks: Int) = file.sgfChar.toString + rank.sgfChar(numRanks).toString

  def key               = file.toString + rank.toString
  override def toString = key
}

object Pos {
  def apply(index: Int): Option[Pos] =
    if (0 <= index && index < File.all.size * Rank.all.size) Some(new Pos(index))
    else None

  def apply(file: File, rank: Rank): Pos = new Pos(File.all.size * rank.index + file.index)

  def at(x: Int, y: Int): Option[Pos] =
    File(x) zip Rank(y) map { case (file, rank) =>
      Pos(file, rank)
    }

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

  //TODO Dameo - write this
  //Works out the pos between the orig and dest that has been jumped over (i.e. where the capture took place
  def capturePos(orig: Pos, dest: Pos): Option[Pos] = None

  val A1  = new Pos(0)
  val B1  = new Pos(1)
  val C1  = new Pos(2)
  val D1  = new Pos(3)
  val E1  = new Pos(4)
  val F1  = new Pos(5)
  val G1  = new Pos(6)
  val H1  = new Pos(7)
  val I1  = new Pos(8)
  val J1  = new Pos(9)
  val A2  = new Pos(10)
  val B2  = new Pos(11)
  val C2  = new Pos(12)
  val D2  = new Pos(13)
  val E2  = new Pos(14)
  val F2  = new Pos(15)
  val G2  = new Pos(16)
  val H2  = new Pos(17)
  val I2  = new Pos(18)
  val J2  = new Pos(19)
  val A3  = new Pos(20)
  val B3  = new Pos(21)
  val C3  = new Pos(22)
  val D3  = new Pos(23)
  val E3  = new Pos(24)
  val F3  = new Pos(25)
  val G3  = new Pos(26)
  val H3  = new Pos(27)
  val I3  = new Pos(28)
  val J3  = new Pos(29)
  val A4  = new Pos(30)
  val B4  = new Pos(31)
  val C4  = new Pos(32)
  val D4  = new Pos(33)
  val E4  = new Pos(34)
  val F4  = new Pos(35)
  val G4  = new Pos(36)
  val H4  = new Pos(37)
  val I4  = new Pos(38)
  val J4  = new Pos(39)
  val A5  = new Pos(40)
  val B5  = new Pos(41)
  val C5  = new Pos(42)
  val D5  = new Pos(43)
  val E5  = new Pos(44)
  val F5  = new Pos(45)
  val G5  = new Pos(46)
  val H5  = new Pos(47)
  val I5  = new Pos(48)
  val J5  = new Pos(49)
  val A6  = new Pos(50)
  val B6  = new Pos(51)
  val C6  = new Pos(52)
  val D6  = new Pos(53)
  val E6  = new Pos(54)
  val F6  = new Pos(55)
  val G6  = new Pos(56)
  val H6  = new Pos(57)
  val I6  = new Pos(58)
  val J6  = new Pos(59)
  val A7  = new Pos(60)
  val B7  = new Pos(61)
  val C7  = new Pos(62)
  val D7  = new Pos(63)
  val E7  = new Pos(64)
  val F7  = new Pos(65)
  val G7  = new Pos(66)
  val H7  = new Pos(67)
  val I7  = new Pos(68)
  val J7  = new Pos(69)
  val A8  = new Pos(70)
  val B8  = new Pos(71)
  val C8  = new Pos(72)
  val D8  = new Pos(73)
  val E8  = new Pos(74)
  val F8  = new Pos(75)
  val G8  = new Pos(76)
  val H8  = new Pos(77)
  val I8  = new Pos(78)
  val J8  = new Pos(79)
  val A9  = new Pos(80)
  val B9  = new Pos(81)
  val C9  = new Pos(82)
  val D9  = new Pos(83)
  val E9  = new Pos(84)
  val F9  = new Pos(85)
  val G9  = new Pos(86)
  val H9  = new Pos(87)
  val I9  = new Pos(88)
  val J9  = new Pos(89)
  val A10 = new Pos(90)
  val B10 = new Pos(91)
  val C10 = new Pos(92)
  val D10 = new Pos(93)
  val E10 = new Pos(94)
  val F10 = new Pos(95)
  val G10 = new Pos(96)
  val H10 = new Pos(97)
  val I10 = new Pos(98)
  val J10 = new Pos(99)

  // current pos limit in db is 128, if adding more perhaps use a different method (map of index to file, rank)

  // if adding new Pos check for use of Pos.all
  val all: List[Pos] = (0 to (File.all.size * Rank.all.size) - 1).map(new Pos(_)).toList

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

  val posR = "([a-j][1-9]|[a-j]10)"

}
