package strategygames.yonkoma

import scala.math.{ abs, max, min }

// Each square gets the character that lila's shared table (ui/stratutils/src/piotr.ts) gives
// its key: a1 to j10 plus k1, l1, k2 and l2 are in that table, and the remaining squares take
// the go package's characters for the same keys (k3 to l10, and ranks 11 and 12 numbered on
// from 260 as go numbers them), which lila does not decode yet. Only the analysis board reads
// these characters, and yonkoma has no analysis board, but a square must never move.
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
    Pos.I1.index  -> '[',
    Pos.J1.index  -> '\\',
    Pos.K1.index  -> '\u00A9',
    Pos.L1.index  -> '\u00AA',
    Pos.A2.index  -> 'i',
    Pos.B2.index  -> 'j',
    Pos.C2.index  -> 'k',
    Pos.D2.index  -> 'l',
    Pos.E2.index  -> 'm',
    Pos.F2.index  -> 'n',
    Pos.G2.index  -> 'o',
    Pos.H2.index  -> 'p',
    Pos.I2.index  -> ']',
    Pos.J2.index  -> '^',
    Pos.K2.index  -> '\u00B2',
    Pos.L2.index  -> '\u00B3',
    Pos.A3.index  -> 'q',
    Pos.B3.index  -> 'r',
    Pos.C3.index  -> 's',
    Pos.D3.index  -> 't',
    Pos.E3.index  -> 'u',
    Pos.F3.index  -> 'v',
    Pos.G3.index  -> 'w',
    Pos.H3.index  -> 'x',
    Pos.I3.index  -> '_',
    Pos.J3.index  -> '`',
    Pos.K3.index  -> '\u00BC',
    Pos.L3.index  -> '\u00BD',
    Pos.A4.index  -> 'y',
    Pos.B4.index  -> 'z',
    Pos.C4.index  -> 'A',
    Pos.D4.index  -> 'B',
    Pos.E4.index  -> 'C',
    Pos.F4.index  -> 'D',
    Pos.G4.index  -> 'E',
    Pos.H4.index  -> 'F',
    Pos.I4.index  -> '{',
    Pos.J4.index  -> '|',
    Pos.K4.index  -> '\u00C5',
    Pos.L4.index  -> '\u00C6',
    Pos.A5.index  -> 'G',
    Pos.B5.index  -> 'H',
    Pos.C5.index  -> 'I',
    Pos.D5.index  -> 'J',
    Pos.E5.index  -> 'K',
    Pos.F5.index  -> 'L',
    Pos.G5.index  -> 'M',
    Pos.H5.index  -> 'N',
    Pos.I5.index  -> '}',
    Pos.J5.index  -> '~',
    Pos.K5.index  -> '\u00CE',
    Pos.L5.index  -> '\u00CF',
    Pos.A6.index  -> 'O',
    Pos.B6.index  -> 'P',
    Pos.C6.index  -> 'Q',
    Pos.D6.index  -> 'R',
    Pos.E6.index  -> 'S',
    Pos.F6.index  -> 'T',
    Pos.G6.index  -> 'U',
    Pos.H6.index  -> 'V',
    Pos.I6.index  -> '\u00A1',
    Pos.J6.index  -> '\u00A2',
    Pos.K6.index  -> '\u00D7',
    Pos.L6.index  -> '\u00D8',
    Pos.A7.index  -> 'W',
    Pos.B7.index  -> 'X',
    Pos.C7.index  -> 'Y',
    Pos.D7.index  -> 'Z',
    Pos.E7.index  -> '0',
    Pos.F7.index  -> '1',
    Pos.G7.index  -> '2',
    Pos.H7.index  -> '3',
    Pos.I7.index  -> '\u00A3',
    Pos.J7.index  -> '\u00A4',
    Pos.K7.index  -> '\u00E0',
    Pos.L7.index  -> '\u00E1',
    Pos.A8.index  -> '4',
    Pos.B8.index  -> '5',
    Pos.C8.index  -> '6',
    Pos.D8.index  -> '7',
    Pos.E8.index  -> '8',
    Pos.F8.index  -> '9',
    Pos.G8.index  -> '!',
    Pos.H8.index  -> '?',
    Pos.I8.index  -> '\u00A5',
    Pos.J8.index  -> '\u00A6',
    Pos.K8.index  -> '\u00E9',
    Pos.L8.index  -> '\u00EA',
    Pos.A9.index  -> '"',
    Pos.B9.index  -> '#',
    Pos.C9.index  -> '$',
    Pos.D9.index  -> '%',
    Pos.E9.index  -> '&',
    Pos.F9.index  -> '\'',
    Pos.G9.index  -> '(',
    Pos.H9.index  -> ')',
    Pos.I9.index  -> '*',
    Pos.J9.index  -> '+',
    Pos.K9.index  -> '\u00F2',
    Pos.L9.index  -> '\u00F3',
    Pos.A10.index -> '\u00A7',
    Pos.B10.index -> '-',
    Pos.C10.index -> '.',
    Pos.D10.index -> '/',
    Pos.E10.index -> ':',
    Pos.F10.index -> '\u00A8',
    Pos.G10.index -> '<',
    Pos.H10.index -> '=',
    Pos.I10.index -> '>',
    Pos.J10.index -> '@',
    Pos.K10.index -> '\u00FB',
    Pos.L10.index -> '\u00FC',
    Pos.A11.index -> 260.toChar,
    Pos.B11.index -> 261.toChar,
    Pos.C11.index -> 262.toChar,
    Pos.D11.index -> 263.toChar,
    Pos.E11.index -> 264.toChar,
    Pos.F11.index -> 265.toChar,
    Pos.G11.index -> 266.toChar,
    Pos.H11.index -> 267.toChar,
    Pos.I11.index -> 268.toChar,
    Pos.J11.index -> 269.toChar,
    Pos.K11.index -> 270.toChar,
    Pos.L11.index -> 271.toChar,
    Pos.A12.index -> 279.toChar,
    Pos.B12.index -> 280.toChar,
    Pos.C12.index -> 281.toChar,
    Pos.D12.index -> 282.toChar,
    Pos.E12.index -> 283.toChar,
    Pos.F12.index -> 284.toChar,
    Pos.G12.index -> 285.toChar,
    Pos.H12.index -> 286.toChar,
    Pos.I12.index -> 287.toChar,
    Pos.J12.index -> 288.toChar,
    Pos.K12.index -> 289.toChar,
    Pos.L12.index -> 290.toChar
  )
}

case class Pos private (index: Int) extends AnyVal {

  def down: Option[Pos]      = Pos.at(file.index, rank.index - 1)
  def left: Option[Pos]      = Pos.at(file.index - 1, rank.index)
  def up: Option[Pos]        = Pos.at(file.index, rank.index + 1)
  def right: Option[Pos]     = Pos.at(file.index + 1, rank.index)
  def upLeft: Option[Pos]    = Pos.at(file.index - 1, rank.index + 1)
  def upRight: Option[Pos]   = Pos.at(file.index + 1, rank.index + 1)
  def downLeft: Option[Pos]  = Pos.at(file.index - 1, rank.index - 1)
  def downRight: Option[Pos] = Pos.at(file.index + 1, rank.index - 1)

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

  def onSameDiagonal(other: Pos): Boolean = other != this && xDist(other) == yDist(other)

  def xDist(other: Pos) = abs(file - other.file)
  def yDist(other: Pos) = abs(rank - other.rank)

  // the number of king moves between two squares
  def kingDistance(other: Pos): Int = max(xDist(other), yDist(other))

  // a1 is dark, as on a checkerboard
  def isLight: Boolean = (file.index + rank.index) % 2 == 1

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
  val K1  = new Pos(10)
  val L1  = new Pos(11)
  val A2  = new Pos(12)
  val B2  = new Pos(13)
  val C2  = new Pos(14)
  val D2  = new Pos(15)
  val E2  = new Pos(16)
  val F2  = new Pos(17)
  val G2  = new Pos(18)
  val H2  = new Pos(19)
  val I2  = new Pos(20)
  val J2  = new Pos(21)
  val K2  = new Pos(22)
  val L2  = new Pos(23)
  val A3  = new Pos(24)
  val B3  = new Pos(25)
  val C3  = new Pos(26)
  val D3  = new Pos(27)
  val E3  = new Pos(28)
  val F3  = new Pos(29)
  val G3  = new Pos(30)
  val H3  = new Pos(31)
  val I3  = new Pos(32)
  val J3  = new Pos(33)
  val K3  = new Pos(34)
  val L3  = new Pos(35)
  val A4  = new Pos(36)
  val B4  = new Pos(37)
  val C4  = new Pos(38)
  val D4  = new Pos(39)
  val E4  = new Pos(40)
  val F4  = new Pos(41)
  val G4  = new Pos(42)
  val H4  = new Pos(43)
  val I4  = new Pos(44)
  val J4  = new Pos(45)
  val K4  = new Pos(46)
  val L4  = new Pos(47)
  val A5  = new Pos(48)
  val B5  = new Pos(49)
  val C5  = new Pos(50)
  val D5  = new Pos(51)
  val E5  = new Pos(52)
  val F5  = new Pos(53)
  val G5  = new Pos(54)
  val H5  = new Pos(55)
  val I5  = new Pos(56)
  val J5  = new Pos(57)
  val K5  = new Pos(58)
  val L5  = new Pos(59)
  val A6  = new Pos(60)
  val B6  = new Pos(61)
  val C6  = new Pos(62)
  val D6  = new Pos(63)
  val E6  = new Pos(64)
  val F6  = new Pos(65)
  val G6  = new Pos(66)
  val H6  = new Pos(67)
  val I6  = new Pos(68)
  val J6  = new Pos(69)
  val K6  = new Pos(70)
  val L6  = new Pos(71)
  val A7  = new Pos(72)
  val B7  = new Pos(73)
  val C7  = new Pos(74)
  val D7  = new Pos(75)
  val E7  = new Pos(76)
  val F7  = new Pos(77)
  val G7  = new Pos(78)
  val H7  = new Pos(79)
  val I7  = new Pos(80)
  val J7  = new Pos(81)
  val K7  = new Pos(82)
  val L7  = new Pos(83)
  val A8  = new Pos(84)
  val B8  = new Pos(85)
  val C8  = new Pos(86)
  val D8  = new Pos(87)
  val E8  = new Pos(88)
  val F8  = new Pos(89)
  val G8  = new Pos(90)
  val H8  = new Pos(91)
  val I8  = new Pos(92)
  val J8  = new Pos(93)
  val K8  = new Pos(94)
  val L8  = new Pos(95)
  val A9  = new Pos(96)
  val B9  = new Pos(97)
  val C9  = new Pos(98)
  val D9  = new Pos(99)
  val E9  = new Pos(100)
  val F9  = new Pos(101)
  val G9  = new Pos(102)
  val H9  = new Pos(103)
  val I9  = new Pos(104)
  val J9  = new Pos(105)
  val K9  = new Pos(106)
  val L9  = new Pos(107)
  val A10 = new Pos(108)
  val B10 = new Pos(109)
  val C10 = new Pos(110)
  val D10 = new Pos(111)
  val E10 = new Pos(112)
  val F10 = new Pos(113)
  val G10 = new Pos(114)
  val H10 = new Pos(115)
  val I10 = new Pos(116)
  val J10 = new Pos(117)
  val K10 = new Pos(118)
  val L10 = new Pos(119)
  val A11 = new Pos(120)
  val B11 = new Pos(121)
  val C11 = new Pos(122)
  val D11 = new Pos(123)
  val E11 = new Pos(124)
  val F11 = new Pos(125)
  val G11 = new Pos(126)
  val H11 = new Pos(127)
  val I11 = new Pos(128)
  val J11 = new Pos(129)
  val K11 = new Pos(130)
  val L11 = new Pos(131)
  val A12 = new Pos(132)
  val B12 = new Pos(133)
  val C12 = new Pos(134)
  val D12 = new Pos(135)
  val E12 = new Pos(136)
  val F12 = new Pos(137)
  val G12 = new Pos(138)
  val H12 = new Pos(139)
  val I12 = new Pos(140)
  val J12 = new Pos(141)
  val K12 = new Pos(142)
  val L12 = new Pos(143)

  // in a1-to-l12 sweep order
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

  // a file letter followed by a rank number, ranks 1 to 12
  val posR = "([a-l](?:1[0-2]|[1-9]))"

}
