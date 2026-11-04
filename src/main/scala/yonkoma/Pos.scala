package strategygames.yonkoma

import scala.math.{ abs, max, min }

import strategygames.Piotr

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

  def piotr: Char = Pos.piotrs(index)
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

  def keyToPiotr(key: String) = fromKey(key) map (_.piotr)

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

  // the shared table, keyed by square name, so lila decodes every square the same way
  private lazy val piotrs: Array[Char] = Piotr.table(all.map(_.key))

  lazy val allPiotrs: Map[Char, Pos] = all.map(pos => pos.piotr -> pos).to(Map)

  // a file letter followed by a rank number, ranks 1 to 12
  val posR = "([a-l](?:1[0-2]|[1-9]))"

}
