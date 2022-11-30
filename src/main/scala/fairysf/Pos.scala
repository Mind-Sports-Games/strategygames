package strategygames.fairysf

import scala.math.{ abs, max, min }

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

  def piotr: Char =
    if (index <= 25) (97 + index).toChar // a ...
    else if (index <= 51) (39 + index).toChar // A ...
    else if (index <= 61) (index - 4).toChar // 0 ...
    else if (index == 62) '!'
    else '?'
  def piotrStr = piotr.toString

  def key               = file.toString + rank.toString
  override def toString = key
}

object Pos {
  def apply(index: Int): Option[Pos] =
    if (0 <= index && index < File.all.size * Rank.all.size) Some(new Pos(index))
    else None

  def apply(file: File, rank: Rank): Pos =
    if (file == File.J) {
      new Pos(File.formerAll.size * Rank.all.size + rank.index)
    } else {
      new Pos(file.index + File.formerAll.size * rank.index)
    }

  def at(x: Int, y: Int): Option[Pos] =
    if (0 <= x && x < File.formerAll.size && 0 <= y && y < Rank.all.size) {
      Some(new Pos(x + File.formerAll.size * y))
    } else if (x == (File.all.size - 1) && 0 <= y && y < Rank.all.size) {
      Some(new Pos(y + File.formerAll.size * Rank.all.size))
    } else None

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
  val A2  = new Pos(9)
  val B2  = new Pos(10)
  val C2  = new Pos(11)
  val D2  = new Pos(12)
  val E2  = new Pos(13)
  val F2  = new Pos(14)
  val G2  = new Pos(15)
  val H2  = new Pos(16)
  val I2  = new Pos(17)
  val A3  = new Pos(18)
  val B3  = new Pos(19)
  val C3  = new Pos(20)
  val D3  = new Pos(21)
  val E3  = new Pos(22)
  val F3  = new Pos(23)
  val G3  = new Pos(24)
  val H3  = new Pos(25)
  val I3  = new Pos(26)
  val A4  = new Pos(27)
  val B4  = new Pos(28)
  val C4  = new Pos(29)
  val D4  = new Pos(30)
  val E4  = new Pos(31)
  val F4  = new Pos(32)
  val G4  = new Pos(33)
  val H4  = new Pos(34)
  val I4  = new Pos(35)
  val A5  = new Pos(36)
  val B5  = new Pos(37)
  val C5  = new Pos(38)
  val D5  = new Pos(39)
  val E5  = new Pos(40)
  val F5  = new Pos(41)
  val G5  = new Pos(42)
  val H5  = new Pos(43)
  val I5  = new Pos(44)
  val A6  = new Pos(45)
  val B6  = new Pos(46)
  val C6  = new Pos(47)
  val D6  = new Pos(48)
  val E6  = new Pos(49)
  val F6  = new Pos(50)
  val G6  = new Pos(51)
  val H6  = new Pos(52)
  val I6  = new Pos(53)
  val A7  = new Pos(54)
  val B7  = new Pos(55)
  val C7  = new Pos(56)
  val D7  = new Pos(57)
  val E7  = new Pos(58)
  val F7  = new Pos(59)
  val G7  = new Pos(60)
  val H7  = new Pos(61)
  val I7  = new Pos(62)
  val A8  = new Pos(63)
  val B8  = new Pos(64)
  val C8  = new Pos(65)
  val D8  = new Pos(66)
  val E8  = new Pos(67)
  val F8  = new Pos(68)
  val G8  = new Pos(69)
  val H8  = new Pos(70)
  val I8  = new Pos(71)
  val A9  = new Pos(72)
  val B9  = new Pos(73)
  val C9  = new Pos(74)
  val D9  = new Pos(75)
  val E9  = new Pos(76)
  val F9  = new Pos(77)
  val G9  = new Pos(78)
  val H9  = new Pos(79)
  val I9  = new Pos(80)
  val A10 = new Pos(81)
  val B10 = new Pos(82)
  val C10 = new Pos(83)
  val D10 = new Pos(84)
  val E10 = new Pos(85)
  val F10 = new Pos(86)
  val G10 = new Pos(87)
  val H10 = new Pos(88)
  val I10 = new Pos(89)

  // due to binary storage cannot change old indexes without affecting the db...
  val J1  = new Pos(90)
  val J2  = new Pos(91)
  val J3  = new Pos(92)
  val J4  = new Pos(93)
  val J5  = new Pos(94)
  val J6  = new Pos(95)
  val J7  = new Pos(96)
  val J8  = new Pos(97)
  val J9  = new Pos(98)
  val J10 = new Pos(99)

  // current pos limit in db is 128, if adding more perhaps use a different method (map of index to file, rank)

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


  private val fairySquareToPos: Map[Int, Pos] = Map(
    0 -> A1, //SQ_A1 = 0
    1 -> B1, //SQ_B1 = 0
    2 -> C1, //SQ_C1 = 0
    3 -> D1, //SQ_D1 = 0
    4 -> E1, //SQ_E1 = 0
    5 -> F1, //SQ_F1 = 0
    6 -> G1, //SQ_G1 = 0
    7 -> H1, //SQ_H1 = 0
    8 -> I1, //SQ_I1 = 0
    9 -> J1, //SQ_J1 = 0
    //10 -> K1, //SQ_K1 = 0
    //11 -> L1, //SQ_L1,
    12 -> A2, //SQ_A2 = 0
    13 -> B2, //SQ_B2 = 0
    14 -> C2, //SQ_C2 = 0
    15 -> D2, //SQ_D2 = 0
    16 -> E2, //SQ_E2 = 0
    17 -> F2, //SQ_F2 = 0
    18 -> G2, //SQ_G2 = 0
    19 -> H2, //SQ_H2 = 0
    20 -> I2, //SQ_I2 = 0
    21 -> J2, //SQ_J2 = 0
    //22 -> K2, //SQ_K2 = 0
    //23 -> L2, //SQ_L2,
    24 -> A3, //SQ_A3 = 0
    25 -> B3, //SQ_B3 = 0
    26 -> C3, //SQ_C3 = 0
    27 -> D3, //SQ_D3 = 0
    28 -> E3, //SQ_E3 = 0
    29 -> F3, //SQ_F3 = 0
    30 -> G3, //SQ_G3 = 0
    31 -> H3, //SQ_H3 = 0
    32 -> I3, //SQ_I3 = 0
    33 -> J3, //SQ_J3 = 0
    //34 -> K3, //SQ_K3 = 0
    //35 -> L3, //SQ_L3,
    36 -> A4, //SQ_A4 = 0
    37 -> B4, //SQ_B4 = 0
    38 -> C4, //SQ_C4 = 0
    39 -> D4, //SQ_D4 = 0
    40 -> E4, //SQ_E4 = 0
    41 -> F4, //SQ_F4 = 0
    42 -> G4, //SQ_G4 = 0
    43 -> H4, //SQ_H4 = 0
    44 -> I4, //SQ_I4 = 0
    45 -> J4, //SQ_J4 = 0
    //46 -> K4, //SQ_K4 = 0
    //47 -> L4, //SQ_L4,
    48 -> A5, //SQ_A5 = 0
    49 -> B5, //SQ_B5 = 0
    50 -> C5, //SQ_C5 = 0
    51 -> D5, //SQ_D5 = 0
    52 -> E5, //SQ_E5 = 0
    53 -> F5, //SQ_F5 = 0
    54 -> G5, //SQ_G5 = 0
    55 -> H5, //SQ_H5 = 0
    56 -> I5, //SQ_I5 = 0
    57 -> J5, //SQ_J5 = 0
    //58 -> K5, //SQ_K5 = 0
    //59 -> L5, //SQ_L5,
    60 -> A6, //SQ_A6 = 0
    61 -> B6, //SQ_B6 = 0
    62 -> C6, //SQ_C6 = 0
    63 -> D6, //SQ_D6 = 0
    64 -> E6, //SQ_E6 = 0
    65 -> F6, //SQ_F6 = 0
    66 -> G6, //SQ_G6 = 0
    67 -> H6, //SQ_H6 = 0
    68 -> I6, //SQ_I6 = 0
    69 -> J6, //SQ_J6 = 0
    //70 -> K6, //SQ_K6 = 0
    //71 -> L6, //SQ_L6,
    72 -> A7, //SQ_A7 = 0
    73 -> B7, //SQ_B7 = 0
    74 -> C7, //SQ_C7 = 0
    75 -> D7, //SQ_D7 = 0
    76 -> E7, //SQ_E7 = 0
    77 -> F7, //SQ_F7 = 0
    78 -> G7, //SQ_G7 = 0
    79 -> H7, //SQ_H7 = 0
    80 -> I7, //SQ_I7 = 0
    81 -> J7, //SQ_J7 = 0
    //82 -> K7, //SQ_K7 = 0
    //83 -> L7, //SQ_L7,
    84 -> A8, //SQ_A8 = 0
    85 -> B8, //SQ_B8 = 0
    86 -> C8, //SQ_C8 = 0
    87 -> D8, //SQ_D8 = 0
    88 -> E8, //SQ_E8 = 0
    89 -> F8, //SQ_F8 = 0
    90 -> G8, //SQ_G8 = 0
    91 -> H8, //SQ_H8 = 0
    92 -> I8, //SQ_I8 = 0
    93 -> J8, //SQ_J8 = 0
    //94 -> K8, //SQ_K8 = 0
    //95 -> L8, //SQ_L8,
    96 -> A9, //SQ_A9 = 0
    97 -> B9, //SQ_B9 = 0
    98 -> C9, //SQ_C9 = 0
    99 -> D9, //SQ_D9 = 0
    100 -> E9, //SQ_E9 = 0
    101 -> F9, //SQ_F9 = 0
    102 -> G9, //SQ_G9 = 0
    103 -> H9, //SQ_H9 = 0
    104 -> I9, //SQ_I9 = 0
    105 -> J9, //SQ_J9 = 0
    //106 -> K9, //SQ_K9 = 0
    //107 -> L9, //SQ_L9,
    108 -> A10, //SQ_A10 = 0
    109 -> B10, //SQ_B10 = 0
    110 -> C10, //SQ_C10 = 0
    111 -> D10, //SQ_D10 = 0
    112 -> E10, //SQ_E10 = 0
    113 -> F10, //SQ_F10 = 0
    114 -> G10, //SQ_G10 = 0
    115 -> H10, //SQ_H10 = 0
    116 -> I10, //SQ_I10 = 0
    117 -> J10, //SQ_J10 = 0
    //118 -> K10, //SQ_K10 = 0
    //119 -> L10, //SQ_L10,
  )
  def fromFairy(i: Int): Option[Pos] = fairySquareToPos.get(i)


}
