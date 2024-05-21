package strategygames.go

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
    Pos.J8.index  -> '¦',
    // NOTE: "§" and "¨" are used above
    Pos.K1.index  -> '\u00A9',
    Pos.L1.index  -> '\u00AA',
    Pos.M1.index  -> '\u00AB',
    Pos.N1.index  -> '\u00AC',
    Pos.O1.index  -> '\u00AD',
    Pos.P1.index  -> '\u00AE',
    Pos.Q1.index  -> '\u00AF',
    Pos.R1.index  -> '\u00B0',
    Pos.S1.index  -> '\u00B1',
    Pos.K2.index  -> '\u00B2',
    Pos.L2.index  -> '\u00B3',
    Pos.M2.index  -> '\u00B4',
    Pos.N2.index  -> '\u00B5',
    Pos.O2.index  -> '\u00B6',
    Pos.P2.index  -> '\u00B7',
    Pos.Q2.index  -> '\u00B8',
    Pos.R2.index  -> '\u00B9',
    Pos.S2.index  -> '\u00BB',
    Pos.K3.index  -> '\u00BC',
    Pos.L3.index  -> '\u00BD',
    Pos.M3.index  -> '\u00BE',
    Pos.N3.index  -> '\u00BF',
    Pos.O3.index  -> '\u00C0',
    Pos.P3.index  -> '\u00C1',
    Pos.Q3.index  -> '\u00C2',
    Pos.R3.index  -> '\u00C3',
    Pos.S3.index  -> '\u00C4',
    Pos.K4.index  -> '\u00C5',
    Pos.L4.index  -> '\u00C6',
    Pos.M4.index  -> '\u00C7',
    Pos.N4.index  -> '\u00C8',
    Pos.O4.index  -> '\u00C9',
    Pos.P4.index  -> '\u00CA',
    Pos.Q4.index  -> '\u00CB',
    Pos.R4.index  -> '\u00CC',
    Pos.S4.index  -> '\u00CD',
    Pos.K5.index  -> '\u00CE',
    Pos.L5.index  -> '\u00CF',
    Pos.M5.index  -> '\u00D0',
    Pos.N5.index  -> '\u00D1',
    Pos.O5.index  -> '\u00D2',
    Pos.P5.index  -> '\u00D3',
    Pos.Q5.index  -> '\u00D4',
    Pos.R5.index  -> '\u00D5',
    Pos.S5.index  -> '\u00D6',
    Pos.K6.index  -> '\u00D7',
    Pos.L6.index  -> '\u00D8',
    Pos.M6.index  -> '\u00D9',
    Pos.N6.index  -> '\u00DA',
    Pos.O6.index  -> '\u00DB',
    Pos.P6.index  -> '\u00DC',
    Pos.Q6.index  -> '\u00DD',
    Pos.R6.index  -> '\u00DE',
    Pos.S6.index  -> '\u00DF',
    Pos.K7.index  -> '\u00E0',
    Pos.L7.index  -> '\u00E1',
    Pos.M7.index  -> '\u00E2',
    Pos.N7.index  -> '\u00E3',
    Pos.O7.index  -> '\u00E4',
    Pos.P7.index  -> '\u00E5',
    Pos.Q7.index  -> '\u00E6',
    Pos.R7.index  -> '\u00E7',
    Pos.S7.index  -> '\u00E8',
    Pos.K8.index  -> '\u00E9',
    Pos.L8.index  -> '\u00EA',
    Pos.M8.index  -> '\u00EB',
    Pos.N8.index  -> '\u00EC',
    Pos.O8.index  -> '\u00ED',
    Pos.P8.index  -> '\u00EE',
    Pos.Q8.index  -> '\u00EF',
    Pos.R8.index  -> '\u00F0',
    Pos.S8.index  -> '\u00F1',
    Pos.K9.index  -> '\u00F2',
    Pos.L9.index  -> '\u00F3',
    Pos.M9.index  -> '\u00F4',
    Pos.N9.index  -> '\u00F5',
    Pos.O9.index  -> '\u00F6',
    Pos.P9.index  -> '\u00F7',
    Pos.Q9.index  -> '\u00F8',
    Pos.R9.index  -> '\u00F9',
    Pos.S9.index  -> '\u00FA',
    Pos.K10.index -> '\u00FB',
    Pos.L10.index -> '\u00FC',
    Pos.M10.index -> '\u00FD',
    Pos.N10.index -> '\u00FE',
    Pos.O10.index -> '\u00FF',
    Pos.P10.index -> '\u0100',
    Pos.Q10.index -> '\u0101',
    Pos.R10.index -> '\u0102',
    Pos.S10.index -> '\u0103',
    190           -> 260.toChar,
    191           -> 261.toChar,
    192           -> 262.toChar,
    193           -> 263.toChar,
    194           -> 264.toChar,
    195           -> 265.toChar,
    196           -> 266.toChar,
    197           -> 267.toChar,
    198           -> 268.toChar,
    199           -> 269.toChar,
    200           -> 270.toChar,
    201           -> 271.toChar,
    202           -> 272.toChar,
    203           -> 273.toChar,
    204           -> 274.toChar,
    205           -> 275.toChar,
    206           -> 276.toChar,
    207           -> 277.toChar,
    208           -> 278.toChar,
    209           -> 279.toChar,
    210           -> 280.toChar,
    211           -> 281.toChar,
    212           -> 282.toChar,
    213           -> 283.toChar,
    214           -> 284.toChar,
    215           -> 285.toChar,
    216           -> 286.toChar,
    217           -> 287.toChar,
    218           -> 288.toChar,
    219           -> 289.toChar,
    220           -> 290.toChar,
    221           -> 291.toChar,
    222           -> 292.toChar,
    223           -> 293.toChar,
    224           -> 294.toChar,
    225           -> 295.toChar,
    226           -> 296.toChar,
    227           -> 297.toChar,
    228           -> 298.toChar,
    229           -> 299.toChar,
    230           -> 300.toChar,
    231           -> 301.toChar,
    232           -> 302.toChar,
    233           -> 303.toChar,
    234           -> 304.toChar,
    235           -> 305.toChar,
    236           -> 306.toChar,
    237           -> 307.toChar,
    238           -> 308.toChar,
    239           -> 309.toChar,
    240           -> 310.toChar,
    241           -> 311.toChar,
    242           -> 312.toChar,
    243           -> 313.toChar,
    244           -> 314.toChar,
    245           -> 315.toChar,
    246           -> 316.toChar,
    247           -> 317.toChar,
    248           -> 318.toChar,
    249           -> 319.toChar,
    250           -> 320.toChar,
    251           -> 321.toChar,
    252           -> 322.toChar,
    253           -> 323.toChar,
    254           -> 324.toChar,
    255           -> 325.toChar,
    256           -> 326.toChar,
    257           -> 327.toChar,
    258           -> 328.toChar,
    // 329 is deprecated
    259           -> 330.toChar,
    260           -> 331.toChar,
    261           -> 332.toChar,
    262           -> 333.toChar,
    263           -> 334.toChar,
    264           -> 335.toChar,
    265           -> 336.toChar,
    266           -> 337.toChar,
    267           -> 338.toChar,
    268           -> 339.toChar,
    269           -> 340.toChar,
    270           -> 341.toChar,
    271           -> 342.toChar,
    272           -> 343.toChar,
    273           -> 344.toChar,
    274           -> 345.toChar,
    275           -> 346.toChar,
    276           -> 347.toChar,
    277           -> 348.toChar,
    278           -> 349.toChar,
    279           -> 350.toChar,
    280           -> 351.toChar,
    281           -> 352.toChar,
    282           -> 353.toChar,
    283           -> 354.toChar,
    284           -> 355.toChar,
    285           -> 356.toChar,
    286           -> 357.toChar,
    287           -> 358.toChar,
    288           -> 359.toChar,
    289           -> 360.toChar,
    290           -> 361.toChar,
    291           -> 362.toChar,
    292           -> 363.toChar,
    293           -> 364.toChar,
    294           -> 365.toChar,
    295           -> 366.toChar,
    296           -> 367.toChar,
    297           -> 368.toChar,
    298           -> 369.toChar,
    299           -> 370.toChar,
    300           -> 371.toChar,
    301           -> 372.toChar,
    302           -> 373.toChar,
    303           -> 374.toChar,
    304           -> 375.toChar,
    305           -> 376.toChar,
    306           -> 377.toChar,
    307           -> 378.toChar,
    308           -> 379.toChar,
    309           -> 380.toChar,
    310           -> 381.toChar,
    311           -> 382.toChar,
    312           -> 383.toChar,
    313           -> 384.toChar,
    314           -> 385.toChar,
    315           -> 386.toChar,
    316           -> 387.toChar,
    317           -> 388.toChar,
    318           -> 389.toChar,
    319           -> 390.toChar,
    320           -> 391.toChar,
    321           -> 392.toChar,
    322           -> 393.toChar,
    323           -> 394.toChar,
    324           -> 395.toChar,
    325           -> 396.toChar,
    326           -> 397.toChar,
    327           -> 398.toChar,
    328           -> 399.toChar,
    329           -> 400.toChar,
    330           -> 401.toChar,
    331           -> 402.toChar,
    332           -> 403.toChar,
    333           -> 404.toChar,
    334           -> 405.toChar,
    335           -> 406.toChar,
    336           -> 407.toChar,
    337           -> 408.toChar,
    338           -> 409.toChar,
    339           -> 410.toChar,
    340           -> 411.toChar,
    341           -> 412.toChar,
    342           -> 413.toChar,
    343           -> 414.toChar,
    344           -> 415.toChar,
    345           -> 416.toChar,
    346           -> 417.toChar,
    347           -> 418.toChar,
    348           -> 419.toChar,
    349           -> 420.toChar,
    350           -> 421.toChar,
    351           -> 422.toChar,
    352           -> 423.toChar,
    353           -> 424.toChar,
    354           -> 425.toChar,
    355           -> 426.toChar,
    356           -> 427.toChar,
    357           -> 428.toChar,
    358           -> 429.toChar,
    359           -> 430.toChar,
    360           -> 431.toChar
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
  val M1  = new Pos(12)
  val N1  = new Pos(13)
  val O1  = new Pos(14)
  val P1  = new Pos(15)
  val Q1  = new Pos(16)
  val R1  = new Pos(17)
  val S1  = new Pos(18)
  val A2  = new Pos(19)
  val B2  = new Pos(20)
  val C2  = new Pos(21)
  val D2  = new Pos(22)
  val E2  = new Pos(23)
  val F2  = new Pos(24)
  val G2  = new Pos(25)
  val H2  = new Pos(26)
  val I2  = new Pos(27)
  val J2  = new Pos(28)
  val K2  = new Pos(29)
  val L2  = new Pos(30)
  val M2  = new Pos(31)
  val N2  = new Pos(32)
  val O2  = new Pos(33)
  val P2  = new Pos(34)
  val Q2  = new Pos(35)
  val R2  = new Pos(36)
  val S2  = new Pos(37)
  val A3  = new Pos(38)
  val B3  = new Pos(39)
  val C3  = new Pos(40)
  val D3  = new Pos(41)
  val E3  = new Pos(42)
  val F3  = new Pos(43)
  val G3  = new Pos(44)
  val H3  = new Pos(45)
  val I3  = new Pos(46)
  val J3  = new Pos(47)
  val K3  = new Pos(48)
  val L3  = new Pos(49)
  val M3  = new Pos(50)
  val N3  = new Pos(51)
  val O3  = new Pos(52)
  val P3  = new Pos(53)
  val Q3  = new Pos(54)
  val R3  = new Pos(55)
  val S3  = new Pos(56)
  val A4  = new Pos(57)
  val B4  = new Pos(58)
  val C4  = new Pos(59)
  val D4  = new Pos(60)
  val E4  = new Pos(61)
  val F4  = new Pos(62)
  val G4  = new Pos(63)
  val H4  = new Pos(64)
  val I4  = new Pos(65)
  val J4  = new Pos(66)
  val K4  = new Pos(67)
  val L4  = new Pos(68)
  val M4  = new Pos(69)
  val N4  = new Pos(70)
  val O4  = new Pos(71)
  val P4  = new Pos(72)
  val Q4  = new Pos(73)
  val R4  = new Pos(74)
  val S4  = new Pos(75)
  val A5  = new Pos(76)
  val B5  = new Pos(77)
  val C5  = new Pos(78)
  val D5  = new Pos(79)
  val E5  = new Pos(80)
  val F5  = new Pos(81)
  val G5  = new Pos(82)
  val H5  = new Pos(83)
  val I5  = new Pos(84)
  val J5  = new Pos(85)
  val K5  = new Pos(86)
  val L5  = new Pos(87)
  val M5  = new Pos(88)
  val N5  = new Pos(89)
  val O5  = new Pos(90)
  val P5  = new Pos(91)
  val Q5  = new Pos(92)
  val R5  = new Pos(93)
  val S5  = new Pos(94)
  val A6  = new Pos(95)
  val B6  = new Pos(96)
  val C6  = new Pos(97)
  val D6  = new Pos(98)
  val E6  = new Pos(99)
  val F6  = new Pos(100)
  val G6  = new Pos(101)
  val H6  = new Pos(102)
  val I6  = new Pos(103)
  val J6  = new Pos(104)
  val K6  = new Pos(105)
  val L6  = new Pos(106)
  val M6  = new Pos(107)
  val N6  = new Pos(108)
  val O6  = new Pos(109)
  val P6  = new Pos(110)
  val Q6  = new Pos(111)
  val R6  = new Pos(112)
  val S6  = new Pos(113)
  val A7  = new Pos(114)
  val B7  = new Pos(115)
  val C7  = new Pos(116)
  val D7  = new Pos(117)
  val E7  = new Pos(118)
  val F7  = new Pos(119)
  val G7  = new Pos(120)
  val H7  = new Pos(121)
  val I7  = new Pos(122)
  val J7  = new Pos(123)
  val K7  = new Pos(124)
  val L7  = new Pos(125)
  val M7  = new Pos(126)
  val N7  = new Pos(127)
  val O7  = new Pos(128)
  val P7  = new Pos(129)
  val Q7  = new Pos(130)
  val R7  = new Pos(131)
  val S7  = new Pos(132)
  val A8  = new Pos(133)
  val B8  = new Pos(134)
  val C8  = new Pos(135)
  val D8  = new Pos(136)
  val E8  = new Pos(137)
  val F8  = new Pos(138)
  val G8  = new Pos(139)
  val H8  = new Pos(140)
  val I8  = new Pos(141)
  val J8  = new Pos(142)
  val K8  = new Pos(143)
  val L8  = new Pos(144)
  val M8  = new Pos(145)
  val N8  = new Pos(146)
  val O8  = new Pos(147)
  val P8  = new Pos(148)
  val Q8  = new Pos(149)
  val R8  = new Pos(150)
  val S8  = new Pos(151)
  val A9  = new Pos(152)
  val B9  = new Pos(153)
  val C9  = new Pos(154)
  val D9  = new Pos(155)
  val E9  = new Pos(156)
  val F9  = new Pos(157)
  val G9  = new Pos(158)
  val H9  = new Pos(159)
  val I9  = new Pos(160)
  val J9  = new Pos(161)
  val K9  = new Pos(162)
  val L9  = new Pos(163)
  val M9  = new Pos(164)
  val N9  = new Pos(165)
  val O9  = new Pos(166)
  val P9  = new Pos(167)
  val Q9  = new Pos(168)
  val R9  = new Pos(169)
  val S9  = new Pos(170)
  val A10 = new Pos(171)
  val B10 = new Pos(172)
  val C10 = new Pos(173)
  val D10 = new Pos(174)
  val E10 = new Pos(175)
  val F10 = new Pos(176)
  val G10 = new Pos(177)
  val H10 = new Pos(178)
  val I10 = new Pos(179)
  val J10 = new Pos(180)
  val K10 = new Pos(181)
  val L10 = new Pos(182)
  val M10 = new Pos(183)
  val N10 = new Pos(184)
  val O10 = new Pos(185)
  val P10 = new Pos(186)
  val Q10 = new Pos(187)
  val R10 = new Pos(188)
  val S10 = new Pos(189)
  val A11 = new Pos(190)
  val B11 = new Pos(191)
  val C11 = new Pos(192)
  val D11 = new Pos(193)
  val E11 = new Pos(194)
  val F11 = new Pos(195)
  val G11 = new Pos(196)
  val H11 = new Pos(197)
  val I11 = new Pos(198)
  val J11 = new Pos(199)
  val K11 = new Pos(200)
  val L11 = new Pos(201)
  val M11 = new Pos(202)
  val N11 = new Pos(203)
  val O11 = new Pos(204)
  val P11 = new Pos(205)
  val Q11 = new Pos(206)
  val R11 = new Pos(207)
  val S11 = new Pos(208)
  val A12 = new Pos(209)
  val B12 = new Pos(210)
  val C12 = new Pos(211)
  val D12 = new Pos(212)
  val E12 = new Pos(213)
  val F12 = new Pos(214)
  val G12 = new Pos(215)
  val H12 = new Pos(216)
  val I12 = new Pos(217)
  val J12 = new Pos(218)
  val K12 = new Pos(219)
  val L12 = new Pos(220)
  val M12 = new Pos(221)
  val N12 = new Pos(222)
  val O12 = new Pos(223)
  val P12 = new Pos(224)
  val Q12 = new Pos(225)
  val R12 = new Pos(226)
  val S12 = new Pos(227)
  val A13 = new Pos(228)
  val B13 = new Pos(229)
  val C13 = new Pos(230)
  val D13 = new Pos(231)
  val E13 = new Pos(232)
  val F13 = new Pos(233)
  val G13 = new Pos(234)
  val H13 = new Pos(235)
  val I13 = new Pos(236)
  val J13 = new Pos(237)
  val K13 = new Pos(238)
  val L13 = new Pos(239)
  val M13 = new Pos(240)
  val N13 = new Pos(241)
  val O13 = new Pos(242)
  val P13 = new Pos(243)
  val Q13 = new Pos(244)
  val R13 = new Pos(245)
  val S13 = new Pos(246)
  val A14 = new Pos(247)
  val B14 = new Pos(248)
  val C14 = new Pos(249)
  val D14 = new Pos(250)
  val E14 = new Pos(251)
  val F14 = new Pos(252)
  val G14 = new Pos(253)
  val H14 = new Pos(254)
  val I14 = new Pos(255)
  val J14 = new Pos(256)
  val K14 = new Pos(257)
  val L14 = new Pos(258)
  val M14 = new Pos(259)
  val N14 = new Pos(260)
  val O14 = new Pos(261)
  val P14 = new Pos(262)
  val Q14 = new Pos(263)
  val R14 = new Pos(264)
  val S14 = new Pos(265)
  val A15 = new Pos(266)
  val B15 = new Pos(267)
  val C15 = new Pos(268)
  val D15 = new Pos(269)
  val E15 = new Pos(270)
  val F15 = new Pos(271)
  val G15 = new Pos(272)
  val H15 = new Pos(273)
  val I15 = new Pos(274)
  val J15 = new Pos(275)
  val K15 = new Pos(276)
  val L15 = new Pos(277)
  val M15 = new Pos(278)
  val N15 = new Pos(279)
  val O15 = new Pos(280)
  val P15 = new Pos(281)
  val Q15 = new Pos(282)
  val R15 = new Pos(283)
  val S15 = new Pos(284)
  val A16 = new Pos(285)
  val B16 = new Pos(286)
  val C16 = new Pos(287)
  val D16 = new Pos(288)
  val E16 = new Pos(289)
  val F16 = new Pos(290)
  val G16 = new Pos(291)
  val H16 = new Pos(292)
  val I16 = new Pos(293)
  val J16 = new Pos(294)
  val K16 = new Pos(295)
  val L16 = new Pos(296)
  val M16 = new Pos(297)
  val N16 = new Pos(298)
  val O16 = new Pos(299)
  val P16 = new Pos(300)
  val Q16 = new Pos(301)
  val R16 = new Pos(302)
  val S16 = new Pos(303)
  val A17 = new Pos(304)
  val B17 = new Pos(305)
  val C17 = new Pos(306)
  val D17 = new Pos(307)
  val E17 = new Pos(308)
  val F17 = new Pos(309)
  val G17 = new Pos(310)
  val H17 = new Pos(311)
  val I17 = new Pos(312)
  val J17 = new Pos(313)
  val K17 = new Pos(314)
  val L17 = new Pos(315)
  val M17 = new Pos(316)
  val N17 = new Pos(317)
  val O17 = new Pos(318)
  val P17 = new Pos(319)
  val Q17 = new Pos(320)
  val R17 = new Pos(321)
  val S17 = new Pos(322)
  val A18 = new Pos(323)
  val B18 = new Pos(324)
  val C18 = new Pos(325)
  val D18 = new Pos(326)
  val E18 = new Pos(327)
  val F18 = new Pos(328)
  val G18 = new Pos(329)
  val H18 = new Pos(330)
  val I18 = new Pos(331)
  val J18 = new Pos(332)
  val K18 = new Pos(333)
  val L18 = new Pos(334)
  val M18 = new Pos(335)
  val N18 = new Pos(336)
  val O18 = new Pos(337)
  val P18 = new Pos(338)
  val Q18 = new Pos(339)
  val R18 = new Pos(340)
  val S18 = new Pos(341)
  val A19 = new Pos(342)
  val B19 = new Pos(343)
  val C19 = new Pos(344)
  val D19 = new Pos(345)
  val E19 = new Pos(346)
  val F19 = new Pos(347)
  val G19 = new Pos(348)
  val H19 = new Pos(349)
  val I19 = new Pos(350)
  val J19 = new Pos(351)
  val K19 = new Pos(352)
  val L19 = new Pos(353)
  val M19 = new Pos(354)
  val N19 = new Pos(355)
  val O19 = new Pos(356)
  val P19 = new Pos(357)
  val Q19 = new Pos(358)
  val R19 = new Pos(359)
  val S19 = new Pos(360)

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

  val posR = "([a-z][1-9][0-9]?)" // "([a-s]1[0-9]|[a-s]0?[1-9])""

}
