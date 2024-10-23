package strategygames.abalone

/*
Extended Piotr representation :

9 -  \" #  $  %  &  \' (  )  *
8 -  4  5  6  7  8  9  !  ?  ¥
7 -  W  X  Y  Z  0  1  2  3  £
6 -  O  P  Q  R  S  T  U  V  ¡
5 -  G  H  I  J  K  L  M  N  }
4 -  y  z  A  B  C  D  E  F  {
3 -  q  r  s  t  u  v  w  x  _
2 -  i  j  k  l  m  n  o  p  ]
1 -  a  b  c  d  e  f  g  h  [
     |  |  |  |  |  |  |  |  |
     A  B  C  D  E  F  G  H  I

9 -  \" #  $  %  &  \' (  )  *
8 -   4  5  6  7  8  9  !  ?  ¥
7 -    W  X  Y  Z  0  1  2  3  £
6 -     O  P  Q  R  S  T  U  V  ¡
5 -      G  H  I  J  K  L  M  N  }
4 -       y  z  A  B  C  D  E  F  {
3 -        q  r  s  t  u  v  w  x  _
2 -         i  j  k  l  m  n  o  p  ]
1 -          a  b  c  d  e  f  g  h  [
              \  \  \  \  \  \  \  \  \
               A  B  C  D  E  F  G  H  I


Actual Piotr representation :

Seen as an hexagon
9 -              &  \' (  )  *
8 -            7  8  9  !  ?  ¥
7 -          Y  Z  0  1  2  3  £
6 -        P  Q  R  S  T  U  V  ¡
5 -      G  H  I  J  K  L  M  N  }
4 -       y  z  A  B  C  D  E  F
3 -        q  r  s  t  u  v  w
2 -         i  j  k  l  m  n
1 -          a  b  c  d  e
              \  \  \  \  \  \  \  \  \
               A  B  C  D  E  F  G  H  I

Seen as a square without the bottomright and topleft triangle
9 -              &  \' (  )  *
8 -           7  8  9  !  ?  ¥
7 -        Y  Z  0  1  2  3  £
6 -     P  Q  R  S  T  U  V  ¡
5 -  G  H  I  J  K  L  M  N  }
4 -  y  z  A  B  C  D  E  F
3 -  q  r  s  t  u  v  w
2 -  i  j  k  l  m  n
1 -  a  b  c  d  e
     |  |  |  |  |  |  |  |  |
     A  B  C  D  E  F  G  H  I


Abalone official coordinates system and "standard start position" are drawn below :
     I O O O O O
    H O O O O O O
   G + + O O O + +
  F + + + + + + + +
 E + + + + + + + + +
  D + + + + + + + + 9
   C + + @ @ @ + + 8
    B @ @ @ @ @ @ 7
     A @ @ @ @ @ 6
        1 2 3 4 5


!!! be mindful about the fact that when we write the FEN we start from topleft to bottom right

 */

// Piotr is what is saved in Database and is used in Uci
// In the lookup val, we target a "FileAsLetterRowAsNumber" square already defined in Pos object
// The .index refer to the value that was given when the Pos was created, and is then associated to the char (which should match src/main/scala/fairysf/Pos.scala for use in analysis and stratops)
object Piotr {
  val lookup: Map[Int, Char] = Map(
    Pos.A1.index -> 'a',
    Pos.B1.index -> 'b',
    Pos.C1.index -> 'c',
    Pos.D1.index -> 'd',
    Pos.E1.index -> 'e',
    // Pos.F1.index -> 'f', // does not need to exist
    // Pos.G1.index -> 'g',
    // Pos.H1.index -> 'h',
    Pos.A2.index -> 'i',  // Pos.A1.index = 9, which is associated to 'i' : Piotr.lookup(9) will return 'i'
    Pos.B2.index -> 'j',
    Pos.C2.index -> 'k',
    Pos.D2.index -> 'l',
    Pos.E2.index -> 'm',
    Pos.F2.index -> 'n',
    // Pos.G2.index -> 'o',
    // Pos.H2.index -> 'p',
    Pos.A3.index -> 'q',
    Pos.B3.index -> 'r',
    Pos.C3.index -> 's',
    Pos.D3.index -> 't',
    Pos.E3.index -> 'u',
    Pos.F3.index -> 'v',
    Pos.G3.index -> 'w',
    // Pos.H3.index -> 'x',
    Pos.A4.index -> 'y',
    Pos.B4.index -> 'z',
    Pos.C4.index -> 'A',
    Pos.D4.index -> 'B',
    Pos.E4.index -> 'C',
    Pos.F4.index -> 'D',
    Pos.G4.index -> 'E',
    Pos.H4.index -> 'F',
    Pos.A5.index -> 'G',
    Pos.B5.index -> 'H',
    Pos.C5.index -> 'I',
    Pos.D5.index -> 'J',
    Pos.E5.index -> 'K',
    Pos.F5.index -> 'L',
    Pos.G5.index -> 'M',
    Pos.H5.index -> 'N',
    // Pos.A6.index -> 'O',
    Pos.B6.index -> 'P',
    Pos.C6.index -> 'Q',
    Pos.D6.index -> 'R',
    Pos.E6.index -> 'S',
    Pos.F6.index -> 'T',
    Pos.G6.index -> 'U',
    Pos.H6.index -> 'V',
    // Pos.A7.index -> 'W',
    // Pos.B7.index -> 'X',
    Pos.C7.index -> 'Y',
    Pos.D7.index -> 'Z',
    Pos.E7.index -> '0',
    Pos.F7.index -> '1',
    Pos.G7.index -> '2',
    Pos.H7.index -> '3',
    // Pos.A8.index -> '4',
    // Pos.B8.index -> '5',
    // Pos.C8.index -> '6',
    Pos.D8.index -> '7',
    Pos.E8.index -> '8',
    Pos.F8.index -> '9',
    Pos.G8.index -> '!',
    Pos.H8.index -> '?',
    // Pos.A9.index -> '\"',
    // Pos.B9.index -> '#',
    // Pos.C9.index -> '$',
    // Pos.D9.index -> '%',
    Pos.E9.index -> '&',
    Pos.F9.index -> '\'',
    Pos.G9.index -> '(',
    Pos.H9.index -> ')',
    Pos.I9.index -> '*',
    // Pos.I1.index -> '[',
    // Pos.J1.index  -> '\\',
    // Pos.I2.index -> ']',
    // Pos.J2.index  -> '^',
    // Pos.I3.index -> '_',
    // Pos.J3.index  -> '`',
    // Pos.I4.index -> '{',
    // Pos.J4.index  -> '|',
    Pos.I5.index -> '}',
    // Pos.J5.index  -> '~',
    // https://en.wikipedia.org/wiki/List_of_Unicode_characters#Latin_script
    // from the latin-1 script
    Pos.I6.index -> '¡',
    // Pos.J6.index  -> '¢',
    Pos.I7.index -> '£',
    // Pos.J7.index  -> '¤',
    Pos.I8.index -> '¥'
    // Pos.J8.index  -> '¦'
  )
}

// Pos represents a square (by it's index (an Int))
// We can use this index to find the char associated with the Pos
// There are utility functions to move in a direction and do some checks, from an instance of Pos
case class Pos private (index: Int) extends AnyVal {

  /*
  hexagonal grids have 6 directions only
  \   /
  - o -
  /   \
  */
  def left: Option[Pos]      = Pos.at(file.index - 1, rank.index)
  def downLeft: Option[Pos]  = Pos.at(file.index - 1, rank.index - 1)
  def upLeft: Option[Pos]    = Pos.at(file.index, rank.index + 1)
  def right: Option[Pos]     = Pos.at(file.index + 1, rank.index)
  def downRight: Option[Pos] = Pos.at(file.index, rank.index - 1)
  def upRight: Option[Pos]   = Pos.at(file.index + 1, rank.index + 1)

  def neighbours: List[Option[Pos]] = List(left, upLeft, upRight, right, downRight, downLeft)
  def sideMovesDirsFromDir(dir: Option[String]): (Option[Pos], Option[Pos]) = dir match {
    case Some("left")       => (this.downLeft, this.upLeft)
    case Some("upLeft")     => (this.left, this.upRight)
    case Some("upRight")    => (this.upLeft, this.right)
    case Some("right")      => (this.upRight, this.downRight)
    case Some("downRight")  => (this.right, this.downLeft)
    case Some("downLeft")   => (this.downRight, this.left)
    case _                  => (None, None)
  }

  // NOTE - *neighbourhood
  // these below only work for neighbour pos but that's probably fine as in Abalone we only move to (potentially extended) neighbourhood
  def dir(dir: Option[String]): Option[Pos]  = dir match {
    case Some("left")       => this.left
    case Some("upLeft")     => this.upLeft
    case Some("upRight")    => this.upRight
    case Some("right")      => this.right
    case Some("downRight")  => this.downRight
    case Some("downLeft")   => this.downLeft
    case _                  => None
  }

  def dir(pos: Pos): Option[String] =
    (pos.file.index - this.file.index, pos.rank.index - this.rank.index) match {
      case (0, 1)   => Some("upLeft")
      case (0, -1)  => Some("downRight")
      case (1, 1)   => Some("upRight")
      case (1, 0)   => Some("right")
      case (-1, 0)  => Some("left")
      case (-1, -1) => Some("downLeft")
      case _ => None
    }

  def isInLine(pos1: Pos, pos2: Pos): Boolean = this.dir(pos1) == pos1.dir(pos2)
  // *end of note about neighbourhood

  @inline def file = File of this // column (as if it was an index in a 1D array)
  @inline def rank = Rank of this // horizontal row, makes sense in a 2D array

  // these 3 below might be handy
  // def touches(other: Pos): Boolean = xDist(other) <= 1 && yDist(other) <= 1
  // def xDist(other: Pos) = abs(file - other.file)
  // def yDist(other: Pos) = abs(rank - other.rank)

  // @TODO VFR: test these
  def >|(stop: Pos => Boolean): List[Pos]                   = |<>|(stop, _.right)
  def |<(stop: Pos => Boolean): List[Pos]                   = |<>|(stop, _.left)
  def |<>|(stop: Pos => Boolean, dir: Direction): List[Pos] =
    dir(this) map { p =>
      p :: (if (stop(p)) Nil else p.|<>|(stop, dir))
    } getOrElse Nil

  // @TODO: understand if these are useful for Abalone, adapt if needed
  /*
  def ?<(other: Pos): Boolean = file < other.file
  def ?>(other: Pos): Boolean = file > other.file
  def ?+(other: Pos): Boolean = rank < other.rank
  def ?^(other: Pos): Boolean = rank > other.rank
  def ?|(other: Pos): Boolean = file == other.file
  def ?-(other: Pos): Boolean = rank == other.rank

  def <->(other: Pos): Iterable[Pos] =
    min(file.index, other.file.index) to max(file.index, other.file.index) flatMap { Pos.at(_, rank.index) }

  def onSameDiagonal(other: Pos): Boolean =
    file.index - rank.index == other.file.index - other.rank.index || file.index + rank.index == other.file.index + other.rank.index
  def onSameLine(other: Pos): Boolean     = ?-(other) || ?|(other)

  def isLight: Boolean = (file.index + rank.index) % 2 == 1
   */

  // We're going to use the chess piotr's which are not based on the
  // indices that we use.
  def piotr: Char = Piotr.lookup
    .get(index)
    .getOrElse(
      '*' // will default to char in i9
    )
  def piotrStr = piotr.toString

  def key                 = file.toString + rank.toString
  def officialNotationKey = s"${File(rank.index).getOrElse("")}${Rank(file.index).getOrElse("")}"
  override def toString   = key
}

object Pos {
/*
  indexes of Pos outside of the hexagon :
                                          row   col
9 -   72  73  74  75 '&' ''' '(' ')' '*'   8:   >3 (<9)
8 -   63  64  65 '7' '8' '9' '!' '?' '¥'   7:   >2 (<9)
7 -   54  55 'Y' 'Z' '0' '1' '2' '3' '£'   6:   >1 (<9)
6 -   45 'P' 'Q' 'R' 'S' 'T' 'U' 'V' '¡'   5:   >0 (<9)
5 -  'G' 'H' 'I' 'J' 'K' 'L' 'M' 'N' '}'   4:   <9
4 -  'y' 'z' 'A' 'B' 'C' 'D' 'E' 'F' 35    3:   <8
3 -  'q' 'r' 's' 't' 'u' 'v' 'w' 25  26    2:   <7
2 -  'i' 'j' 'k' 'l' 'm' 'n' 15  16  17    1:   <6
1 -  'a' 'b' 'c' 'd' 'e'  5   6   7   8    0:   <5
      |   |   |   |   |   |   |   |   |
      A   B   C   D   E   F   G   H   I
*/
  def isInHexagon(index: Int): Boolean = {
    if (index < 0) return false
    val row = index / File.all.size
    val remainder = index % File.all.size
    if (index >= File.all.size * Rank.all.size) return false
    if (row < (File.all.size / 2 + 1)) {
      if (remainder >= (File.all.size / 2 + 1 + row)) return false
    } else {
      if (remainder <= (row - (File.all.size / 2 + 1))) return false
    }
    return true
  }

  def apply(index: Int): Option[Pos] =
    if (isInHexagon(index)) Some(new Pos(index))
    else None

  def apply(file: File, rank: Rank): Option[Pos] =
    if (isInHexagon(file.index + File.all.size * rank.index)) Some(new Pos(file.index + File.all.size * rank.index))
    else None

  def at(x: Int, y: Int): Option[Pos] =
    if (isInHexagon(x + File.all.size * y)) Some(new Pos(x + File.all.size * y))
    else None

  def fromKey(key: String): Option[Pos] = allKeys get key

  def piotr(c: Char): Option[Pos] = allPiotrs get c

  // @TODO VFR: check these piotrKey funcs work as expected, did not test yet
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
  // val F1 = new Pos(5) //
  // val G1 = new Pos(6) //
  // val H1 = new Pos(7) //
  // val I1 = new Pos(8) //
  val A2 = new Pos(9) // means the 9th square of our square grid is represented by A2 (index starting bottom left)
  val B2 = new Pos(10)
  val C2 = new Pos(11)
  val D2 = new Pos(12)
  val E2 = new Pos(13)
  val F2 = new Pos(14)
  // val G2 = new Pos(15)
  // val H2 = new Pos(16)
  // val I2 = new Pos(17)
  val A3 = new Pos(18)
  val B3 = new Pos(19)
  val C3 = new Pos(20)
  val D3 = new Pos(21)
  val E3 = new Pos(22)
  val F3 = new Pos(23)
  val G3 = new Pos(24)
  // val H3 = new Pos(25) //
  // val I3 = new Pos(26) //
  val A4 = new Pos(27)
  val B4 = new Pos(28)
  val C4 = new Pos(29)
  val D4 = new Pos(30)
  val E4 = new Pos(31)
  val F4 = new Pos(32)
  val G4 = new Pos(33)
  val H4 = new Pos(34)
  // val I4 = new Pos(35) //
  val A5 = new Pos(36)
  val B5 = new Pos(37)
  val C5 = new Pos(38)
  val D5 = new Pos(39)
  val E5 = new Pos(40)
  val F5 = new Pos(41)
  val G5 = new Pos(42)
  val H5 = new Pos(43)
  val I5 = new Pos(44)
  // val A6 = new Pos(45) //
  val B6 = new Pos(46)
  val C6 = new Pos(47)
  val D6 = new Pos(48)
  val E6 = new Pos(49)
  val F6 = new Pos(50)
  val G6 = new Pos(51)
  val H6 = new Pos(52)
  val I6 = new Pos(53)
  // val A7 = new Pos(54) //
  // val B7 = new Pos(55) //
  val C7 = new Pos(56)
  val D7 = new Pos(57)
  val E7 = new Pos(58)
  val F7 = new Pos(59)
  val G7 = new Pos(60)
  val H7 = new Pos(61)
  val I7 = new Pos(62)
  // val A8 = new Pos(63) //
  // val B8 = new Pos(64) //
  // val C8 = new Pos(65) //
  val D8 = new Pos(66)
  val E8 = new Pos(67)
  val F8 = new Pos(68)
  val G8 = new Pos(69)
  val H8 = new Pos(70)
  val I8 = new Pos(71)
  // val A9 = new Pos(72) //
  // val B9 = new Pos(73) //
  // val C9 = new Pos(74) //
  // val D9 = new Pos(75) //
  val E9 = new Pos(76)
  val F9 = new Pos(77)
  val G9 = new Pos(78)
  val H9 = new Pos(79)
  val I9 = new Pos(80)

  val all: List[Pos] = (0 to (File.all.size * Rank.all.size) - 1).map(new Pos(_)).toList.filter(i => isInHexagon(i.index))

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

  val posR = "([a-i][1-9])" // will be used in Uci as regex to find origin square and target square in a move
}
