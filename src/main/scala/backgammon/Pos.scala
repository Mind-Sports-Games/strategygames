package strategygames.backgammon

import strategygames.Player

import scala.math.{ abs, max, min }

//match src/main/scala/fairysf/Pos.scala for use in analysis and stratops
object Piotr {
  val lookup: Map[Int, Char] = Map(
    Pos.A1.index -> 'a',
    Pos.B1.index -> 'b',
    Pos.C1.index -> 'c',
    Pos.D1.index -> 'd',
    Pos.E1.index -> 'e',
    Pos.F1.index -> 'f',
    Pos.G1.index -> 'g',
    Pos.H1.index -> 'h',
    Pos.A2.index -> 'i',
    Pos.B2.index -> 'j',
    Pos.C2.index -> 'k',
    Pos.D2.index -> 'l',
    Pos.E2.index -> 'm',
    Pos.F2.index -> 'n',
    Pos.G2.index -> 'o',
    Pos.H2.index -> 'p',
    Pos.I1.index -> '[',
    Pos.I2.index -> ']',
    Pos.J1.index -> '\\',
    Pos.J2.index -> '^',
    Pos.K1.index -> '\u00A9',
    Pos.K2.index -> '\u00B2',
    Pos.L1.index -> '\u00AA',
    Pos.L2.index -> '\u00B3'
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

  // We're going to use the chess piotr's which are not based on the indices that we use.
  def piotr: Char = Piotr.lookup.get(index).getOrElse('?')
  def piotrStr    = piotr.toString

  def sgf = (97 + (23 - index)).toChar.toString()

  def last: Boolean = (index + 1) % File.all.size == 0

  def key               = file.toString + rank.toString
  override def toString = key

}

object Pos {

  def apply(index: Int): Option[Pos] =
    if (0 <= index && index < File.all.size * Rank.all.size) Some(new Pos(index))
    else None

  def apply(file: File, rank: Rank): Pos = new Pos(file.index + File.all.size * rank.index)

  def at(x: Int, y: Int): Option[Pos] =
    if (0 <= x && x < File.all.size && 0 <= y && y < Rank.all.size)
      Some(new Pos(x + (File.all.size - x) * y))
    else None

  def opposite(index: Int): Option[Pos] =
    apply(if (index < File.all.size) index + File.all.size else index - File.all.size)

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

  // backwards for first rank
  val A1 = new Pos(11)
  val B1 = new Pos(10)
  val C1 = new Pos(9)
  val D1 = new Pos(8)
  val E1 = new Pos(7)
  val F1 = new Pos(6)
  val G1 = new Pos(5)
  val H1 = new Pos(4)
  val I1 = new Pos(3)
  val J1 = new Pos(2)
  val K1 = new Pos(1)
  val L1 = new Pos(0)

  val A2 = new Pos(12)
  val B2 = new Pos(13)
  val C2 = new Pos(14)
  val D2 = new Pos(15)
  val E2 = new Pos(16)
  val F2 = new Pos(17)
  val G2 = new Pos(18)
  val H2 = new Pos(19)
  val I2 = new Pos(20)
  val J2 = new Pos(21)
  val K2 = new Pos(22)
  val L2 = new Pos(23)

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

  def barIndex(player: Player): Int = player.fold(L2, L1).index - indexDirection(player)

  def indexDirection(player: Player): Int = player.fold(-1, 1)

  def home(player: Player): List[Pos] = player.fold(
    all.filter(_.index < 6),
    all.filter(_.index > 17)
  )

  // 1 is opponents home, 4 is your home section.
  def byQuarter(player: Player, quarter: Int) =
    player.fold(
      all.filter(p => p.index < 6 * (5 - quarter) && p.index >= (6 * (4 - quarter))),
      all.filter(p => p.index >= 6 * (quarter - 1) && p.index < (6 * quarter))
    )

  val posR = "([a-l][1-2])"

}
