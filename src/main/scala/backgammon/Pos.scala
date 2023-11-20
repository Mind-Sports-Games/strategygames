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
    Pos.I2.index -> ']'
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

  // We're going to use the chess piotr's which are not based on the
  // indices that we use.
  def piotr: Char = Piotr.lookup.get(index).getOrElse('?')
  def piotrStr    = piotr.toString

  def player: Player = if (index < 9) Player.P1 else Player.P2

  def last: Boolean = (index + 1) % 9 == 0

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

  def opposite(index: Int): Option[Pos] = apply(if (index < 9) index + 9 else index - 9)

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
  val H1 = new Pos(7)
  val I1 = new Pos(8)

  // backwards for 2nd rank
  val I2 = new Pos(9)
  val H2 = new Pos(10)
  val G2 = new Pos(11)
  val F2 = new Pos(12)
  val E2 = new Pos(13)
  val D2 = new Pos(14)
  val C2 = new Pos(15)
  val B2 = new Pos(16)
  val A2 = new Pos(17)

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

  // val posR  = "([a-i][1-9]|[a-i]10)"
  val posR = "([a-i][1-2])"

}
