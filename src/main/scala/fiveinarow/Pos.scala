package strategygames.fiveinarow

import strategygames.Piotr

case class Pos private (index: Int) extends AnyVal {

  @inline def file = File of this
  @inline def rank = Rank of this

  def step(dx: Int, dy: Int): Option[Pos] =
    Pos.at(file.index + dx, rank.index + dy)

  def piotr: Char = Pos.piotrs(index)
  def piotrStr    = piotr.toString

  def key               = file.toString + rank.toString
  override def toString = key
}

object Pos {
  def apply(index: Int): Option[Pos] =
    if (0 <= index && index < allSize) Some(new Pos(index))
    else None

  def apply(file: File, rank: Rank): Pos = new Pos(file.index + File.allSize * rank.index)

  def at(x: Int, y: Int): Option[Pos] =
    if (0 <= x && x < File.allSize && 0 <= y && y < Rank.allSize)
      Some(new Pos(x + File.allSize * y))
    else None

  def fromKey(key: String): Option[Pos] = allKeys get key

  def piotr(c: Char): Option[Pos] = allPiotrs get c

  def keyToPiotr(key: String) = fromKey(key) map (_.piotr)

  val all: List[Pos] = (0 until File.allSize * Rank.allSize).map(new Pos(_)).toList
  val allSize: Int   = File.allSize * Rank.allSize

  val allKeys: Map[String, Pos] = all.map(pos => pos.key -> pos).to(Map)

  private lazy val piotrs: Array[Char] = all.map(pos => Piotr.lookup.getOrElse(pos.key, '?')).toArray

  lazy val allPiotrs: Map[Char, Pos] = all.map(pos => pos.piotr -> pos).to(Map)

  val posR = "([a-s](?:1[0-9]|[1-9]))"

}
