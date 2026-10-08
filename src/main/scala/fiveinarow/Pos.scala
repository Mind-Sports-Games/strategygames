package strategygames.fiveinarow

case class Pos private (index: Int) extends AnyVal {

  @inline def file = File of this
  @inline def rank = Rank of this

  def step(dx: Int, dy: Int): Option[Pos] =
    Pos.at(file.index + dx, rank.index + dy)

  def piotr: Char = Pos.piotrLookup.getOrElse(index, '?')
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

  // lila decodes piotr with one table keyed by square name, which go's 19x19 board already covers
  private lazy val piotrLookup: Map[Int, Char] =
    all.flatMap(pos => strategygames.go.Pos.fromKey(pos.key).map(g => pos.index -> g.piotr)).to(Map)

  lazy val allPiotrs: Map[Char, Pos] = all.map(pos => pos.piotr -> pos).to(Map)

  val posR = "([a-o](?:1[0-5]|[1-9]))"

}
