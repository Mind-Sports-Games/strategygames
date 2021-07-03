package draughts

import variant.{ Standard, Variant }

case class StartingPosition(
    code: String,
    fen: String,
    moves: String,
    name: Option[String] = None,
    wikiPath: Option[String] = None,
    featurable: Boolean = true
) {

  val shortName = code
  val fullName  = name.fold(code) { n => s"$code: $n" }

  def url                        = wikiPath.map(u => s"https://en.wikipedia.org/wiki/$u")
  def initialStandard            = initialVariant(Standard)
  def initialVariant(v: Variant) = fen == v.initialFen || fen == v.shortInitialFen
}

object StartingPosition {

  case class Category(name: String, positions: List[StartingPosition])

  val random = StartingPosition("random", "random", "")

  /*lazy val featurable = new scala.util.Random(475591).shuffle(all.filter(_.featurable)).toIndexedSeq

  def randomFeaturable = featurable(scala.util.Random.nextInt(featurable.size))*/

}
