package chess

// Correctness depends on singletons for each variant ID
abstract class RuleSet (
    val id: Int,
    val name: String,
    val title: String,
) {

  def chess    = this == Chess
  def draughts = this == Draughts

}

object RuleSet {

  val all = List(
    Chess,
    Draughts
  )
  val byId = all map { v =>
    (v.id, v)
  } toMap

  val default = Chess

  def apply(id: Int): Option[RuleSet]     = byId get id
  def orDefault(id: Int): RuleSet         = apply(id) | default

  def byName(name: String): Option[RuleSet] =
    all find (_.name.toLowerCase == name.toLowerCase)

  def exists(id: Int): Boolean = byId contains id

}

case object Chess
    extends RuleSet(
      id = 1,
      name = "Chess",
      title = "FIDE Chess",
    ) {
}

case object Draughts
    extends RuleSet(
      id = 1,
      name = "Draughts",
      title = "Draughts (FMJD)",
    ) {
}
