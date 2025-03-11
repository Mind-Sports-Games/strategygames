package strategygames.dameo.opening

// format: off
object EcopeningDB {

  import Ecopening._

  val MAX_TURNS = 25

  lazy val all = allByEco.values.toList.sorted

  lazy val allByFen: Map[FEN, Ecopening] = allByEco.map {
    case (_, opening) => opening.fen -> opening
  }

  lazy val allByEco: Map[ECO, Ecopening] = Map(
    "A00" -> new Ecopening("A00", "dameo", "Dameo Start Pos", "Dameo Start Pos", "", "ADD INITIAL FEN HERE", ""),
  )
}
