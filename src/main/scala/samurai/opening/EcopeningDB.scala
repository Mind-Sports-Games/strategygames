package strategygames.samurai.opening

// format: off
object EcopeningDB {

  import Ecopening._

  val MAX_MOVES = 25

  lazy val all = allByEco.values.toList.sorted

  lazy val allByFen: Map[FEN, Ecopening] = allByEco.map {
    case (_, opening) => opening.fen -> opening
  }

  lazy val allByEco: Map[ECO, Ecopening] = Map(
"A00" -> new Ecopening("A00", "oware", "Oware Start Pos", "Oware Start Pos", "", "4S,4S,4S,4S,4S,4S/4S,4S,4S,4S,4S,4S 0 0 S 1", "")
  )
}
