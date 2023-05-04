package strategygames.go.opening

// format: off
object EcopeningDB {

  import Ecopening._

  val MAX_MOVES = 25

  lazy val all = allByEco.values.toList.sorted

  lazy val allByFen: Map[FEN, Ecopening] = allByEco.map {
    case (_, opening) => opening.fen -> opening
  }

  lazy val allByEco: Map[ECO, Ecopening] = Map(
"A00" -> new Ecopening("A00", "go9x9", "Go Start Pos", "Go Start Pos", "", "9/9/9/9/9/9/9/9/9 0 0 W 1", "")
  )
}
