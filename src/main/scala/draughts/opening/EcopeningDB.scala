package strategygames.draughts
package opening

// format: OFF
object EcopeningDB {

  import Ecopening._

  val MAX_TURNS = 25

  lazy val all = allByEco.values.toList.sorted

  lazy val allByFen: Map[FEN, Ecopening] = allByEco.map {
    case (_, opening) => opening.fen -> opening
  }

  lazy val allByEco: Map[ECO, Ecopening] = Map()
}
