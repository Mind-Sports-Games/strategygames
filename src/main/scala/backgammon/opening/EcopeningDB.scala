package strategygames.backgammon.opening

// format: off
object EcopeningDB {

  import Ecopening._

  val MAX_TURNS = 25

  lazy val all = allByEco.values.toList.sorted

  lazy val allByFen: Map[FEN, Ecopening] = allByEco.map {
    case (_, opening) => opening.fen -> opening
  }

  lazy val allByEco: Map[ECO, Ecopening] = Map(
"A00" -> new Ecopening("A00", "backgammon", "Backgammon Start Pos", "Backgammon Start Pos", "", "5S,3,3s,1,5s,4,2S/5s,3,3S,1,5S,4,2s[] w - - 1", "")
  )
}
