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
"A00" -> new Ecopening("A00", "go9x9", "Go Start Pos", "Go Start Pos", "", "9/9/9/9/9/9/9/9/9[SSSSSSSSSSssssssssss] b - 0 0 0 0 6 1", ""),
"B00" -> new Ecopening("B00", "go13x13", "Go Start Pos", "Go Start Pos", "", "13/13/13/13/13/13/13/13/13/13/13/13/13[SSSSSSSSSSssssssssss] b - 0 0 0 0 6 1", ""),
"C00" -> new Ecopening("C00", "go19x19", "Go Start Pos", "Go Start Pos", "", "19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19[SSSSSSSSSSssssssssss] b - 0 0 - - 6 1", "")
  )
}
