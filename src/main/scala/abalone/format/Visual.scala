package strategygames.abalone.format
import strategygames.abalone._
import strategygames.Player

object Visual {

  def <<(source: String): Board = {
    val lines    = augmentString(source).linesIterator.to(List)
    val filtered = lines.size match {
      case 8          => lines
      case n if n > 8 => lines.slice(1, 9)
      case n          => (List.fill(8 - n)("")) ::: lines
    }
    Board(
      pieces = (for {
        (l, y) <- (filtered zipWithIndex)
        (c, x) <- (l zipWithIndex)
        // might need to get changed for different gameFamilys
        // but then the whole file will need changing! only used for tests
        // role   <- Role forsyth c.toLower
      } yield {
        Pos.at(x, 7 - y) map { pos =>
          pos -> (Piece(Player.fromP1(c isUpper), Stone))
        }
      }) flatten,
      variant = strategygames.abalone.variant.Variant.default
    )
  }

  def >>(board: Board): String = >>|(board, Map.empty)

  def >>|(board: Board, marks: Map[Iterable[Pos], Char]): String = {
    val markedPoss: Map[Pos, Char] = marks.foldLeft(Map[Pos, Char]()) { case (marks, (poss, char)) =>
      marks ++ (poss.toList map { pos =>
        (pos, char)
      })
    }
    for (y <- Rank.allReversed) yield {
      for (x <- File.all) yield {
        val pos = Pos(x, y)
        pos match {
          case Some(pos) => markedPoss.get(pos) getOrElse board(pos).fold(' ')(_ forsyth)
          case None      => None
        }
      }
    } mkString
  } map { """\s*$""".r.replaceFirstIn(_, "") } mkString "\n"

  def addNewLines(str: String) = "\n" + str + "\n"
}
