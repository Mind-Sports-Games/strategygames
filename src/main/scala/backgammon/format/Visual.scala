package strategygames.backgammon.format
import strategygames.backgammon._
import strategygames.Player

/** r bqkb r p ppp pp pr P p QnB PP N P PPP RN K R
  */
//Never been tested for Backgammon
object Visual {

  def <<(source: String): Board = {
    val lines    = augmentString(source).linesIterator.to(List)
    val filtered = lines.size match {
      case n if n == Rank.all.size => lines
      case n if n > Rank.all.size  => lines.slice(1, File.all.size + 1)
      case n                       => (List.fill(File.all.size - n)("")) ::: lines
    }
    Board(
      pieces = (for {
        (l, y) <- (filtered zipWithIndex)
        (c, x) <- (l zipWithIndex)
        // might need to get changed for different gameFamilys
        // but then the whole file will need changing! only used for tests
        // role   <- Role forsyth c.toLower
      } yield {
        Pos.at(x, Rank.all.size - 1 - y) map { pos =>
          pos -> ((Piece(Player.fromP1(c isUpper), Stone)), c.toInt)
        }
      }) flatten,
      variant = strategygames.backgammon.variant.Variant.default
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
        markedPoss.get(pos) getOrElse board(pos).fold(' ')(_ forsyth)
      }
    } mkString
  } map { """\s*$""".r.replaceFirstIn(_, "") } mkString "\n"

  def addNewLines(str: String) = "\n" + str + "\n"
}
