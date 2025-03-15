package abalone.format

import strategygames.Player
import strategygames.abalone._
import strategygames.abalone.geometry.Cell
import strategygames.abalone.variant.Variant

object VVisual {
  def <<(source: String): BBoard = {
    val lines = augmentString(source).linesIterator.to(List)
    val filtered = lines.size match {
      case 8 => lines
      case n if n > 8 => lines.slice(1, 9)
      case n => (List.fill(8 - n)("")) ::: lines
    }

    val v = Variant.default //FIXME?

    BBoard(
      pieces = (for {
        (l, y) <- (filtered zipWithIndex)
        (c, x) <- (l zipWithIndex)
        // might need to get changed for different gameFamilys
        // but then the whole file will need changing! only used for tests
        // role   <- Role forsyth c.toLower
      } yield {
        val a = new Cell(x, 7 - y)
        (if (v.boardType.isCell(a)) Option(a) else None) map { pos =>
          pos -> Piece(Player.fromP1(c isUpper), Stone)
        }
      }) flatten,
      variant = v
    )
  }

  def >>(board: BBoard): String = >>|(board, Map.empty)

  def >>|(board: BBoard, marks: Map[Iterable[Cell], Char]): String = {
    val markedCells: Map[Cell, Char] = marks.foldLeft(Map[Cell, Char]()) { case (marks, (cells, char)) =>
      marks ++ cells.toList.map(a => (a, char))
    }

    board.variant.boardType.cellSet
      .map(a => markedCells.get(a).getOrElse(board.getPiece(a).fold(' ')(_ forsyth)))
      .mkString
  }
    .map(char => """\s*$""".r.replaceFirstIn(char.toString, ""))
    .mkString("\n")

  def addNewLines(str: String) = "\n" + str + "\n"
}