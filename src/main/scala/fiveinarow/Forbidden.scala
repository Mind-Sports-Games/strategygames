package strategygames.fiveinarow

// The renju international federation's definitions, applied to black only.
object Forbidden {

  import Line.directions

  private val reach = 4

  private def isOpen(board: Board, pos: Pos): Boolean =
    board.boardSize.onBoard(pos) && board.empty(pos)

  private def line(at: Pos, dx: Int, dy: Int): List[Pos] =
    (-reach to reach).toList.flatMap(n => at.step(dx * n, dy * n))

  private def run(board: Board, at: Pos, dx: Int, dy: Int): List[Pos] =
    Line.run(board, at, BlackStone, dx, dy)

  private def runLength(board: Board, at: Pos, dx: Int, dy: Int): Int = run(board, at, dx, dy).size

  def makesFive(board: Board, at: Pos): Boolean =
    directions.exists { case (dx, dy) => runLength(board, at, dx, dy) == 5 }

  private def makesOverline(board: Board, at: Pos): Boolean =
    directions.exists { case (dx, dy) => runLength(board, at, dx, dy) > 5 }

  private def place(board: Board, at: Pos): Board =
    board.copy(pieces = board.pieces + (at -> Piece(board.seatOf(BlackStone), BlackStone)))

  // a four is the set of black stones one drop on the line turns into exactly five, keyed here to
  // the drops that do it: the two ends of an open four are one four, and B_BBB_B is two
  private def foursAlong(board: Board, at: Pos, dx: Int, dy: Int): Map[Set[Pos], List[Pos]] =
    line(at, dx, dy)
      .filter(isOpen(board, _))
      .flatMap { gap =>
        val five = run(place(board, gap), gap, dx, dy)
        Option.when(five.size == 5 && five.contains(at))((five.toSet - gap) -> gap)
      }
      .groupMap(_._1)(_._2)

  // a three is a line one more stone turns into a straight four, by a drop that is itself allowed
  private def threeAlong(board: Board, at: Pos, dx: Int, dy: Int): Boolean =
    line(at, dx, dy).filter(isOpen(board, _)).exists { gap =>
      foursAlong(place(board, gap), at, dx, dy).exists { case (four, ends) =>
        ends.size == 2 && four.contains(gap)
      } && !isForbidden(board, gap)
    }

  def isForbidden(board: Board, at: Pos): Boolean = {
    val after = place(board, at)
    if (makesFive(after, at)) false
    else if (makesOverline(after, at)) true
    else {
      val fours = directions.map { case (dx, dy) => foursAlong(after, at, dx, dy).size }.sum
      fours >= 2 || directions.count { case (dx, dy) => threeAlong(after, at, dx, dy) } >= 2
    }
  }

}
