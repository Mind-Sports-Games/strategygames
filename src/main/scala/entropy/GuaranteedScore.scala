package strategygames.entropy

object GuaranteedScore {

  def apply(board: Board): Int = lines(board).map(scoreLine).sum

  def lines(board: Board): List[Line] = rows(board) ::: columns(board)

  private def rows(board: Board): List[Line] =
    Rank.all.map { rank =>
      File.all.map(file => board(file, rank).map(_.role)).toVector
    }

  private def columns(board: Board): List[Line] =
    File.all.map { file =>
      Rank.all.map(rank => board(file, rank).map(_.role)).toVector
    }

  def scoreLine(line: Line): Int =
    (for {
      start <- line.indices
      end   <- (start + 2) to line.size
      run    = line.slice(start, end)
      if qualifies(run)
    } yield run.size).sum

  def qualifies(run: Line): Boolean = {
    val n         = run.size
    val gaps      = run.indices.filter(run(_).isEmpty)
    // the centre of an odd run is never mirrored against anything, so it may stand empty:
    // whatever colour lands there, the run still reads the same in both directions
    val gapIsFree = gaps.isEmpty || (n % 2 == 1 && gaps.size == 1 && gaps.head == n / 2)
    n >= 2 && gapIsFree && (0 until n / 2).forall { i =>
      run(i).isDefined && run(i) == run(n - 1 - i)
    }
  }
}
