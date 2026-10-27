package strategygames.fiveinarow

object Line {

  // across, up, and the two diagonals: every way a five can run
  val directions: List[(Int, Int)] = List((1, 0), (0, 1), (1, 1), (1, -1))

  // the unbroken stones of a colour through a point along a direction, the point itself included
  def run(board: Board, at: Pos, role: Role, dx: Int, dy: Int): List[Pos] = {
    def side(sx: Int, sy: Int): List[Pos] =
      Iterator
        .iterate(at.step(sx, sy))(_.flatMap(_.step(sx, sy)))
        .takeWhile(_.flatMap(board.apply).exists(_.role == role))
        .flatten
        .toList
    side(-dx, -dy).reverse ::: at :: side(dx, dy)
  }

}
