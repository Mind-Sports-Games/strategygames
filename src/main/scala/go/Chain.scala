package strategygames.go

import strategygames.Player

object Chain {

  def at(board: Board, pos: Pos): Set[Pos] =
    if (board.stoneGrid(pos.index) == Board.emptyPoint) Set.empty
    else {
      val walk = new Walk(board)
      walk.chainFrom(pos.index, Walk.noPoint)
      walk.pointsBetween(0, walk.walked)
    }

  private[go] def regionFrom(board: Board, origin: Pos)(extendsThrough: Pos => Boolean): Set[Pos] =
    stonesOf(board).regionFrom(origin, extendsThrough)

  def liberties(board: Board, group: Set[Pos]): Set[Pos] = stonesOf(board).libertiesOf(group)

  def hasLiberty(board: Board, group: Set[Pos]): Boolean = stonesOf(board).hasLiberty(group)

  def capturedBy(board: Board, player: Player, emptyPoint: Pos): Set[Pos] = {
    requireVacant(board, emptyPoint)
    new Walk(board).capturedBy(player, emptyPoint.index)
  }

  // NOTE: capture and suicide are one question, because the captured stones come off the board
  // before the new stone's liberties are counted and are often what gives it a liberty at all.
  def capturesUnlessSuicide(board: Board, player: Player, emptyPoint: Pos): Option[Set[Pos]] = {
    requireVacant(board, emptyPoint)
    val walk     = new Walk(board)
    val captured = walk.capturedBy(player, emptyPoint.index)
    Option.when(captured.nonEmpty || walk.placementHasLiberty(player, emptyPoint.index))(captured)
  }

  private def requireVacant(board: Board, point: Pos): Unit =
    require(!board.pieces.contains(point), s"a stone already stands on ${point.key}")

  private def stonesOf(board: Board): Stones =
    Stones(board.pieces, board.variant.boardSize)

  private object Walk {
    val noPoint = -1
  }

  final private class Walk(board: Board) {
    private val stones     = board.stoneGrid
    private val neighbours = board.variant.boardSize.neighbourIndices
    private val reached    = new Array[Boolean](Pos.allSize)
    private val points     = new Array[Int](Pos.allSize)
    var walked             = 0

    def chainFrom(origin: Int, placedAt: Int): Boolean = {
      val colour     = stones(origin)
      var next       = walked
      var hasLiberty = false
      reached(origin) = true
      points(walked) = origin
      walked += 1
      while (next < walked) {
        val around = neighbours(points(next))
        var n      = 0
        while (n < around.length) {
          val neighbour = around(n)
          val stone     = stones(neighbour)
          if (stone == colour) {
            if (!reached(neighbour)) {
              reached(neighbour) = true
              points(walked) = neighbour
              walked += 1
            }
          } else if (stone == Board.emptyPoint && neighbour != placedAt) hasLiberty = true
          n += 1
        }
        next += 1
      }
      hasLiberty
    }

    def capturedBy(player: Player, placedAt: Int): Set[Pos] = {
      val opponent = Board.stoneCode(!player)
      val around   = neighbours(placedAt)
      var captured = Set.empty[Pos]
      var n        = 0
      while (n < around.length) {
        val neighbour = around(n)
        if (stones(neighbour) == opponent && !reached(neighbour)) {
          val from = walked
          if (!chainFrom(neighbour, placedAt)) captured = captured ++ pointsBetween(from, walked)
        }
        n += 1
      }
      captured
    }

    def placementHasLiberty(player: Player, placedAt: Int): Boolean = {
      val own    = Board.stoneCode(player)
      val around = neighbours(placedAt)
      var found  = false
      var n      = 0
      while (!found && n < around.length) {
        val neighbour = around(n)
        val stone     = stones(neighbour)
        if (stone == Board.emptyPoint) found = true
        else if (stone == own && !reached(neighbour)) found = chainFrom(neighbour, placedAt)
        n += 1
      }
      found
    }

    def pointsBetween(from: Int, until: Int): Set[Pos] = {
      val builder = Set.newBuilder[Pos]
      var i       = from
      while (i < until) {
        builder += Pos.atIndex(points(i))
        i += 1
      }
      builder.result()
    }
  }

  private case class Stones(pieces: PieceMap, boardSize: Board.BoardSize) {

    def regionFrom(origin: Pos, extendsThrough: Pos => Boolean): Set[Pos] =
      if (extendsThrough(origin)) grownFrom(List(origin), Set(origin), extendsThrough)
      else Set.empty

    def libertiesOf(group: Set[Pos]): Set[Pos] =
      group.flatMap(neighboursOf).filterNot(pieces.contains)

    def hasLiberty(group: Set[Pos]): Boolean =
      group.exists(neighboursOf(_).exists(!pieces.contains(_)))

    private def neighboursOf(pos: Pos): List[Pos] = boardSize.neighbours(pos.index)

    // NOTE: a point enters `reached` as it is queued rather than as it is expanded, so a point that
    // several of its neighbours touch is still walked once. Marking on the way out returns the same
    // region at a much higher cost on a large one, and no assertion on the result can tell them apart.
    @annotation.tailrec
    private def grownFrom(
        pending: List[Pos],
        reached: Set[Pos],
        extendsThrough: Pos => Boolean
    ): Set[Pos] =
      pending match {
        case Nil         => reached
        case pos :: rest =>
          val newlyReached =
            neighboursOf(pos).filter(neighbour => !reached.contains(neighbour) && extendsThrough(neighbour))
          grownFrom(newlyReached ::: rest, reached ++ newlyReached, extendsThrough)
      }

  }

}
