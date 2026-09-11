package strategygames.go

import strategygames.Player

object Chain {

  def at(board: Board, pos: Pos): Set[Pos] = stonesOf(board).chainAt(pos)

  private[go] def regionFrom(board: Board, origin: Pos)(extendsThrough: Pos => Boolean): Set[Pos] =
    stonesOf(board).regionFrom(origin, extendsThrough)

  def liberties(board: Board, group: Set[Pos]): Set[Pos] = stonesOf(board).libertiesOf(group)

  def hasLiberty(board: Board, group: Set[Pos]): Boolean = stonesOf(board).hasLiberty(group)

  def capturedBy(board: Board, player: Player, emptyPoint: Pos): Set[Pos] = {
    requireVacant(board, emptyPoint)
    stonesOf(board).withStone(player, emptyPoint).capturesAround(player, emptyPoint)
  }

  // NOTE: capture and suicide are one question, because the captured stones come off the board
  // before the new stone's liberties are counted and are often what gives it a liberty at all.
  def capturesUnlessSuicide(board: Board, player: Player, emptyPoint: Pos): Option[Set[Pos]] = {
    requireVacant(board, emptyPoint)
    val afterPlacement = stonesOf(board).withStone(player, emptyPoint)
    val captured       = afterPlacement.capturesAround(player, emptyPoint)
    val settled        = afterPlacement.without(captured)
    Option.when(settled.hasLiberty(settled.chainAt(emptyPoint)))(captured)
  }

  private def requireVacant(board: Board, point: Pos): Unit =
    require(!board.pieces.contains(point), s"a stone already stands on ${point.key}")

  private def stonesOf(board: Board): Stones =
    Stones(board.pieces, board.variant.boardSize, board.variant.defaultRole)

  private case class Stones(pieces: PieceMap, boardSize: Board.BoardSize, stoneRole: Role) {

    def withStone(player: Player, pos: Pos): Stones =
      copy(pieces = pieces.updated(pos, Piece(player, stoneRole)))

    def without(removed: Set[Pos]): Stones = copy(pieces = pieces -- removed)

    def chainAt(pos: Pos): Set[Pos] =
      pieces.get(pos).fold(Set.empty[Pos])(stone => regionFrom(pos, holdsStoneOf(stone.player)))

    def regionFrom(origin: Pos, extendsThrough: Pos => Boolean): Set[Pos] =
      if (extendsThrough(origin)) grownFrom(List(origin), Set(origin), extendsThrough)
      else Set.empty

    def libertiesOf(group: Set[Pos]): Set[Pos] =
      group.flatMap(neighboursOf).filterNot(pieces.contains)

    def hasLiberty(group: Set[Pos]): Boolean =
      group.exists(neighboursOf(_).exists(!pieces.contains(_)))

    def capturesAround(player: Player, placedAt: Pos): Set[Pos] =
      enemyChainsTouching(player, placedAt).filterNot(hasLiberty).flatten

    private def enemyChainsTouching(player: Player, placedAt: Pos): Set[Set[Pos]] =
      neighboursOf(placedAt).filter(pieces.get(_).exists(_.isNot(player))).map(chainAt).toSet

    private def neighboursOf(pos: Pos): List[Pos] = boardSize.neighbours(pos.index)

    private def holdsStoneOf(player: Player): Pos => Boolean = pieces.get(_).exists(_.is(player))

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
