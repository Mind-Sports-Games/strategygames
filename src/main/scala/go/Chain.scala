package strategygames.go

import strategygames.Player

/** Connectivity on a go board: chains, liberties, and what a placement captures.
  *
  * Every rule in `variant.Variant` that has to look at neighbouring points comes through here, and everything
  * here comes through the one flood fill in `Stones.grownFrom`. That is deliberate — see the note on
  * `grownFrom` for what a second, hand-written fill would get away with.
  */
object Chain {

  def at(board: Board, pos: Pos): Set[Pos] = stonesOf(board).chainAt(pos)

  // the general fill: `chainAt` is this with "holds a stone of the same colour" as the step
  // predicate, and `Variant.emptyRegionsOf` is this with "is empty". private[go] rather than
  // public because the predicate is unconstrained and a caller can ask for nonsense with it.
  private[go] def regionFrom(board: Board, origin: Pos)(extendsThrough: Pos => Boolean): Set[Pos] =
    stonesOf(board).regionFrom(origin, extendsThrough)

  def liberties(board: Board, group: Set[Pos]): Set[Pos] = stonesOf(board).libertiesOf(group)

  // NOTE: an empty group has no liberties, so this answers false for one. That is the literal
  // answer to the question asked and it is not "the group is captured" — capture is
  // `capturesAround`'s job. Reading false here as "captured" is the trap; the group simply
  // isn't there.
  def hasLiberty(board: Board, group: Set[Pos]): Boolean = stonesOf(board).hasLiberty(group)

  def capturedBy(board: Board, player: Player, emptyPoint: Pos): Set[Pos] = {
    requireVacant(board, emptyPoint)
    stonesOf(board).withStone(player, emptyPoint).capturesAround(player, emptyPoint)
  }

  /** What placing `player`'s stone on `emptyPoint` captures, or `None` if the placement is suicide.
    *
    * Capture and suicide are answered together on purpose. A capturing placement is never suicide — lifting
    * the captured stones is what gives the new one its liberty — so a caller that computed the two separately
    * could ask "is this suicide" with an empty capture set and be told yes for a legal recapture, silently
    * withholding the only move of a player in atari. The `Option` makes "captures something and is also
    * suicide" unspellable rather than merely wrong.
    *
    * Ko and superko are not its business; `variant.Variant.isPlayable` layers those on top.
    */
  def capturesUnlessSuicide(board: Board, player: Player, emptyPoint: Pos): Option[Set[Pos]] = {
    requireVacant(board, emptyPoint)
    val afterPlacement = stonesOf(board).withStone(player, emptyPoint)
    val captured       = afterPlacement.capturesAround(player, emptyPoint)
    val settled        = afterPlacement.without(captured)
    Option.when(settled.hasLiberty(settled.chainAt(emptyPoint)))(captured)
  }

  // NOTE: guards the two entry points that place a stone, and only those — `at` and `hasLiberty`
  // place nothing. Without it `pieces.updated` silently overwrites the stone already standing
  // there, so a placement onto an occupied point deletes an enemy stone and can then report the
  // chain it belonged to as captured. Wrong, and quiet about it. `at`/`liberties` stay unguarded.
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
      if (extendsThrough(origin)) grownFrom(List(origin), Nil, Set(origin), extendsThrough)
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

    // NOTE: `reached` grows when a point is *enqueued*, not when it is dequeued, so a point that
    // several frontier points touch is only ever walked once. Move that bookkeeping to the dequeue
    // and the returned Set is identical — the Set collapses the repeats — while the fill walks a
    // large region many times over. No assertion on the result can tell the two apart, which is
    // exactly why there is one fill here and `Variant.emptyRegionsOf` calls it instead of writing
    // its own. The sharing is the mitigation; there is no test standing behind this.
    @annotation.tailrec
    private def grownFrom(
        pending: List[Pos],
        unclassified: List[Pos],
        reached: Set[Pos],
        extendsThrough: Pos => Boolean
    ): Set[Pos] =
      unclassified match {
        case neighbour :: restOfNeighbours =>
          if (reached.contains(neighbour) || !extendsThrough(neighbour))
            grownFrom(pending, restOfNeighbours, reached, extendsThrough)
          else grownFrom(neighbour :: pending, restOfNeighbours, reached + neighbour, extendsThrough)
        case Nil                           =>
          pending match {
            case Nil         => reached
            case pos :: rest => grownFrom(rest, neighboursOf(pos), reached, extendsThrough)
          }
      }

  }

}
