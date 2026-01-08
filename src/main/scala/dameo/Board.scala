package strategygames.dameo

import strategygames.Player

import variant.Variant

case class Board(
    pieces: PieceMap,
    history: History,
    variant: Variant
) {

  def apply(at: Pos): Option[Piece] = pieces get at
  def apply(file: File, rank: Rank) = pieces get Pos(file, rank)

  def boardSize = variant.boardSize

  // Not sure its correct for actors to be restricted by active piece?
  lazy val actors: Map[Pos, Actor] = {
    val active = pieces.filter { case (_, piece) => piece.isActive }
    (if (active.isEmpty) pieces else active).map { case (pos, piece) =>
      (pos, Actor(piece, pos, this))
    }
  }

  lazy val actorsOf: Player.Map[Seq[Actor]] = {
    val (w, b) = actors.values.toSeq.partition {
      _.player.p1
    }
    Player.Map(w, b)
  }

  lazy val posMap: Map[Piece, Iterable[Pos]] = pieces.groupMap(_._2)(_._1)

  def withinBounds(pos: Pos): Boolean = {
    val x = pos.file.index
    val y = pos.rank.index
    x >= 0 && x < boardSize.width && y >= 0 && y < boardSize.height
  }

  def backrow(pos: Pos, player: Player) = {
    player match {
      case P1 => pos.rank.index == boardSize.height - 1
      case P2 => pos.rank.index == 0
    }
  }

  def kingVsKing(): Boolean = {
    pieces.size == 2 && pieces.values.filter(_.role == King).size == 2
  }

  def empty(pos: Pos): Boolean = {
    !pieces.contains(pos)
  }

  def actorAt(at: Pos): Option[Actor] = actors get at

  def withHistory(h: History): Board       = copy(history = h)
  def updateHistory(f: History => History) = copy(history = f(history))

  def withVariant(v: Variant): Board =
    if (v.dropsVariant) copy(variant = v)
    else copy(variant = v)

  def situationOf(player: Player) = Situation(this, player)

  lazy val ghosts = pieces.values.count(_.isGhost)

  def move(orig: Pos, dest: Pos): Option[Board] =
    if (pieces.contains(dest)) None
    else
      pieces.get(orig).map { piece =>
        copy(pieces = pieces - orig + (dest -> piece))
      }

  def autoDraw: Boolean =
    (variant.maxDrawingMoves(this).fold(false)(m => history.halfMoveClock > m)) || variant.twentyFiveMoves(
      history
    ) || (history.threefoldRepetition && variant.repetitionEnabled)

  def valid(strict: Boolean) = variant.valid(this, strict)

  def materialImbalance: Int = variant.materialImbalance(this)

  override def toString = s"$variant Position after ${history.recentTurnUciString}"
}

object Board {

  def apply(pieces: Iterable[(Pos, Piece)], variant: Variant): Board =
    Board(pieces.toMap, History(), variant)

  def init(variant: Variant): Board = Board(variant.pieces, variant)

  sealed abstract class BoardSize(
      val width: Int,
      val height: Int
  ) {

    val key   = s"${width}x${height}"
    val sizes = List(width, height)

    val validPos: List[Pos] =
      Pos.all.filter(p => p.file.index < width && p.rank.index < height)

    override def toString = key

  }

  object BoardSize {
    val all: List[BoardSize] = List(Dim8x8)
  }

  case object Dim8x8
      extends BoardSize(
        width = 8,
        height = 8
      )

}
