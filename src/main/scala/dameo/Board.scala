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

  lazy val actors: Map[Pos, Actor] = pieces map { case (pos, piece) =>
    (pos, Actor(piece, pos, this))
  }

  lazy val actorsOf: Player.Map[Seq[Actor]] = {
    val (w, b) = actors.values.toSeq.partition {
      _.player.p1
    }
    Player.Map(w, b)
  }

  lazy val posMap: Map[Piece, Iterable[Pos]] = pieces.groupMap(_._2)(_._1)

  def withHistory(h: History): Board       = copy(history = h)
  def updateHistory(f: History => History) = copy(history = f(history))

  def withVariant(v: Variant): Board =
    if (v.dropsVariant) copy(variant = v)
    else copy(variant = v)

  def situationOf(player: Player) = Situation(this, player)

  lazy val ghosts = pieces.values.count(_.isGhost)

  def autoDraw: Boolean =
    ghosts == 0 && variant.maxDrawingMoves(this).fold(false)(m => history.halfMoveClock >= m)

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
