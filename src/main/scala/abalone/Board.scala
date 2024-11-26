package strategygames.abalone

import strategygames.Player

import variant.Variant

case class Board(
    pieces: PieceMap,
    history: History,
    variant: Variant
) {
  def apply(at: Pos): Option[Piece]                = pieces.get(at)
  def apply(file: File, rank: Rank): Option[Piece] = {
    val pos = Pos(file, rank)
    pos match {
      case Some(pos) => pieces.get(pos)
      case None      => None
    }
  }

  def piecesOf(player: Player): PieceMap = pieces.filter(_._2.is(player))

  def isEmptySquare(pos: Option[Pos]): Boolean = pos.fold(false)(!this.pieces.contains(_))

  def withHistory(h: History): Board       = copy(history = h)
  def updateHistory(f: History => History) = copy(history = f(history))

  def withVariant(v: Variant): Board =
    if (v.dropsVariant) copy(variant = v)
    else copy(variant = v)

  def situationOf(player: Player) = Situation(this, player)

  def valid(strict: Boolean) = variant.valid(this, strict)

  def materialImbalance: Int = variant.materialImbalance(this)

  def autoDraw: Boolean = history.threefoldRepetition && variant.repetitionEnabled

  override def toString = s"$variant Position after ${history.recentTurnUciString}"

  lazy val actors: Map[Pos, Actor] = pieces.map { case (pos, piece) =>
    (pos, Actor(piece, pos, this))
  }

  lazy val posMap: Map[Piece, Iterable[Pos]] = pieces.groupMap(_._2)(_._1)

  lazy val piecesOnBoardCount: Int = pieces.keys.size
}

object Board {

  def apply(pieces: Iterable[(Pos, Piece)], variant: Variant): Board =
    Board(pieces.toMap, History(), variant)

  def init(variant: Variant): Board = Board(variant.pieces, variant)

  // def empty(variant: Variant): Board = Board(Nil, variant)

  sealed abstract class BoardSize(
      val width: Int,
      val height: Int,
      val irregular: Boolean = true
  ) {

    val key   = s"${width}x${height}"
    val sizes = List(width, height)

    val validPos: List[Pos] = Pos.all

    override def toString = key

  }

  // an Hexagon of width 5 fits in a square of width 9
  object BoardSize {
    val all: List[BoardSize] = List(Dim9x9)
  }

  case object Dim9x9
      extends BoardSize(
        width = 9,
        height = 9
      )

}
