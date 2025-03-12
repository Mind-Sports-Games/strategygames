package strategygames.abalone

import abalone.Cell
import strategygames.Player
import strategygames.abalone.variant.Variant

case class Board(
                  pieces: PieceMap,
                  history: History,
                  variant: Variant
                ) {
  def apply(at: Pos): Option[Piece] = pieces.get(at)

  def apply(file: File, rank: Rank): Option[Piece] = {
    Pos(file, rank) match {
      case Some(pos) => apply(pos)
      case None => None
    }
  }

  def piecesOf(player: Player): PieceMap = pieces.filter(_._2.is(player))

  def isEmptyPos(pos: Option[Pos]): Boolean = pos.fold(false)(!pieces.contains(_))

  def withHistory(h: History): Board = copy(history = h)

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

  sealed abstract class BoardType(
                                   val width: Int,
                                   val height: Int,
                                   val irregular: Boolean = true
                                 ) {
    val key = s"${width}x${height}"
    //val validPos: List[Pos] = Pos.all
    final val cells: Set[Cell] = Range(0, width)
      .flatMap(x => Range(0, height)
        .filter(y => isCell(x, y))
        .map(y => new Cell(x, y))
      ).toSet

    //
    // Norm
    final def isCell(a: Cell): Boolean = isCell(a.x, a.y)

    def isCell(x: Int, y: Int): Boolean = 0 <= x & x < width & 0 <= y & y < height

    final def dist(a: Cell, b: Cell): Int = dist(a, b.x, b.y)

    final def dist(a: Cell, x: Int, y: Int): Int = dist(a.x, a.y, x, y)

    def dist(x: Int, y: Int, z: Int, t: Int): Int = norm(z - x, t - y)

    final def norm(a: Cell): Int = norm(a.x, a.y)

    def norm(x: Int, y: Int): Int = math.abs(x) + math.abs(y)


    //
    //
    override def toString = key
  }

  object BoardType {
    val all: List[BoardType] = List(Hex5, Hex6)
  }

  /** A Hexagon of side n fits in a square of side 2n - 1 */
  sealed abstract class HexBoardType(val side: Int) extends BoardType(width = 2 * side - 1, height = 2 * side - 1) {
    val centre = new Cell(side - 1, side - 1)

    override val key = s"hex-${side}"

    override def isCell(x: Int, y: Int) = dist(centre, x, y) < side

    override def norm(x: Int, y: Int) = {
      if (x * y < 0) math.abs(x) + math.abs(y)
      else math.max(math.abs(x), math.abs(y))
    }
  }

  case object Hex5 extends HexBoardType(5)

  case object Hex6 extends HexBoardType(6)
}