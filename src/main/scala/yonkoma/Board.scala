package strategygames.yonkoma

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

  def piecesOf(player: Player): PieceMap = pieces filter (_._2 is player)

  // the squares of a player's checkers, in a1-to-l12 order
  def checkersOf(player: Player): List[Pos] = piecesOf(player).keys.toList.sortBy(_.index)

  lazy val posMap: Map[Piece, Iterable[Pos]] = pieces.groupMap(_._2)(_._1)

  def empty(pos: Pos): Boolean = !pieces.contains(pos)

  def onBoard(pos: Pos): Boolean = boardSize.onBoard(pos)

  def move(orig: Pos, dest: Pos): Option[Board] =
    if (pieces.contains(dest) || !onBoard(dest)) None
    else
      pieces.get(orig).map { piece =>
        copy(pieces = pieces - orig + (dest -> piece))
      }

  def place(piece: Piece, at: Pos): Option[Board] =
    if (pieces.contains(at) || !onBoard(at)) None
    else Some(copy(pieces = pieces + (at -> piece)))

  def withHistory(h: History): Board       = copy(history = h)
  def updateHistory(f: History => History) = copy(history = f(history))

  def withVariant(v: Variant): Board = copy(variant = v)

  def situationOf(player: Player) = Situation(this, player)

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

    def onBoard(pos: Pos): Boolean = pos.file.index < width && pos.rank.index < height

    val validPos: List[Pos] = Pos.all.filter(onBoard)

    val files: List[File] = File.all.take(width)
    val ranks: List[Rank] = Rank.all.take(height)

    override def toString = key

  }

  object BoardSize {
    val all: List[BoardSize] = List(Dim10x10, Dim12x12)
  }

  case object Dim10x10
      extends BoardSize(
        width = 10,
        height = 10
      )

  case object Dim12x12
      extends BoardSize(
        width = 12,
        height = 12
      )

}
