package abalone

import abalone.util.geometry.Cell
import strategygames.Player
import strategygames.abalone.Piece
import strategygames.abalone.variant.Variant

case class BBoard(
                   pieces: Map[Cell, Piece],
                   history: HHistory,
                   variant: Variant
                 ) {
  def piecesOf(player: Player): Map[Cell, Piece] = pieces.filter(_._2.is(player))

  def isPiece(a: Cell): Boolean = pieces.contains(a)

  def getPiece(a: Cell): Option[Piece] = pieces.get(a)

  def withHistory(h: HHistory): BBoard = copy(history = h)

  def updateHistory(f: HHistory => HHistory) = copy(history = f(history))

  def withVariant(v: Variant): BBoard = copy(variant = v)

  def situationOf(player: Player) = SSituation(this, player)

  def valid(strict: Boolean) = variant.valid(this, strict)

  def autoDraw: Boolean = history.threefoldRepetition && variant.repetitionEnabled

  override def toString = s"$variant Position after ${history.recentTurnUciString}"

  lazy val actors: Map[Cell, AActor] = pieces.map { case (a, piece) => (a, AActor(piece, a, this)) }

  lazy val posMap: Map[Piece, Iterable[Cell]] = pieces.groupMap(_._2)(_._1)

  lazy val piecesOnBoardCount: Int = pieces.keys.size
}

object BBoard {
  def apply(pieces: Iterable[(Cell, Piece)], variant: Variant): BBoard =
    BBoard(pieces.toMap, HHistory(), variant)

  def init(variant: Variant): BBoard = BBoard(variant.ppieces, variant)

  // def empty(variant: Variant): Board = Board(Nil, variant)
}