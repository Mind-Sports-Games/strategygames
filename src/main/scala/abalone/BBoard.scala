package abalone

import abalone.util.geometry.Cell
import strategygames.Player
import strategygames.abalone.variant.Variant
import strategygames.abalone.{Actor, History, Piece, Situation}

case class BBoard(
                   pieces: Map[Cell, Piece],
                   history: History,
                   variant: Variant
                 ) {
  def piecesOf(player: Player): Map[Cell, Piece] = pieces.filter(_._2.is(player))

  def isPiece(a: Cell): Boolean = pieces.contains(a)

  def getPiece(a: Cell): Option[Piece] = pieces.get(a)

  def withHistory(h: History): BBoard = copy(history = h)

  def updateHistory(f: History => History) = copy(history = f(history))

  def withVariant(v: Variant): BBoard = copy(variant = v)

  def situationOf(player: Player) = Situation(this, player)

  def valid(strict: Boolean) = variant.valid(this, strict)

  def autoDraw: Boolean = history.threefoldRepetition && variant.repetitionEnabled

  override def toString = s"$variant Position after ${history.recentTurnUciString}"

  lazy val actors: Map[Cell, Actor] = pieces.map { case (a, piece) => (a, Actor(piece, a, this)) }

  lazy val posMap: Map[Piece, Iterable[Cell]] = pieces.groupMap(_._2)(_._1)

  lazy val piecesOnBoardCount: Int = pieces.keys.size
}

