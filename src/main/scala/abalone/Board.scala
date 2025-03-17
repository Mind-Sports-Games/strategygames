package strategygames.abalone

import strategygames.Player
import strategygames.abalone.format.Uci
import strategygames.abalone.variant.Variant

case class Board(
                  pieces: PieceMap,
                  history: History,
                  variant: Variant
                ) {
  def piecesOf(player: Player): PieceMap = pieces.filter(_._2.is(player))

  def isPiece(a: Pos): Boolean = pieces.contains(a)

  def getPiece(a: Pos): Option[Piece] = pieces.get(a)

  def withHistory(h: History): Board = copy(history = h)

  def updateHistory(f: History => History) = copy(history = f(history))

  def withVariant(v: Variant): Board = copy(variant = v)

  def situationOf(player: Player) = Situation(this, player)

  def valid(strict: Boolean) = variant.valid(this, strict)

  def materialImbalance: Int = variant.materialImbalance(this)

  def autoDraw: Boolean = history.threefoldRepetition && variant.repetitionEnabled

  def lastActionPlayer: Option[Player] = history.lastAction
    .flatMap {
      case m: Uci.Move => Some(m.dest)
      case d: Uci.Drop => Some(d.pos)
      // TODO: this probably needs to be fixed?
      case _           => sys.error("Dice Rolls are not supported (lastActionPlayer)")
    }
    .flatMap(apply)
    .map(_.player)

  override def toString = s"$variant Position after ${history.recentTurnUciString}"

  lazy val actors: Map[Pos, Actor] = pieces.map { case (a, piece) => (a, Actor(piece, a, this)) }

  lazy val posMap: Map[Piece, Iterable[Pos]] = pieces.groupMap(_._2)(_._1)

  lazy val piecesOnBoardCount: Int = pieces.keys.size
}

object Board {
  def apply(pieces: Iterable[(Pos, Piece)], variant: Variant): Board =
    Board(pieces.toMap, History(), variant)

  def init(variant: Variant): Board = Board(variant.pieces, variant)

  // def empty(variant: Variant): Board = Board(Nil, variant)
}