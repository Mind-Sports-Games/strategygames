package strategygames.fiveinarow

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

  def empty(pos: Pos): Boolean = !pieces.contains(pos)

  lazy val emptyPositions: List[Pos] = Pos.all.filterNot(pieces.contains)

  lazy val isFull: Boolean = emptyPositions.isEmpty

  def count(role: Role): Int = pieces.values.count(_.role == role)

  def blackSeat: Player = history.blackSeat

  def openingStep: OpeningStep = history.openingStep

  def seatOf(role: Role): Player = if (role == BlackStone) blackSeat else !blackSeat

  // black and white alternate from the first stone, so the counts alone say which colour comes next
  lazy val nextColour: Role = if (count(BlackStone) == count(WhiteStone)) BlackStone else WhiteStone

  // one seat drops both colours while the opening and the swap2 pair are being placed
  lazy val playerToMove: Player = openingStep match {
    case OpeningStep.Opening    => Player.P1
    case OpeningStep.Swap2Drops => Player.P2
    case _                      => seatOf(nextColour)
  }

  def place(role: Role, at: Pos): Option[Board] =
    if (pieces contains at) None
    else Some(copy(pieces = pieces + (at -> Piece(seatOf(role), role))))

  // the stones keep their colour; each seat takes over the colour the other held
  def swapColours: Board = {
    val swapped = history.copy(blackSeat = !blackSeat)
    copy(
      pieces = pieces.view.mapValues(p => p.copy(player = !p.player)).to(Map),
      history = swapped
    )
  }

  def withOpeningStep(step: OpeningStep): Board = updateHistory(_.copy(openingStep = step))

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

    override def toString = key

  }

  object BoardSize {
    val all: List[BoardSize] = List(Dim15x15)
  }

  case object Dim15x15
      extends BoardSize(
        width = 15,
        height = 15
      )

}
