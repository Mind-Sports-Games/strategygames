package strategygames.fiveinarow

import strategygames.{ Player, Score }

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

  lazy val emptyPositions: List[Pos] = boardSize.positions.filterNot(pieces.contains)

  lazy val isFull: Boolean = emptyPositions.isEmpty

  lazy val drawn: Boolean = isFull || variant.noFivePossible(this)

  def count(role: Role): Int = pieces.values.count(_.role == role)

  def blackSeat: Player = history.blackSeat

  def openingStep: OpeningStep = history.openingStep

  def score: Score = history.score

  def seatOf(role: Role): Player = if (role == BlackStone) blackSeat else !blackSeat

  def dropped(role: Role): Int = count(role) + score(seatOf(role.opposite))

  lazy val droppedCount: Int = dropped(BlackStone) + dropped(WhiteStone)

  // black and white alternate from the first stone, so the stones dropped of each say which comes next
  lazy val nextColour: Role = if (dropped(BlackStone) == dropped(WhiteStone)) BlackStone else WhiteStone

  def offered: List[Pos] = history.offered

  lazy val playerToMove: Player = openingStep match {
    case OpeningStep.Opening    => Player.P1
    case OpeningStep.Swap2Drops => Player.P2
    case OpeningStep.Picking    => seatOf(nextColour.opposite)
    case _                      => seatOf(nextColour)
  }

  lazy val centre: Pos = Pos.at(boardSize.width / 2, boardSize.height / 2).get

  def place(role: Role, at: Pos): Option[Board] =
    if (pieces.contains(at) || !boardSize.onBoard(at)) None
    else Some(copy(pieces = pieces + (at -> Piece(seatOf(role), role))))

  def offer(at: Pos): Board = updateHistory(h => h.copy(offered = (h.offered :+ at).sortBy(_.index)))

  def keepOffered(at: Pos): Option[Board] =
    if (!offered.contains(at)) None
    else place(BlackStone, at).map(_.updateHistory(_.copy(offered = List.empty)))

  def capture(by: Role, stones: List[Pos]): Board =
    if (stones.isEmpty) this
    else
      copy(pieces = pieces -- stones)
        .updateHistory(h => h.copy(score = h.score.add(seatOf(by), stones.size)))

  // the stones and the captures keep their colour; each seat takes over the colour the other held
  def swapColours: Board = {
    val swapped = history.copy(blackSeat = !blackSeat, score = Score(score.p2, score.p1))
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

  def init(variant: Variant): Board =
    Board(variant.pieces, History(openingStep = variant.opening.initialStep), variant)

  sealed abstract class BoardSize(
      val width: Int,
      val height: Int
  ) {

    val key   = s"${width}x${height}"
    val sizes = List(width, height)

    def onBoard(pos: Pos): Boolean = pos.file.index < width && pos.rank.index < height

    lazy val positions: List[Pos] = Pos.all.filter(onBoard)

    lazy val files: List[File] = File.all.take(width)

    lazy val ranks: List[Rank] = Rank.all.take(height)

    override def toString = key

  }

  object BoardSize {
    val all: List[BoardSize] = List(Dim15x15, Dim19x19)
  }

  case object Dim15x15
      extends BoardSize(
        width = 15,
        height = 15
      )

  case object Dim19x19
      extends BoardSize(
        width = 19,
        height = 19
      )

}
