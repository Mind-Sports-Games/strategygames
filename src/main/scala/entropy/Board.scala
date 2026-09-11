package strategygames.entropy

import scalalib.extensions.*

import strategygames.Player

import variant.Variant

case class Board(
    pieces: PieceMap,
    history: History,
    variant: Variant,
    pocketData: Option[PocketData] = None
) {

  def apply(at: Pos): Option[Piece] = pieces get at
  def apply(file: File, rank: Rank) = pieces get Pos(file, rank)

  def boardSize = variant.boardSize

  def piecesOf(player: Player): PieceMap = pieces filter (_._2 is player)

  lazy val posMap: Map[Piece, Iterable[Pos]] = pieces.groupMap(_._2)(_._1)

  def empty(pos: Pos): Boolean = !pieces.contains(pos)

  lazy val emptyPositions: List[Pos] = Pos.all.filterNot(pieces.contains)

  // Pos.all is in a1-to-g7 order, so the head is the square a flagged Chaos must use
  lazy val firstEmptyPosition: Option[Pos] = emptyPositions.headOption

  lazy val isFull: Boolean = emptyPositions.isEmpty

  def round: Int = history.round

  // the role a player holds this round; P1 opens as Chaos, and they swap for round two
  def chaosPlayer: Player = if (round == 1) Player.P1 else Player.P2
  def orderPlayer: Player = !chaosPlayer

  def counterInPocket(player: Player): Option[Role] =
    pocketData.flatMap(_.pockets(player).roles.headOption).collect { case strategygames.Role.EntropyRole(r) =>
      r
    }

  lazy val guaranteedScore: Int = GuaranteedScore(this)

  // the round's score is the board's guaranteed score rewritten, never added to,
  // so no pattern is ever paid for twice
  def withScoreUpdated: Board = {
    val scored = guaranteedScore
    updateHistory { h =>
      h.copy(
        score =
          if (orderPlayer == Player.P1) h.score.copy(p1 = scored)
          else h.score.copy(p2 = scored)
      )
    }
  }

  // a round closes once Order has taken its turn on a full board, which is the position
  // worth the most; the last round keeps its board rather than clearing it
  def endRoundIfComplete(actingPlayer: Player): Board =
    if (isFull && actingPlayer == orderPlayer)
      (if (round >= 2) this else clearForNextRound)
        .updateHistory(h => h.copy(round = h.round + 1))
    else this

  def afterTurnBy(actingPlayer: Player): Board =
    withScoreUpdated.endRoundIfComplete(actingPlayer)

  def move(orig: Pos, dest: Pos): Option[Board] =
    if (pieces.contains(dest)) None
    else
      pieces.get(orig).map { piece =>
        copy(pieces = pieces - orig + (dest -> piece))
      }

  def place(piece: Piece, at: Pos): Option[Board] =
    if (pieces contains at) None
    else Some(copy(pieces = pieces + (at -> piece)))

  // a round ends with a full board; the next one starts from nothing
  def clearForNextRound: Board =
    copy(
      pieces = Map.empty,
      pocketData = Some(PocketData.init)
    )

  def withHistory(h: History): Board       = copy(history = h)
  def updateHistory(f: History => History) = copy(history = f(history))

  def withVariant(v: Variant): Board = copy(variant = v)

  def withPocketData(data: PocketData)                   = copy(pocketData = Option(data))
  def withPocketData(data: Option[PocketData])           = copy(pocketData = data)
  def withPocketData(f: PocketData => PocketData): Board =
    withPocketData(f(pocketData | PocketData.init))

  def ensurePocketData = withPocketData(pocketData | PocketData.init)

  def situationOf(player: Player) = Situation(this, player)

  def valid(strict: Boolean) = variant.valid(this, strict)

  def materialImbalance: Int = variant.materialImbalance(this)

  override def toString = s"$variant Position after ${history.recentTurnUciString}"
}

object Board {

  def apply(pieces: Iterable[(Pos, Piece)], variant: Variant): Board =
    Board(pieces.toMap, History(), variant, Some(PocketData.init))

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
    val all: List[BoardSize] = List(Dim7x7)
  }

  case object Dim7x7
      extends BoardSize(
        width = 7,
        height = 7
      )

}
