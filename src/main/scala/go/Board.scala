package strategygames.go

import strategygames.{ Player, Score }

import format.Uci
import variant.Variant
import scalalib.extensions.*

case class Board(
    pieces: PieceMap,
    history: History,
    variant: Variant,
    pocketData: Option[PocketData] = None,
    // NOTE: komi belongs to the game rather than the variant. A handicap game carries its own in its
    // fen, and scoring a finished game under the variant's value can award it to the wrong player.
    komi: Double,
    ko: Option[Pos] = None,
    consecutivePasses: Int = 0,
    deadStonesSelected: Boolean = false,
    // NOTE: the starting fen and move list lila restored this board from, kept so that lila can read
    // the starting fen back. Play advances the fields above and leaves this as it was at the restore.
    position: Option[StoredPosition] = None
) {

  def apply(at: Pos): Option[Piece] = pieces get at
  def apply(file: File, rank: Rank) = pieces get Pos(file, rank)

  lazy val actors: Map[Pos, Actor] = pieces map { case (pos, piece) =>
    (pos, Actor(piece, pos, this))
  }

  lazy val posMap: Map[Piece, Iterable[Pos]] = pieces.groupMap(_._2)(_._1)

  lazy val piecesOnBoardCount: Int                    = pieces.keys.size
  lazy val playerPiecesOnBoardCount: Map[Player, Int] = Player.all.map { p =>
    (p, pieces.collect { case (pos, piece) if piece.player == p => (pos, piece) }.size)
  }.toMap

  lazy val areaScore: Score = variant.areaScore(this)

  def withHistory(h: History): Board       = copy(history = h)
  def updateHistory(f: History => History) = copy(history = f(history))

  def withVariant(v: Variant): Board =
    if (v.dropsVariant) copy(variant = v).ensurePocketData
    else copy(variant = v)

  def withPocketData(data: PocketData)                   = copy(pocketData = Option(data))
  def withPocketData(data: Option[PocketData])           = copy(pocketData = data)
  def withPocketData(f: PocketData => PocketData): Board =
    withPocketData(f(pocketData | PocketData.init))

  def ensurePocketData = withPocketData(pocketData | PocketData.init)

  // NOTE: lila reads `board.toGo.apiPosition.initialFen` in `SgfDump` and `Rematcher`, which is what
  // holds this name in place. A board that was never restored answers with the variant's own start.
  // TODO(lila): read `position` instead, and this accessor can go.
  def apiPosition: StoredPosition = position getOrElse StoredPosition(variant.initialFen, List())

  def uciMoves: List[String] = position.fold(List.empty[String])(_.uciMoves)

  def withPosition(p: Option[StoredPosition]): Board =
    copy(position = p, komi = Board.restoredKomi(variant, p))

  def passed: Board = copy(ko = None, consecutivePasses = consecutivePasses + 1)

  def settled: Board =
    copy(ko = None, consecutivePasses = 0, deadStonesSelected = true).withHistoryStartingHere

  def stonePlaced: Board = copy(consecutivePasses = 0)

  def positionHash: Long = Hash.positionHash(this)

  def withHistoryStartingHere: Board = updateHistory(_.startingAtPosition(positionHash))

  def withKo(point: Option[Pos]): Board = copy(ko = point)

  def situationOf(player: Player) = Situation(this, player)

  def valid(strict: Boolean) = variant.valid(this, strict)

  def materialImbalance: Int = variant.materialImbalance(this)

  override def toString = s"$variant Position after ${history.recentTurnUciString}"
}

object Board {

  def apply(pieces: Iterable[(Pos, Piece)], variant: Variant): Board =
    Board(
      pieces = pieces.toMap,
      history = History(),
      variant = variant,
      pocketData = variantPocketData(variant),
      komi = variant.komi
    ).withHistoryStartingHere

  // NOTE: lila stores a go game as a starting fen plus a list of uci moves, so this overload takes the
  // parameters `readGoGame` supplies and works the position state out from the move list. See
  // `docs/go-refactor.md`.
  // TODO(lila): persist the position state, and this overload can be deleted along with `StoredPosition`.
  def apply(
      pieces: PieceMap,
      history: History,
      variant: Variant,
      pocketData: Option[PocketData],
      uciMoves: List[String],
      position: Option[StoredPosition]
  ): Board = {
    val resumedFrom = position orElse Some(StoredPosition(variant.initialFen, uciMoves))
    Board(
      pieces = pieces,
      history = history.copy(halfMoveClock = restoredPlyCount(uciMoves, position)),
      variant = variant,
      pocketData = pocketData,
      komi = restoredKomi(variant, position),
      ko = restoredKo(pieces, variant, uciMoves, position),
      consecutivePasses = restoredPassCount(uciMoves),
      deadStonesSelected = restoredSettlement(uciMoves),
      position = resumedFrom.map(_.copy(uciMoves = uciMoves))
    )
  }

  def init(variant: Variant): Board = Board(variant.pieces, variant)

  // def empty(variant: Variant): Board = Board(Nil, variant)

  private def variantPocketData(variant: Variant) =
    (variant.dropsVariant) option PocketData.init

  private[go] def restoredKomi(variant: Variant, position: Option[StoredPosition]): Double =
    position.map(_.initialFen.komi) getOrElse variant.komi

  // NOTE: lila derives `halfMoveClock` with a chess heuristic that answers 0 for every go game, so
  // the count comes from the starting fen's ply plus the moves played since. `Forsyth` exports the
  // full move number from it.
  private[go] def restoredPlyCount(uciMoves: List[String], position: Option[StoredPosition]): Int =
    position.flatMap(_.initialFen.ply).getOrElse(0).max(0) + uciMoves.size

  // NOTE: four consecutive passes settle a game that is played or replayed, but the restore counts
  // the run instead, so that games standing at four passes in lila's database stay playable.
  private[go] def restoredPassCount(uciMoves: List[String]): Int =
    if (restoredSettlement(uciMoves)) 0
    else uciMoves.reverseIterator.takeWhile(Uci.Pass.passR.matches).size

  private[go] def restoredSettlement(uciMoves: List[String]): Boolean =
    uciMoves.lastOption.exists(Uci.SelectSquares.selectSquaresR.matches)

  // NOTE: a ko point is a fact about the position before the last move, so establishing one takes a
  // replay. Gating on the two conditions that read off the current stones keeps that replay rare.
  private[go] def restoredKo(
      pieces: PieceMap,
      variant: Variant,
      uciMoves: List[String],
      position: Option[StoredPosition]
  ): Option[Pos] =
    lastPlacement(uciMoves)
      .filter(koIsPossibleAt(pieces, variant, _))
      .flatMap(_ => replayed(variant, uciMoves, position).flatMap(_.ko))

  private def lastPlacement(uciMoves: List[String]): Option[Pos] =
    uciMoves.lastOption.collect { case Uci.Drop.dropR(_, dest) => dest }.flatMap(Pos.fromKey)

  private def koIsPossibleAt(pieces: PieceMap, variant: Variant, at: Pos): Boolean = {
    val probe = Board(
      pieces = pieces,
      history = History(),
      variant = variant,
      pocketData = None,
      komi = variant.komi
    )
    val chain = Chain.at(probe, at)
    chain.size == 1 && Chain.liberties(probe, chain).size == 1
  }

  private def replayed(
      variant: Variant,
      uciMoves: List[String],
      position: Option[StoredPosition]
  ): Option[Board] =
    Replay
      .situationsFromUci(uciMoves.flatMap(Uci.apply), position.map(_.initialFen), variant)
      .toOption
      .flatMap(_.lastOption)
      .map(_.board)

  sealed abstract class BoardSize(
      val width: Int,
      val height: Int
  ) {

    val key   = s"${width}x${height}"
    val sizes = List(width, height)

    val validPos: List[Pos] =
      Pos.all.filter(onBoard)

    val neighbours: Array[List[Pos]] = {
      val table = Array.fill(Pos.allSize)(List.empty[Pos])
      validPos.foreach(pos => table(pos.index) = cardinalNeighboursOf(pos))
      table
    }

    def onBoard(pos: Pos): Boolean = pos.file.index < width && pos.rank.index < height

    private def cardinalNeighboursOf(pos: Pos): List[Pos] =
      List(
        Pos.at(pos.file.index - 1, pos.rank.index),
        Pos.at(pos.file.index + 1, pos.rank.index),
        Pos.at(pos.file.index, pos.rank.index - 1),
        Pos.at(pos.file.index, pos.rank.index + 1)
      ).flatten.filter(onBoard)

    override def toString = key

  }

  object BoardSize {
    val all: List[BoardSize] = List(Dim9x9, Dim13x13, Dim19x19)
  }

  case object Dim9x9
      extends BoardSize(
        width = 9,
        height = 9
      )

  case object Dim13x13
      extends BoardSize(
        width = 13,
        height = 13
      )

  case object Dim19x19
      extends BoardSize(
        width = 19,
        height = 19
      )

}
