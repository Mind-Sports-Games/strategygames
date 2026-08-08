package strategygames.go

import strategygames.Player

import variant.Variant
import scalalib.extensions.*

case class Board(
    pieces: PieceMap,
    history: History,
    variant: Variant,
    pocketData: Option[PocketData] = None,
    komi: Double,
    ko: Option[Pos] = None,
    consecutivePasses: Int = 0,
    deadStonesSelected: Boolean = false,
    uciMoves: List[String] = List(),
    position: Option[Api.Position] = None
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

  def withPosition(p: Option[Api.Position]): Board = copy(position = p)

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

  def passed: Board = copy(ko = None, consecutivePasses = consecutivePasses + 1)

  def settled: Board =
    copy(ko = None, consecutivePasses = 0, deadStonesSelected = true).withHistoryStartingHere

  def stonePlaced: Board = copy(consecutivePasses = 0)

  def positionHash: Long = Hash.positionHash(this)

  def withHistoryStartingHere: Board = updateHistory(_.startingAtPosition(positionHash))

  def withKo(point: Option[Pos]): Board = copy(ko = point)

  def withKoOf(p: Api.Position): Board = withKo(p.fen.ko)

  def situationOf(player: Player) = Situation(this, player)

  def valid(strict: Boolean) = variant.valid(this, strict)

  def materialImbalance: Int = variant.materialImbalance(this)

  // This won't work if the Board has been generated FromPosition. Will need to generate from FEN
  // However generating from FEN can't be done all the time as we would like uciMoves to help with repetition
  // Future problem when we come to deal with FromPosition for go games
  lazy val apiPosition = position match {
    case Some(position) => position
    case None           => Api.positionFromVariantAndMoves(variant, uciMoves)
  }

  def afterDrop(player: Player, dest: Pos): Board = {
    val stonesBefore = apiPosition.pieceMap
    val uciMove      =
      s"${Role.defaultRole.forsyth}@${dest.key}"
    val newPosition  = apiPosition.makeMovesWithPosUnchecked(List(uciMove), apiPosition.deepCopy)
    val stonesAfter  = newPosition.pieceMap
    copy(
      pieces = stonesAfter,
      uciMoves = uciMoves :+ uciMove,
      pocketData = newPosition.pocketData,
      position = Some(newPosition)
    ).stonePlaced
      .withKoOf(newPosition)
      .withHistory(
        history
          .copy(
            // lastTurn handled in action.finalizeAfter
            score = newPosition.fenScore,
            captures = history.captures.add(
              player,
              stonesBefore.size - stonesAfter.size + 1
            ),
            halfMoveClock = history.halfMoveClock + 1
          )
          .afterPosition(
            hashAfterPlacing(Piece(player, Role.defaultRole), dest, stonesBefore -- stonesAfter.keys)
          )
      )

  }

  private def hashAfterPlacing(stone: Piece, at: Pos, captured: PieceMap): Long =
    captured.foldLeft(history.currentPosition.getOrElse(positionHash) ^ Hash.mask(stone, at)) {
      case (hash, (pos, piece)) => hash ^ Hash.mask(piece, pos)
    }

  override def toString = s"$variant Position after ${history.recentTurnUciString}"
}

object Board {

  def apply(pieces: Iterable[(Pos, Piece)], variant: Variant): Board =
    Board(
      pieces.toMap,
      History(),
      variant,
      variantPocketData(variant),
      variant.komi
    ).withHistoryStartingHere

  def init(variant: Variant): Board = Board(variant.pieces, variant)

  // def empty(variant: Variant): Board = Board(Nil, variant)

  private def variantPocketData(variant: Variant) =
    (variant.dropsVariant) option PocketData.init

  sealed abstract class BoardSize(
      val width: Int,
      val height: Int
  ) {

    val key   = s"${width}x${height}"
    val sizes = List(width, height)

    val validPos: List[Pos] =
      Pos.all.filter(onBoard)

    val neighbours: Array[List[Pos]] = {
      val table = Array.fill(Pos.all.size)(List.empty[Pos])
      validPos.foreach(pos => table(pos.index) = cardinalNeighboursOf(pos))
      table
    }

    private def onBoard(pos: Pos): Boolean = pos.file.index < width && pos.rank.index < height

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
