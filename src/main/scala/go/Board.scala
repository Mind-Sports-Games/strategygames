package strategygames.go

import strategygames.Player
import strategygames.Score

import variant.Variant
import scalalib.extensions.*

case class Board(
    pieces: PieceMap,
    history: History,
    variant: Variant,
    pocketData: Option[PocketData] = None,
    uciMoves: List[String] = List(),
    position: Option[Api.Position] = None,
    // NOTE: komi belongs to the game rather than the variant. A handicap game carries its own in its
    // fen, and scoring a finished game under the variant's value can award it to the wrong player.
    komi: Double,
    ko: Option[Pos] = None,
    consecutivePasses: Int = 0,
    deadStonesSelected: Boolean = false
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

  // NOTE: `passed`, `settled` and `stonePlaced` are the only writers of `ko`, `consecutivePasses`
  // and `deadStonesSelected` on the action path, so they cannot drift apart as a game is played.
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

  // This won't work if the Board has been generated FromPosition. Will need to generate from FEN
  // However generating from FEN can't be done all the time as we would like uciMoves to help with repetition
  // Future problem when we come to deal with FromPosition for go games
  lazy val apiPosition = position match {
    case Some(position) => position
    case None           => Api.positionFromVariantAndMoves(variant, uciMoves)
  }

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

    val neighbours: Array[List[Pos]] =
      Array.tabulate(Pos.allSize)(index =>
        Pos(index).filter(onBoard).fold(List.empty[Pos])(cardinalNeighboursOf)
      )

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
