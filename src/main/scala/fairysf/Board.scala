package strategygames.fairysf

import strategygames.Player

import variant.Variant

case class Board(
    pieces: PieceMap,
    history: History,
    variant: Variant,
    pocketData: Option[PocketData] = None,
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

  def autoDraw: Boolean = variant.isInsufficientMaterial(this)

  def situationOf(player: Player) = Situation(this, player)

  def valid(strict: Boolean) = variant.valid(this, strict)

  def materialImbalance: Int = variant.materialImbalance(this)

  lazy val apiPosition = position match {
    case Some(position)                               => position
    case None if variant.recreateApiPositionFromMoves =>
      Api.positionFromVariantAndMoves(variant, uciMoves)
    case None                                         =>
      Api.positionFromVariantNameAndFEN(variant.fishnetKey, variant.exportBoardFen(this).value)
  }

  override def toString = s"$variant Position after ${history.recentTurnUciString}"
}

object Board {

  def apply(pieces: Iterable[(Pos, Piece)], variant: Variant): Board =
    Board(pieces.toMap, History(), variant, variantPocketData(variant))

  def init(variant: Variant): Board = Board(variant.pieces, variant)

  def empty(variant: Variant): Board = Board(Nil, variant)

  private def variantPocketData(variant: Variant) =
    (variant.dropsVariant) option PocketData.init

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
    val all: List[BoardSize] = List(Dim5x5, Dim7x7, Dim8x8, Dim9x9, Dim9x10, Dim10x10)
  }

  case object Dim5x5
      extends BoardSize(
        width = 5,
        height = 5
      )
  case object Dim7x7
      extends BoardSize(
        width = 7,
        height = 7
      )
  case object Dim8x8
      extends BoardSize(
        width = 8,
        height = 8
      )
  case object Dim9x9
      extends BoardSize(
        width = 9,
        height = 9
      )
  case object Dim9x10
      extends BoardSize(
        width = 9,
        height = 10
      )
  case object Dim10x10
      extends BoardSize(
        width = 10,
        height = 10
      )

}
