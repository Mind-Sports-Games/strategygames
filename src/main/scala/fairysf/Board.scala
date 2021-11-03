package strategygames.fairysf

import strategygames.Color

import variant.Variant

case class Board(
    pieces: PieceMap,
    history: History,
    variant: Variant,
    pocketData: Option[PocketData] = None,
    uciMoves: List[String] = List()
) {

  def apply(at: Pos): Option[Piece] = pieces get at
  def apply(file: File, rank: Rank) = pieces get Pos(file, rank)

  def withHistory(h: History): Board = copy(history = h)
  def updateHistory(f: History => History) = copy(history = f(history))

  def withVariant(v: Variant): Board =
    if (v.dropsVariant) copy(variant = v).ensurePocketData
    else copy(variant = v)

  def withPocketData(data: PocketData)         = copy(pocketData = Option(data))
  def withPocketData(data: Option[PocketData]) = copy(pocketData = data)
  def withPocketData(f: PocketData => PocketData): Board =
    withPocketData(f(pocketData | PocketData.init))

  def ensurePocketData = withPocketData(pocketData | PocketData.init)

  def situationOf(color: Color) = Situation(this, color)

  def valid(strict: Boolean) = variant.valid(this, strict)

  def materialImbalance: Int = variant.materialImbalance(this)

  override def toString = s"$variant Position after ${history.lastMove}"
}

object Board {

  def apply(pieces: Iterable[(Pos, Piece)], variant: Variant): Board =
    Board(pieces.toMap, History(), variant, variantPocketData(variant))

  def init(variant: Variant): Board = Board(variant.pieces, variant)

  //def empty(variant: Variant): Board = Board(Nil, variant)

  private def variantPocketData(variant: Variant) =
    (variant.dropsVariant) option PocketData.init

  sealed abstract class BoardSize(
    val width: Int,
    val height: Int
  ) {

    val key   = (width * height).toString
    val sizes = List(width, height)

    val validPos: List[Pos] =
      Pos.all.filter(p => p.file.index < width && p.rank.index < height)

    override def toString = s"${width}x${height}"

  }

  object BoardSize {
    val all: List[BoardSize] = List(Dim8x8, Dim9x9, Dim9x10)
  }

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

}
