package strategygames.oware

import strategygames.Player
import format.FEN

import cats.implicits._

import variant.Variant

case class Board(
    pieces: PieceMap,
    history: History,
    variant: Variant,
    uciMoves: List[String] = List(),
    position: Option[Api.Position] = None
) {

  def apply(at: Pos): Option[Piece] = pieces get at
  def apply(file: File, rank: Rank) = pieces get Pos(file, rank)

  lazy val posMap: Map[Piece, Iterable[Pos]] = pieces.groupMap(_._2)(_._1)

  lazy val piecesOnBoardCount: Int = pieces.keys.size
  lazy val playerPiecesOnBoardCount: Map[Player, Int] = Player.all.map{
    p => (p, pieces.collect{ case (pos, piece) if piece.player == p  => (pos, piece)}.size)
  }.toMap

  def withHistory(h: History): Board = copy(history = h)
  def updateHistory(f: History => History) = copy(history = f(history))

  def withVariant(v: Variant): Board =
    if (v.dropsVariant) copy(variant = v)
    else copy(variant = v)

  def situationOf(player: Player) = Situation(this, player)

  def valid(strict: Boolean) = variant.valid(this, strict)

  def materialImbalance: Int = variant.materialImbalance(this)

  lazy val apiPosition = position match {
    case Some(position) => position
    case None           => Api.positionFromVariantAndMoves(variant, uciMoves)
  }

  override def toString = s"$variant Position after ${history.lastMove}"
}

object Board {

  def apply(pieces: Iterable[(Pos, Piece)], variant: Variant): Board =
    Board(pieces.toMap, History(), variant)

  def init(variant: Variant): Board = Board(variant.pieces, variant)

  //def empty(variant: Variant): Board = Board(Nil, variant)

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
    val all: List[BoardSize] = List(Dim6x2)
  }

  case object Dim6x2
      extends BoardSize(
        width = 6,
        height = 2
      )

}
