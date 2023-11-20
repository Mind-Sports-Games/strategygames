package strategygames.backgammon

import strategygames.Player

import variant.Variant

case class Board(
    pieces: PieceMap,
    history: History,
    variant: Variant
) {

  def apply(at: Pos): Option[Piece] = (pieces get at).map(_._1)
  def apply(file: File, rank: Rank) = pieces get Pos(file, rank)

  lazy val actors: Map[Pos, Actor] = pieces map { case (pos, (piece, count)) =>
    (pos, Actor(piece, pos, this))
  }

  lazy val stoneCount: Int                    =
    pieces
      .filter { case (_, (piece, _)) => piece.role == Stone }
      .map { case (_, (_, count)) => count }
      .sum
  lazy val playerStoneCount: Map[Player, Int] = Player.all.map { p =>
    (
      p,
      pieces
        .filter { case (_, (piece, _)) => piece.role == Stone && piece.player == p }
        .map { case (_, (_, count)) => count }
        .sum
    )
  }.toMap

  lazy val posMap: Map[(Piece, Int), Iterable[Pos]] = pieces.groupMap(_._2)(_._1)

  lazy val piecesOnBoardCount: Int                    = pieces.keys.size
  lazy val playerPiecesOnBoardCount: Map[Player, Int] = Player.all.map { p =>
    (p, pieces.collect { case (pos, (piece, count)) if piece.player == p => (pos, (piece, count)) }.size)
  }.toMap

  def withHistory(h: History): Board       = copy(history = h)
  def updateHistory(f: History => History) = copy(history = f(history))

  def withVariant(v: Variant): Board =
    if (v.dropsVariant) copy(variant = v)
    else copy(variant = v)

  def situationOf(player: Player) = Situation(this, player)

  def valid(strict: Boolean) = variant.valid(this, strict)

  def materialImbalance: Int = variant.materialImbalance(this)

  override def toString = s"$variant Position after ${history.lastMove}"
}

object Board {

  def apply(pieces: Iterable[(Pos, (Piece, Int))], variant: Variant): Board =
    Board(pieces.toMap, History(), variant)

  def init(variant: Variant): Board = Board(variant.pieces, variant)

  // def empty(variant: Variant): Board = Board(Nil, variant)

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
    val all: List[BoardSize] = List(Dim9x2)
  }

  case object Dim9x2
      extends BoardSize(
        width = 9,
        height = 2
      )

}
