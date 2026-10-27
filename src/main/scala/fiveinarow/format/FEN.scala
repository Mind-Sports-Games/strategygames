package strategygames.fiveinarow.format

import scalalib.extensions.*

import strategygames.{ Player, Score }
import strategygames.fiveinarow.{ Board, File, History, OpeningStep, Piece, PieceMap, Pos, Rank, Role }
import strategygames.fiveinarow.variant.Variant

final case class FEN(value: String) extends AnyVal {

  override def toString = value

  private def parts: Array[String] = value.split(' ')

  private def field(index: Int): Option[String] = parts.lift(index)

  def boardPart: String = field(0).getOrElse("")

  def nextColour: Option[Role] =
    field(1).flatMap(_.headOption).flatMap(c => Role.forsyth(c.toUpper))

  def blackSeat: Player = field(2) match {
    case Some("2") => Player.P2
    case _         => Player.P1
  }

  def openingStep: OpeningStep =
    field(3).flatMap(_.headOption).flatMap(OpeningStep.fromFen).getOrElse(OpeningStep.Play)

  private def hasScore: Boolean = parts.size >= 7

  def score: Score =
    if (hasScore)
      Score(field(4).flatMap(_.toIntOption).getOrElse(0), field(5).flatMap(_.toIntOption).getOrElse(0))
    else Score(0, 0)

  def fullMove: Option[Int] = field(if (hasScore) 6 else 4).flatMap(_.toIntOption)

  private def ranks: List[String] = boardPart.split('/').toList

  def boardSize: Option[Board.BoardSize] = Board.BoardSize.all.find(_.height == ranks.size)

  private def points: List[(Pos, Char)] =
    ranks.zipWithIndex
      .flatMap { case (rankStr, rankIndexFromTop) =>
        val rankIndex      = ranks.size - 1 - rankIndexFromTop
        val (points, _, _) = rankStr.foldLeft((List.empty[(Pos, Char)], 0, 0)) {
          case ((acc, fileIndex, run), c) if c.isDigit => (acc, fileIndex, run * 10 + c.asDigit)
          case ((acc, fileIndex, run), c)              =>
            val at     = fileIndex + run
            val placed = for {
              file <- File(at)
              rank <- Rank(rankIndex)
            } yield Pos(file, rank) -> c
            (placed.toList ::: acc, at + 1, 0)
        }
        points
      }

  def stones: List[(Pos, Role)] =
    points.flatMap { case (pos, c) => Role.forsyth(c).map(pos -> _) }

  def offered: List[Pos] = points.collect { case (pos, c) if c == FEN.offeredChar => pos }.sortBy(_.index)

  def board(variant: Variant): Board = {
    val history          =
      History(blackSeat = blackSeat, openingStep = openingStep, score = score, offered = offered)
    val pieces: PieceMap = stones.map { case (pos, role) =>
      pos -> Piece(if (role == strategygames.fiveinarow.BlackStone) blackSeat else !blackSeat, role)
    }.toMap
    Board(pieces, history, variant)
  }

  def player: Option[Player] = Some(board(Variant.forFen(this) | Variant.default).playerToMove)

  def ply: Option[Int] =
    fullMove map { fm =>
      fm * 2 - (if (player.exists(_.p1)) 2 else 1)
    }

}

object FEN {

  // a fifth stone offered in renju's opening, waiting for white's drop on one of the ten
  val offeredChar = 'b'

}
