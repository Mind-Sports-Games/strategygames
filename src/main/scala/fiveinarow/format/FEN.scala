package strategygames.fiveinarow.format

import strategygames.Player
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

  def fullMove: Option[Int] = field(4).flatMap(_.toIntOption)

  def stones: List[(Pos, Role)] =
    boardPart
      .split('/')
      .toList
      .zipWithIndex
      .flatMap { case (rankStr, rankIndexFromTop) =>
        val rankIndex      = Rank.allSize - 1 - rankIndexFromTop
        val (stones, _, _) = rankStr.foldLeft((List.empty[(Pos, Role)], 0, 0)) {
          case ((acc, fileIndex, run), c) if c.isDigit => (acc, fileIndex, run * 10 + c.asDigit)
          case ((acc, fileIndex, run), c)              =>
            val at     = fileIndex + run
            val placed = for {
              file <- File(at)
              rank <- Rank(rankIndex)
              role <- Role.forsyth(c)
            } yield Pos(file, rank) -> role
            (placed.toList ::: acc, at + 1, 0)
        }
        stones
      }

  def board(variant: Variant): Board = {
    val history          = History(blackSeat = blackSeat, openingStep = openingStep)
    val pieces: PieceMap = stones.map { case (pos, role) =>
      pos -> Piece(if (role == strategygames.fiveinarow.BlackStone) blackSeat else !blackSeat, role)
    }.toMap
    Board(pieces, history, variant)
  }

  def player: Option[Player] = Some(board(Variant.default).playerToMove)

  def ply: Option[Int] =
    fullMove map { fm =>
      fm * 2 - (if (player.exists(_.p1)) 2 else 1)
    }

}
