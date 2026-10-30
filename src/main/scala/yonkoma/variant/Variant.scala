package strategygames.yonkoma.variant

import cats.data.Validated
import cats.syntax.option._
import scalalib.extensions.*

import strategygames.yonkoma._
import strategygames.yonkoma.format.FEN
import strategygames.{ GameFamily, Player }

import scala.annotation.{ nowarn, tailrec }

// Correctness depends on singletons for each variant ID
abstract class Variant private[variant] (
    val id: Int,
    val key: String,
    val name: String,
    val standardInitialPosition: Boolean,
    val boardSize: Board.BoardSize
) {

  def exotic = true

  def baseVariant: Boolean        = false
  def fenVariant: Boolean         = false
  def variableInitialFen: Boolean = false

  def hasAnalysisBoard: Boolean = false
  def hasFishnet: Boolean       = false

  def p1IsBetterVariant: Boolean = false
  def blindModeVariant: Boolean  = false

  def materialImbalanceVariant: Boolean = false

  def dropsVariant: Boolean = false

  def canOfferDraw: Boolean = false

  def repetitionEnabled: Boolean = false

  def perfId: Int
  def perfIcon: Char

  def startPlayer: Player = P1

  def width: Int  = boardSize.width
  def height: Int = boardSize.height

  // Red (P1) starts on the dark squares a1 and the far corner, Blue (P2) on the light
  // squares in the other two corners
  def pieces: PieceMap =
    List(
      Pos.at(0, 0)                  -> Piece(P1, Checker),
      Pos.at(width - 1, height - 1) -> Piece(P1, Checker),
      Pos.at(0, height - 1)         -> Piece(P2, Checker),
      Pos.at(width - 1, 0)          -> Piece(P2, Checker)
    ).collect { case (Some(pos), piece) => pos -> piece }.toMap

  def initialFen: FEN = format.Forsyth.>>(Situation(Board.init(this), startPlayer))

  private val diagonals: Directions = List(_.upLeft, _.upRight, _.downLeft, _.downRight)

  // the empty squares a checker can slide to along one diagonal, in order, stopping at
  // the first checker or the edge of the board
  private def slide(board: Board, from: Pos, dir: Direction): List[Pos] = {
    @tailrec def go(cur: Pos, acc: List[Pos]): List[Pos] =
      dir(cur) match {
        case Some(next) if board.onBoard(next) && board.empty(next) => go(next, next :: acc)
        case _                                                      => acc
      }
    go(from, Nil)
  }

  // A move must bring the moved checker closer to the two enemy checkers, so that the sum
  // of its king distances to them decreases, or keep that sum and take the checker farther
  // from its partner.
  private def allowed(from: Pos, to: Pos, partner: Option[Pos], enemies: List[Pos]): Boolean = {
    val sumBefore     = enemies.map(from.kingDistance).sum
    val sumAfter      = enemies.map(to.kingDistance).sum
    val partnerBefore = partner.fold(0)(from.kingDistance)
    val partnerAfter  = partner.fold(0)(to.kingDistance)
    sumAfter < sumBefore || (sumAfter == sumBefore && partnerAfter > partnerBefore)
  }

  def destinations(board: Board, player: Player, from: Pos): List[Pos] = {
    val mine    = board.checkersOf(player)
    val enemies = board.checkersOf(!player)
    val partner = mine.find(_ != from)
    diagonals
      .flatMap(slide(board, from, _))
      .filter(allowed(from, _, partner, enemies))
      .sortBy(_.index)
  }

  def validMoves(situation: Situation): Map[Pos, List[Move]] =
    if (situation.end) Map.empty
    else
      situation.board
        .piecesOf(situation.player)
        .map { case (pos, piece) =>
          pos -> destinations(situation.board, situation.player, pos).flatMap { dest =>
            situation.board.move(pos, dest).map(Move(piece, pos, dest, situation, _))
          }
        }
        .filter(_._2.nonEmpty)
        .to(Map)

  def move(situation: Situation, from: Pos, to: Pos): Validated[String, Move] =
    situation.moves
      .get(from)
      .flatMap(_.find(_.dest == to))
      .toValid(s"Not a valid move: ${from}${to}")

  // passing is not allowed, but a player with no legal move has the turn skipped
  def pass(situation: Situation): Validated[String, Pass] =
    if (situation.end) Validated.invalid("The game is over")
    else if (situation.moves.nonEmpty) Validated.invalid("You may not pass while you have a legal move")
    else Validated.valid(Pass(situation, situation.board))

  def hasMoveEffects = false

  def addVariantEffect(move: Move): Move = move

  // a crosscut: all four checkers in a 2x2 block, the two red checkers on one diagonal of
  // the block and the two blue checkers on the other
  def isCrosscut(board: Board): Boolean = {
    val red  = board.checkersOf(P1)
    val blue = board.checkersOf(P2)
    red.size == 2 && blue.size == 2 && {
      val all   = red ++ blue
      val files = all.map(_.file.index)
      val ranks = all.map(_.rank.index)
      files.max - files.min == 1 && ranks.max - ranks.min == 1 &&
      red(0).onSameDiagonal(red(1)) && blue(0).onSameDiagonal(blue(1))
    }
  }

  private def hasMoves(board: Board, player: Player): Boolean =
    board.checkersOf(player).exists(pos => destinations(board, player, pos).nonEmpty)

  // whoever completes a crosscut loses, so the player to move has won; a position in which
  // neither player can move cannot arise in play, and is counted as a loss for the player to
  // move so that no game can ever stall
  def specialEnd(situation: Situation): Boolean =
    isCrosscut(situation.board) || (
      !hasMoves(situation.board, situation.player) && !hasMoves(situation.board, !situation.player)
    )

  def specialDraw(@nowarn situation: Situation): Boolean = false

  def winner(situation: Situation): Option[Player] =
    if (isCrosscut(situation.board)) Some(situation.player)
    else if (specialEnd(situation)) Some(!situation.player)
    else None

  def materialImbalance(@nowarn board: Board): Int = 0

  // two checkers each, red ones on dark squares and blue ones on light squares
  def valid(board: Board, strict: Boolean): Boolean = {
    val red  = board.checkersOf(P1)
    val blue = board.checkersOf(P2)
    board.pieces.keys.forall(board.onBoard) && (!strict || {
      red.size == 2 && blue.size == 2 && red.forall(!_.isLight) && blue.forall(_.isLight)
    })
  }

  val roles: List[Role] = Role.all

  lazy val rolesByPgn: Map[Char, Role] = roles
    .map { r =>
      (r.pgn, r)
    }
    .to(Map)

  def defaultRole: Role = Role.defaultRole

  def gameFamily: GameFamily

  def updatePositionHashes(board: Board, action: Action, hash: PositionHash): PositionHash =
    Hash(Situation(board, !action.player)) ++ hash

  override def toString = s"Variant($name)"

  override def equals(that: Any): Boolean = this eq that.asInstanceOf[AnyRef]

  override def hashCode: Int = id

}

object Variant {

  lazy val all: List[Variant] = List(Yonkoma10x10, Yonkoma12x12)

  val byId  = all map { v =>
    (v.id, v)
  } toMap
  val byKey = all map { v =>
    (v.key, v)
  } toMap

  val default = Yonkoma10x10

  def apply(id: Int): Option[Variant]     = byId get id
  def apply(key: String): Option[Variant] = byKey get key
  def orDefault(id: Int): Variant         = apply(id) | default
  def orDefault(key: String): Variant     = apply(key) | default

  def byName(name: String): Option[Variant] =
    all find (_.name.toLowerCase == name.toLowerCase)

  def exists(id: Int): Boolean = byId contains id

  val openingSensibleVariants: Set[Variant] = Set()

  val divisionSensibleVariants: Set[Variant] = Set()

}
