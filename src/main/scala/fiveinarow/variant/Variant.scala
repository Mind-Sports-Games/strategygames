package strategygames.fiveinarow.variant

import cats.data.Validated
import cats.syntax.option._
import scalalib.extensions.*

import strategygames.fiveinarow._
import strategygames.fiveinarow.format.FEN
import strategygames.{ GameFamily, Player }

import scala.annotation.nowarn

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

  def hasAnalysisBoard: Boolean = true
  def hasFishnet: Boolean       = false

  def p1IsBetterVariant: Boolean = false
  def blindModeVariant: Boolean  = false

  def materialImbalanceVariant: Boolean = false

  def dropsVariant: Boolean = true

  def canOfferDraw: Boolean = true

  def repetitionEnabled: Boolean = false

  def perfId: Int
  def perfIcon: Char

  def initialFen: FEN = format.Forsyth.initial

  def pieces: PieceMap = Map.empty

  def startPlayer: Player = P1

  def winningLine(length: Int): Boolean = length >= 5

  private val lineDirections: List[(Int, Int)] = List((1, 0), (0, 1), (1, 1), (1, -1))

  private def runLength(board: Board, from: Pos, role: Role, dx: Int, dy: Int): Int =
    Iterator
      .iterate(Option(from))(_.flatMap(_.step(dx, dy)))
      .takeWhile(_.flatMap(board.apply).exists(_.role == role))
      .size

  // counted only from the first stone of each run, so a long line is measured once
  def colourWithLine(board: Board): Option[Role] =
    board.pieces.collectFirst {
      case (pos, piece) if lineDirections.exists { case (dx, dy) =>
            !pos.step(-dx, -dy).flatMap(board.apply).exists(_.role == piece.role) &&
            winningLine(runLength(board, pos, piece.role, dx, dy))
          } =>
        piece.role
    }

  private def nextStep(board: Board): OpeningStep = board.openingStep match {
    case OpeningStep.Opening if board.pieces.size == 3    => OpeningStep.Choice
    case OpeningStep.Swap2Drops if board.pieces.size == 5 => OpeningStep.FinalChoice
    case OpeningStep.Opening | OpeningStep.Swap2Drops     => board.openingStep
    case _                                                => OpeningStep.Play
  }

  private def afterDrop(placed: Board): Board = {
    val stepped = placed.withOpeningStep(nextStep(placed))
    if (stepped.openingStep == OpeningStep.FinalChoice) stepped.swapColours else stepped
  }

  def validDrops(situation: Situation): List[Drop] =
    if (situation.end) List.empty
    else situation.board.emptyPositions.flatMap(drop(situation, situation.nextColour, _).toOption)

  def drop(situation: Situation, role: Role, pos: Pos): Validated[String, Drop] =
    if (situation.end) Validated.invalid("The game is over")
    else if (role != situation.nextColour) Validated.invalid(s"The next stone is ${situation.nextColour}")
    else
      situation.board
        .place(role, pos)
        .map(placed => Drop(Piece(situation.board.seatOf(role), role), pos, situation, afterDrop(placed)))
        .toValid(s"${pos} is occupied")

  def canSwap(situation: Situation): Boolean =
    !situation.end && (situation.openingStep match {
      case OpeningStep.Choice | OpeningStep.FinalChoice => true
      case _                                            => false
    })

  def canSwap2(situation: Situation): Boolean =
    !situation.end && situation.openingStep == OpeningStep.Choice

  def swap(situation: Situation): Validated[String, Swap] =
    if (!canSwap(situation)) Validated.invalid("A swap is only offered at a choice in the opening")
    else
      Validated.valid(
        Swap(situation, situation.board.swapColours.withOpeningStep(OpeningStep.Play))
      )

  def swap2(situation: Situation): Validated[String, Swap2] =
    if (!canSwap2(situation)) Validated.invalid("Swap2 is only offered in reply to the opening")
    else Validated.valid(Swap2(situation, situation.board.withOpeningStep(OpeningStep.Swap2Drops)))

  def hasMoveEffects = false

  def specialEnd(situation: Situation): Boolean =
    colourWithLine(situation.board).isDefined || situation.board.isFull

  def specialDraw(situation: Situation): Boolean =
    colourWithLine(situation.board).isEmpty && situation.board.isFull

  def winner(situation: Situation): Option[Player] =
    colourWithLine(situation.board).map(situation.board.seatOf)

  def materialImbalance(@nowarn board: Board): Int = 0

  def valid(@nowarn board: Board, @nowarn strict: Boolean): Boolean = true

  val roles: List[Role] = Role.all

  lazy val rolesByPgn: Map[Char, Role] = roles.map(r => (r.pgn, r)).to(Map)

  def defaultRole: Role = Role.defaultRole

  def gameFamily: GameFamily

  override def toString = s"Variant($name)"

  override def equals(that: Any): Boolean = this eq that.asInstanceOf[AnyRef]

  override def hashCode: Int = id

}

object Variant {

  lazy val all: List[Variant] = List(Gomoku)

  val byId  = all.map(v => (v.id, v)).toMap
  val byKey = all.map(v => (v.key, v)).toMap

  val default = Gomoku

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
