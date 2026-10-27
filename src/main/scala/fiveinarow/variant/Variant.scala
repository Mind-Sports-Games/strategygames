package strategygames.fiveinarow.variant

import cats.data.Validated
import cats.syntax.option._
import scalalib.extensions.*

import strategygames.fiveinarow._
import strategygames.fiveinarow.format.FEN
import strategygames.{ GameFamily, Player, Status }

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

  def canOfferDraw: Boolean = false

  def drawOfferMinStones: Int = 75

  def repetitionEnabled: Boolean = false

  def perfId: Int
  def perfIcon: Char

  def initialFen: FEN = format.Forsyth.initial(this)

  def pieces: PieceMap = Map.empty

  def startPlayer: Player = P1

  def opening: Opening = Opening.Swap2

  def winningLine(@nowarn role: Role, length: Int): Boolean = length >= 5

  def captureTarget: Option[Int] = None

  def hasGameScore: Boolean = captureTarget.isDefined

  def forbiddenMoves: Boolean = false

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
            winningLine(piece.role, runLength(board, pos, piece.role, dx, dy))
          } =>
        piece.role
    }

  private val captureDirections: List[(Int, Int)] =
    lineDirections ::: lineDirections.map { case (dx, dy) => (-dx, -dy) }

  // exactly two of the other colour, closed off at the far end by the stone just dropped's colour
  def capturedBy(board: Board, role: Role, at: Pos): List[Pos] =
    if (captureTarget.isEmpty) List.empty
    else
      captureDirections.flatMap { case (dx, dy) =>
        val run = List(1, 2, 3).map(n => at.step(dx * n, dy * n).flatMap(board.apply).map(_.role))
        if (run == List(Some(role.opposite), Some(role.opposite), Some(role)))
          List(1, 2).flatMap(n => at.step(dx * n, dy * n))
        else List.empty
      }

  private case class FiveWindow(cells: List[Pos], ends: List[Pos])

  private lazy val fiveWindows: List[FiveWindow] =
    for {
      start    <- boardSize.positions
      (dx, dy) <- lineDirections
      cells     = (0 until 5).toList.flatMap(n => start.step(dx * n, dy * n)).filter(boardSize.onBoard)
      if cells.size == 5
    } yield FiveWindow(cells, List(start.step(-dx, -dy), start.step(dx * 5, dy * 5)).flatten)

  // a window that would join a stone of its own colour beyond either end can only ever make an overline
  private def windowOpenTo(board: Board, role: Role, window: FiveWindow): Boolean =
    window.cells.forall(board(_).forall(_.role == role)) &&
      (winningLine(role, 6) || !window.ends.exists(board(_).exists(_.role == role)))

  // each row splits into width / 5 separate stretches, and every one needs a stone of both colours
  private lazy val fewestStonesBlockingEveryFive: Int = 2 * boardSize.height * (boardSize.width / 5)

  // captures can reopen a blocked line, so only a variant without them can run out of fives
  def noFivePossible(board: Board): Boolean =
    captureTarget.isEmpty && board.pieces.size >= fewestStonesBlockingEveryFive &&
      !fiveWindows.exists(window => roles.exists(windowOpenTo(board, _, window)))

  def seatWithCaptures(board: Board): Option[Player] =
    captureTarget.flatMap(target => List(P1, P2).find(board.score(_) >= target))

  def isForbidden(board: Board, role: Role, at: Pos): Boolean =
    forbiddenMoves && role == BlackStone && Forbidden.isForbidden(board, at)

  def forbiddenDrops(situation: Situation): List[Pos] =
    if (
      !forbiddenMoves || situation.end || situation.nextColour != BlackStone || opening.offeringStep(
        situation.board
      )
    )
      List.empty
    else situation.board.emptyPositions.filter(Forbidden.isForbidden(situation.board, _))

  private def afterDrop(before: Board, placed: Board, role: Role, at: Pos): Board = {
    val captured = placed.capture(role, capturedBy(placed, role, at))
    val marked   =
      if (isForbidden(before, role, at)) captured.updateHistory(_.copy(forbiddenDrop = Some(at)))
      else captured
    val stepped  = marked.withOpeningStep(opening.stepAfterDrop(marked))
    if (opening.swapsOnDrop(stepped.openingStep)) stepped.swapColours else stepped
  }

  def validDrops(situation: Situation): List[Drop] =
    if (situation.end || situation.openingStep == OpeningStep.Picking) List.empty
    else situation.board.emptyPositions.flatMap(drop(situation, situation.nextColour, _).toOption)

  def drop(situation: Situation, role: Role, pos: Pos): Validated[String, Drop] = {
    val board = situation.board
    if (situation.end) Validated.invalid("The game is over")
    else if (situation.openingStep == OpeningStep.Picking) Validated.invalid("White picks an offered stone")
    else if (role != situation.nextColour) Validated.invalid(s"The next stone is ${situation.nextColour}")
    else if (!board.empty(pos) || !board.boardSize.onBoard(pos))
      Validated.invalid(s"${pos} is occupied or off the board")
    else if (!opening.allowsDrop(board, pos))
      Validated.invalid(s"The opening does not allow a stone on ${pos}")
    else {
      val piece = Piece(board.seatOf(role), role)
      val after =
        if (opening.offeringStep(board)) {
          val offered = board.offer(pos)
          offered.withOpeningStep(opening.stepAfterDrop(offered))
        } else afterDrop(board, board.place(role, pos).get, role, pos)
      Validated.valid(Drop(piece, pos, situation, after))
    }
  }

  def canSwap(situation: Situation): Boolean =
    !situation.end && opening.canSwap(situation.openingStep, situation.board)

  def canSwap2(situation: Situation): Boolean =
    !situation.end && opening.canSwap2(situation.openingStep)

  def canOffer10(situation: Situation): Boolean =
    !situation.end && opening.canOffer10(situation.board)

  def canSelectSquares(situation: Situation): Boolean =
    !situation.end && situation.openingStep == OpeningStep.Picking

  def swap(situation: Situation): Validated[String, Swap] =
    if (!canSwap(situation)) Validated.invalid("A swap is only offered at a choice in the opening")
    else
      Validated.valid(
        Swap(situation, situation.board.swapColours.withOpeningStep(opening.stepAfterSwap))
      )

  def swap2(situation: Situation): Validated[String, Swap2] =
    if (!canSwap2(situation)) Validated.invalid("Swap2 is only offered in reply to the opening")
    else Validated.valid(Swap2(situation, situation.board.withOpeningStep(OpeningStep.Swap2Drops)))

  def offer10(situation: Situation): Validated[String, Offer10] =
    if (!canOffer10(situation)) Validated.invalid("Ten fifth stones are only offered after the fourth stone")
    else Validated.valid(Offer10(situation, situation.board.withOpeningStep(OpeningStep.Offering)))

  // white keeps one offered stone as black's fifth, and plays the sixth anywhere
  def selectSquares(situation: Situation, squares: List[Pos]): Validated[String, SelectSquares] =
    if (!canSelectSquares(situation)) Validated.invalid("There are no offered stones to pick from")
    else
      squares match {
        case List(pos) =>
          situation.board
            .keepOffered(pos)
            .map(kept => SelectSquares(squares, situation, kept.withOpeningStep(OpeningStep.Placing)))
            .toValid(s"${pos} is not an offered stone")
        case _         => Validated.invalid("Pick exactly one offered stone")
      }

  def selectSquaresActions(situation: Situation): List[SelectSquares] =
    if (!canSelectSquares(situation)) List.empty
    else situation.board.offered.flatMap(pos => selectSquares(situation, List(pos)).toOption)

  def hasMoveEffects = false

  def specialEnd(situation: Situation): Boolean =
    winner(situation).isDefined || situation.board.drawn

  def specialDraw(situation: Situation): Boolean =
    winner(situation).isEmpty && situation.board.drawn

  def winner(situation: Situation): Option[Player] = {
    val board = situation.board
    colourWithLine(board).map(board.seatOf) orElse
      board.history.forbiddenDrop.map(_ => board.seatOf(WhiteStone)) orElse
      seatWithCaptures(board)
  }

  def endStatus(situation: Situation): Option[Status] = {
    val board = situation.board
    if (colourWithLine(board).isDefined) Some(Status.LineOfFive)
    else if (board.history.forbiddenDrop.isDefined) Some(Status.ForbiddenMove)
    else if (seatWithCaptures(board).isDefined) Some(Status.TenCaptures)
    else if (board.drawn) Some(Status.Draw)
    else None
  }

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

  lazy val all: List[Variant] = List(Gomoku, Pente, Renju)

  val byId  = all.map(v => (v.id, v)).toMap
  val byKey = all.map(v => (v.key, v)).toMap

  val default = Gomoku

  def apply(id: Int): Option[Variant]     = byId get id
  def apply(key: String): Option[Variant] = byKey get key

  def bySize(boardSize: Board.BoardSize): Variant = all.find(_.boardSize == boardSize) | default

  // a fen without its variant: the board size narrows it, and an opening step only one variant uses settles it
  def forFen(fen: FEN): Option[Variant] =
    fen.boardSize.map { size =>
      all.find(v => v.boardSize == size && v.opening.steps.contains(fen.openingStep)) | bySize(size)
    }
  def orDefault(id: Int): Variant       = apply(id) | default
  def orDefault(key: String): Variant   = apply(key) | default

  def byName(name: String): Option[Variant] =
    all find (_.name.toLowerCase == name.toLowerCase)

  def exists(id: Int): Boolean = byId contains id

  val openingSensibleVariants: Set[Variant] = Set()

  val divisionSensibleVariants: Set[Variant] = Set()

}
