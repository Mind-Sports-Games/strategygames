package strategygames.backgammon

import strategygames.{ Player, Status }
import strategygames.backgammon.format.Uci

import cats.data.Validated
//import cats implicits in this file causes mindtrap to have problems
//import cats.implicits._

import scala.util.Random

case class Situation(board: Board, player: Player) {

  lazy val moves: Map[Pos, List[Move]] = board.variant.validMoves(this)

  private val movesList: List[Move] = moves.values.flatten.toList

  lazy val destinations: Map[Pos, List[Pos]] = moves.view.mapValues { _ map (_.dest) }.to(Map)

  def drops: Option[List[Pos]] = board.variant.possibleDrops(this)

  def dropsByRole: Option[Map[Role, List[Pos]]] = board.variant.possibleDropsByRole(this)

  def dropsAsDrops: List[Drop] = board.variant.validDrops(this)

  def lifts: List[Lift] = board.variant.validLifts(this)

  def diceRolls: List[DiceRoll] = board.variant.validDiceRolls(this)

  def undos: List[Undo] = undo.toList

  def endTurns: List[EndTurn] = endTurn.toList

  def cubeActions: List[CubeAction] = board.variant.validCubeActions(this)

  // don't include undos as it is not a progressive action
  def actions: List[Action] =
    movesList ::: dropsAsDrops ::: lifts ::: diceRolls ::: endTurns ::: cubeActions

  private def nextTurn(actions: List[Action]): List[List[Action]] =
    actions match {
      case Nil                                => Nil
      case h :: _ if h.toUci.uci == "endturn" => List(Nil)
      case h :: t                             =>
        nextTurn(t) ::: nextTurn(h.lazySituationAfter.actions).map(c => h :: c)
    }

  lazy val validTurns: List[List[Action]] = nextTurn(actions)

  def canMove: Boolean = moves.nonEmpty

  def canDrop: Boolean = dropsAsDrops.nonEmpty

  // In Backgammon when we can drop we have to drop - we can't do anything else
  def canOnlyDrop: Boolean = canDrop

  def canLift: Boolean = lifts.nonEmpty

  def canOnlyLift: Boolean = canLift && !canMove && !canDrop

  def canTouchPieces: Boolean = canMove || canDrop || canLift

  def canRollDice: Boolean = diceRolls.nonEmpty

  def canOnlyRollDice: Boolean =
    canRollDice && !canMove && !canDrop && !canLift && !canEndTurn && !canCubeAction

  def canCubeAction: Boolean = cubeActions.nonEmpty

  def canOnlyCubeAction: Boolean =
    canCubeAction && !canRollDice && !canMove && !canDrop && !canLift && !canEndTurn

  def canUndo: Boolean = undos.nonEmpty
  // def canUndo: Boolean = board.history.lastAction.map(_.undoable).getOrElse(false)

  // Users can't make the decision to endTurn at the start of the game
  // only SG can make that decision (using random diceRoll) so don't set
  // canEndTurn to true at the start of the game
  def canEndTurn: Boolean = board.variant.validEndTurn(this).nonEmpty &&
    !(board.history.lastTurn.isEmpty && board.history.currentTurn.isEmpty)

  def canOnlyEndTurn: Boolean = canEndTurn && !canMove && !canDrop && !canLift

  private def forcedEndTurn =
    canOnlyEndTurn && (board.history.forcedTurn || board.history.lastAction
      .map { case _: Uci.DiceRoll => true; case _ => false }
      .getOrElse(false))

  def canUseDice: Boolean = board.unusedDice.nonEmpty && (canMove || canDrop || canLift)

  def canCapture: Boolean = actions
    .map {
      case m: Move => m.capture.nonEmpty
      case d: Drop => d.capture.nonEmpty
      case _       => false
    }
    .contains(true)

  private def commonSetElements[A](sets: List[Set[A]]): Set[A] =
    sets.fold(sets.headOption.getOrElse(Set())) { (a, b) => a intersect b }

  // forcedPair should not be true if the order matters (i.e. 3/2 captures, but 2/3 doesn't)
  private lazy val forcedPair: Boolean =
    if (actions.length == 2 && board.unusedDice.toSet.size == 2)
      actions
        .flatMap { a =>
          a.lazySituationAfter.actions.map { a2 =>
            (a2.lazySituationAfter.board.pieces, a2.lazySituationAfter.board.pocketData)
          }
        }
        .toSet
        .size == 1
    else false

  // If orig is in all paths of validTurns then we know that a piece on that pos has to
  // play at some point during the turn. We need to check the dice is forced for that piece
  // either now or next turn. If so there will only be one forced action this turn.
  // The forced action is the action which outright gives the most paths (a tie for most
  // paths means that the dice isn't forced, although the piece has to move)
  // This assumes that some of the other forced action checks have been done first,
  // and that there is not a capture possible in any of the current available actions
  private lazy val forcedSingle: Option[Action] = // None
    commonSetElements(validTurns.map(_.flatMap {
      case m: Move => Some((m.orig, 0))
      case l: Lift => Some((l.pos, 1))
      case _       => None
    }.toSet)).headOption.map(_._1).flatMap { pos =>
      {
        val nextActionWithPathCount = validTurns
          .flatMap(_.headOption)
          .filter {
            case m: Move => m.orig == pos
            case l: Lift => l.pos == pos
            case _       => false
          }
          .groupBy(identity)
          .toList
          .map(a => (a._1, a._2.size))
          .sortBy(-_._2)
        if (
          nextActionWithPathCount.filter { awp =>
            awp._2 > 1 && Some(awp._2) == nextActionWithPathCount.headOption.map(_._2)
          }.size == 1
        )
          nextActionWithPathCount.headOption.map(_._1)
        else None
      }
    }

  private def uciWithDice(a: Action) = a match {
    case l: Lift => s"{${l.diceUsed}${l.toUci.uci}"
    case a       => a.toUci.uci
  }

  // no matter what path we pick, we have to choose this action at some point:
  private lazy val forcedInTurn: Option[Action] =
    commonSetElements(validTurns.map(_.map(uciWithDice).toSet)).headOption
      .flatMap(uci => actions.filter(a => uciWithDice(a) == uci).headOption)

  lazy val forcedAction: Option[Action] =
    if (board.history.justUsedUndo) None
    else if ((canTouchPieces && actions.length == 1) || forcedEndTurn || (canTouchPieces && forcedPair))
      actions.headOption
    else if (canTouchPieces && !canCapture && forcedSingle.nonEmpty) forcedSingle
    else if (canTouchPieces && forcedInTurn.nonEmpty) forcedInTurn
    else None

  def history = board.history

  def end: Boolean = board.variant.specialEnd(this)

  def winner: Option[Player] = board.variant.winner(this)

  def playable(strict: Boolean): Boolean =
    (board valid strict) && !end

  lazy val status: Option[Status] =
    if (board.variant.cubeRejected(this)) Some(Status.CubeDropped)
    else if (board.variant.backgammonWin(this)) Some(Status.BackgammonWin)
    else if (board.variant.gammonWin(this)) Some(Status.GammonWin)
    else if (end) Some(Status.SingleWin)
    else None

  def resignStatus(player: Player): Status.type => Status =
    if (board.racePosition)
      if (board.variant.backgammonPosition(this, player)) _.ResignBackgammon
      else if (board.variant.gammonPosition(this, player)) _.ResignGammon
      else _.Resign
    else if (board.history.score(player) == 0 && board.variant.key != "hyper") _.ResignBackgammon
    else _.Resign

  def outOfTimeStatus: Status.type => Status =
    if (board.racePosition)
      if (board.variant.backgammonPosition(this, player)) _.OutoftimeBackgammon
      else if (board.variant.gammonPosition(this, player)) _.OutoftimeGammon
      else _.Outoftime
    else if (board.history.score(player) == 0 && board.variant.key != "hyper") _.OutoftimeBackgammon
    else _.Outoftime

  // only works when we are not mid turn and have not rolled dice
  def maxTurnsFromEnd(player: Player): Option[Int] =
    if (board.racePosition)
      // min dice roll is 3. min pieces that can move is 2.
      Some(
        Math
          .ceil(board.pipCount(player).toDouble / 3)
          .toInt
          .max(
            Math.ceil(board.playerPiecesOnBoardCount(player).toDouble / 2).toInt
          )
      )
    else None

  // only works when we are not mid turn and have not rolled dice
  def minTurnsFromEnd(player: Player): Option[Int] =
    if (board.racePosition)
      // max dice roll is 24. max pieces that can move is 4.
      Some(
        Math
          .ceil(board.pipCount(player).toDouble / 24)
          .toInt
          .max(
            Math.ceil(board.playerPiecesOnBoardCount(player).toDouble / 4).toInt
          )
      )
    else None

  private def minTurnsFromScoring(player: Player): Option[Int] =
    if (board.racePosition)
      if (board.history.score(player) == 0)
        Some(
          Math
            .ceil(
              ((board.pieceCountOnBar(player) * 4) +
                (board.piecesInQuarter(player, 1) * 3) +
                (board.piecesInQuarter(player, 2) * 2) +
                (board.piecesInQuarter(player, 3)) +
                1).toDouble / 4
            )
            .toInt
        )
      else Some(0)
    else None

  private def minTurnsFromExitingOpponentHome(player: Player): Option[Int] =
    if (board.racePosition)
      if (board.history.score(player) == 0)
        Some(
          Math
            .ceil(((board.pieceCountOnBar(player) * 2) + board.piecesInQuarter(player, 1)).toDouble / 4)
            .toInt
        )
      else Some(0)
    else None

  // no unused dice so we can do a simple gin position calculation
  private def noUnusedDiceGinPosition: Boolean =
    if (board.unusedDice.isEmpty)
      maxTurnsFromEnd(player)
        .map { turns =>
          turns <= minTurnsFromEnd(!player).getOrElse(0) - (if (board.history.hasRolledDiceThisTurn) 1 else 0)
        }
        .getOrElse(false)
    else false

  private def noUnusedDiceGinGammonPosition: Boolean =
    if (board.unusedDice.isEmpty)
      maxTurnsFromEnd(player)
        .map { turns =>
          turns <= minTurnsFromScoring(!player).getOrElse(0) - (if (board.history.hasRolledDiceThisTurn) 1
                                                                else 0)
        }
        .getOrElse(false)
    else false

  private def noUnusedDiceGinBackgammonPosition: Boolean =
    if (board.unusedDice.isEmpty)
      maxTurnsFromEnd(player)
        .map { turns =>
          turns <= minTurnsFromExitingOpponentHome(!player)
            .getOrElse(0) - (if (board.history.hasRolledDiceThisTurn) 1 else 0)
        }
        .getOrElse(false)
    else false

  def opponentHasInsufficientMaterial: Boolean =
    if (board.unusedDice.isEmpty)
      noUnusedDiceGinPosition
    else
      !validTurns
        .map(t =>
          if (t.isEmpty) noUnusedDiceGinPosition
          else t.last.lazySituationAfter.end || t.last.lazySituationAfter.noUnusedDiceGinPosition
        )
        .contains(false)

  def opponentHasInsufficientMaterialForGammon: Boolean =
    if (board.unusedDice.isEmpty)
      noUnusedDiceGinGammonPosition
    else
      !validTurns
        .map(t =>
          if (t.isEmpty) noUnusedDiceGinGammonPosition
          else
            (t.last.lazySituationAfter.end && t.last.lazySituationAfter.board.variant.gammonWin(
              t.last.lazySituationAfter
            )) || t.last.lazySituationAfter.noUnusedDiceGinGammonPosition
        )
        .contains(false)

  def opponentHasInsufficientMaterialForBackgammon: Boolean =
    if (board.unusedDice.isEmpty)
      noUnusedDiceGinBackgammonPosition
    else
      !validTurns
        .map(t =>
          if (t.isEmpty) noUnusedDiceGinBackgammonPosition
          else
            (t.last.lazySituationAfter.end && t.last.lazySituationAfter.board.variant.backgammonWin(
              t.last.lazySituationAfter
            )) || t.last.lazySituationAfter.noUnusedDiceGinBackgammonPosition
        )
        .contains(false)

  def insufficientMaterialStatus: Status.type => Status =
    if (board.variant.key == "hyper") _.RuleOfGin // TODO fix correctly when using doubling cube
    else if (opponentHasInsufficientMaterialForBackgammon) _.GinBackgammon
    else if (opponentHasInsufficientMaterialForGammon) _.GinGammon
    else _.RuleOfGin

  def pointValue(player: Option[Player]): Option[Int] = board.variant.pointValue(this, player)

  def move(from: Pos, to: Pos): Validated[String, Move] =
    board.variant.move(this, from, to)

  def move(uci: Uci.Move): Validated[String, Move] =
    board.variant.move(this, uci.orig, uci.dest)

  def drop(role: Role, pos: Pos): Validated[String, Drop] =
    board.variant.drop(this, role, pos)

  def lift(pos: Pos): Validated[String, Lift] =
    board.variant.lift(this, pos)

  def diceRoll(dice: List[Int]): Validated[String, DiceRoll] =
    board.variant.diceRoll(this, dice)

  def undo: Validated[String, Undo] = board.variant.undo(this)

  def endTurn: Validated[String, EndTurn] = board.variant.endTurn(this)

  def cubeAction(interaction: CubeInteraction): Validated[String, CubeAction] =
    board.variant.cubeAction(this, interaction)

  def withHistory(history: History) =
    copy(
      board = board withHistory history
    )

  def withVariant(variant: strategygames.backgammon.variant.Variant) =
    copy(
      board = board withVariant variant
    )

  def unary_! = copy(player = !player)
}

object Situation {

  def apply(variant: strategygames.backgammon.variant.Variant): Situation =
    Situation(Board init variant, Random.shuffle(Player.all).head)

}
