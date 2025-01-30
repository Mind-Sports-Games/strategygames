package strategygames.backgammon.variant

import cats.data.Validated
import cats.syntax.option._

import strategygames.backgammon._
import strategygames.backgammon.format.{ FEN, Uci }
import strategygames.{ GameFamily, Player, Score, Status }

import scala.util.Random
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
  def variableInitialFen: Boolean = true

  def hasAnalysisBoard: Boolean = false
  def hasFishnet: Boolean       = false

  def p1IsBetterVariant: Boolean = false
  def blindModeVariant: Boolean  = true

  def materialImbalanceVariant: Boolean = false

  def dropsVariant: Boolean     = true
  def onlyDropsVariant: Boolean = false
  def hasGameScore: Boolean     = true

  def canOfferDraw: Boolean       = false
  def ignoreSubmitAction: Boolean = true

  def repetitionEnabled: Boolean = false

  def perfId: Int
  def perfIcon: Char

  def initialFen: FEN = FEN("5S,3,3s,1,5s,4,2S/5s,3,3S,1,5S,4,2s[] - - w 0 0 - 1")

  def initialFens = List(
    FEN("5S,3,3s,1,5s,4,2S/5s,3,3S,1,5S,4,2s[] - - w 0 0 - 1"),
    FEN("5S,3,3s,1,5s,4,2S/5s,3,3S,1,5S,4,2s[] - - b 0 0 - 1")
  )

  def fenFromSetupConfig(multipoint: Boolean): FEN =
    if (multipoint) Random.shuffle(initialFens).head.initialiseCube
    else Random.shuffle(initialFens).head

  def pieces: PieceMap = initialFen.pieces

  def startPlayer: Player = P1

  def recalcStartPlayerForStats: Boolean = true

  def numStartingPiecesPerPlayer: Int = 15

  // returns the updated piecemap for pieces on the board and an optional captured piece to put in the pocket
  private def piecesAfterAction(
      pieces: PieceMap,
      player: Player,
      orig: Option[Pos],
      dest: Option[Pos]
  ): (PieceMap, Option[Piece]) = {
    var capture = false
    val p1      = pieces.map {
      // adding a piece to a stack we already own
      case (pos, posInfo) if Some(pos) == dest && posInfo._1.player == player                    =>
        (pos, (posInfo._1, posInfo._2 + 1))
      // adding a piece to a square where opponent has an unguarded piece
      case (pos, posInfo) if Some(pos) == dest && posInfo._1.player != player && posInfo._2 == 1 => {
        capture = true
        (pos, (Piece(player, Role.defaultRole), 1))
      }
      // removing a piece from a square where we still have more pieces on it
      case (pos, posInfo) if Some(pos) == orig && posInfo._2 > 1                                 =>
        (pos, (posInfo._1, posInfo._2 - 1))
      case (pos, posInfo)                                                                        => (pos, posInfo)
    }
    // adding a piece to a previously unoccupied square
    val p2      = dest match {
      case Some(pos) if pieces.get(pos).isEmpty => p1 + ((pos, (Piece(player, Role.defaultRole), 1)))
      case _                                    => p1
    }
    // removing a piece from a square that is now free
    val p3      = orig match {
      case Some(pos) =>
        pieces.get(pos) match {
          case Some((_, count)) if count == 1 => p2 - pos
          case _                              => p2
        }
      case _         => p2
    }
    (p3, if (capture) Some(Piece(!player, Role.defaultRole)) else None)
  }

  def boardAfter(situation: Situation, orig: Option[Pos], dest: Option[Pos], die: Int): Board = {
    val (pieces, capture)   = piecesAfterAction(situation.board.pieces, situation.player, orig, dest)
    val pocketsAfterDrop    = orig match {
      case None => situation.board.pocketData.flatMap(_.drop(Piece(situation.player, Role.defaultRole)))
      case _    => situation.board.pocketData
    }
    val pocketsAfterCapture = capture match {
      case Some(piece) =>
        pocketsAfterDrop.map(_.store(piece))
      case None        => pocketsAfterDrop
    }
    situation.board
      .copy(
        pieces = pieces,
        pocketData = pocketsAfterCapture
      )
      .useDie(die)
  }

  // for undos
  private def boardBefore(
      situation: Situation,
      orig: Option[Pos],
      dest: Option[Pos],
      die: Int,
      capture: Boolean
  ): Board = {
    val (pieces, _)          = piecesAfterAction(situation.board.pieces, situation.player, orig, dest)
    val pocketsBeforeDrop    = dest match {
      case None => situation.board.pocketData.map(_.store(Piece(situation.player, Role.defaultRole)))
      case _    => situation.board.pocketData
    }
    val pocketsBeforeCapture =
      if (capture)
        pocketsBeforeDrop.flatMap(_.drop(Piece(!situation.player, Role.defaultRole)))
      else pocketsBeforeDrop
    val piecesBeforeCapture  =
      orig match {
        case Some(pos) if capture && pieces.get(pos).isEmpty =>
          pieces + ((pos, (Piece(!situation.player, Role.defaultRole), 1)))
        case _                                               => pieces
      }
    situation.board
      .copy(
        pieces = piecesBeforeCapture,
        pocketData = pocketsBeforeCapture,
        history = situation.board.history.copy(
          score = orig match {
            case None =>
              Score(
                situation.board.history.score.p1 - situation.player.fold(1, 0),
                situation.board.history.score.p2 - situation.player.fold(0, 1)
              )
            case _    => situation.board.history.score
          }
        )
      )
      .undoUseDie(die)
  }

  private def actionsContinueTurnOrEnd(actionsWithLookAhead: Iterable[(Action, Boolean)]): Boolean =
    actionsWithLookAhead.map(_._2).toSet.contains(true)

  private def actionCanContinueTurnOrEnd(action: Action): Boolean =
    action.lazySituationAfter.canMove ||
      action.lazySituationAfter.canLift ||
      action.lazySituationAfter.canDrop ||
      action.lazySituationAfter.end

  private def generateMoves(situation: Situation) =
    situation.board
      .piecesOf(situation.player)
      .map { case (pos, _) =>
        situation.board.unusedDice.distinct
          .flatMap { die =>
            Pos(pos.index + Pos.indexDirection(situation.player) * die).map((die, pos, _))
          }
          .filterNot {
            case (_, _, dest) => {
              situation.board.pieces.get(dest) match {
                case Some((piece, count)) => piece.player != situation.player && count > 1
                case _                    => false
              }
            }
          }
      }
      .flatten
      .map { case (die, orig, dest) =>
        Move(
          piece = Piece(situation.player, Role.defaultRole),
          orig = orig,
          dest = dest,
          situationBefore = situation,
          after = boardAfter(situation, Some(orig), Some(dest), die),
          capture = situation.board.piecesOf(!situation.player).get(dest).map(_ => dest)
        )
      }

  private def checkMovesWithLookAhead(situation: Situation) = {
    val baseMoves = generateMoves(situation)
    situation.board.unusedDice.toSet.size match {
      case 2 => {
        val movesWithLookAhead = baseMoves.map { m => (m, actionCanContinueTurnOrEnd(m)) }
        if (actionsContinueTurnOrEnd(movesWithLookAhead) || liftsCanContinueTurnOrEnd(situation))
          movesWithLookAhead.filter(_._2).map(_._1)
        else baseMoves.filter(_.diceUsed == baseMoves.map(_.diceUsed).max)
      }
      case _ => baseMoves
    }
  }

  private def movesCanContinueTurnOrEnd(situation: Situation) =
    canMove(situation) && actionsContinueTurnOrEnd(generateMoves(situation).map { m =>
      (m, actionCanContinueTurnOrEnd(m))
    })

  private def canMove(situation: Situation): Boolean =
    situation.board.unusedDice.nonEmpty && !situation.board.piecesOnBar(situation.player)

  def validMoves(situation: Situation): Map[Pos, List[Move]] =
    if (canMove(situation))
      checkMovesWithLookAhead(situation)
        .map { m => (m.orig, m) }
        .groupBy(_._1)
        .map { case (k, v) => (k, v.toList.map(_._2)) }
    else Map.empty

  private def generateDrops(situation: Situation) =
    situation.board.unusedDice.distinct
      .flatMap { die =>
        Pos(
          Pos.barIndex(situation.player) + Pos.indexDirection(situation.player) * die
        ).map(pos => (die, pos))
      }
      .filterNot {
        case (_, pos) => {
          situation.board.pieces.get(pos) match {
            case Some((piece, count)) => piece.player != situation.player && count > 1
            case None                 => false
          }
        }
      }
      .map { case (die, dest) =>
        Drop(
          piece = Piece(situation.player, Role.defaultRole),
          pos = dest,
          situationBefore = situation,
          after = boardAfter(situation, None, Some(dest), die),
          capture = situation.board.piecesOf(!situation.player).get(dest).map(_ => dest)
        )
      }

  private def checkDropsWithLookAhead(situation: Situation) = {
    val baseDrops = generateDrops(situation)
    situation.board.unusedDice.toSet.size match {
      case 2 => {
        val dropsWithLookAhead = baseDrops.map { d => (d, actionCanContinueTurnOrEnd(d)) }
        if (actionsContinueTurnOrEnd(dropsWithLookAhead))
          dropsWithLookAhead.filter(_._2).map(_._1)
        else baseDrops.filter(_.diceUsed == baseDrops.map(_.diceUsed).max)
      }
      case _ => baseDrops
    }
  }

  private def canDrop(situation: Situation): Boolean =
    situation.board.unusedDice.nonEmpty && situation.board.piecesOnBar(situation.player)

  def validDrops(situation: Situation): List[Drop] =
    if (canDrop(situation))
      checkDropsWithLookAhead(situation)
    else List.empty

  private def generateLifts(situation: Situation) =
    situation.board.unusedDice.distinct
      .flatMap { die =>
        Pos(
          (
            List(
              situation.board.furthestFromEnd(situation.player),
              die
            ).min - (Pos.barIndex(!situation.player) * Pos.indexDirection(situation.player))
          ).abs
        ).map(pos => (die, pos))
      }
      .filter {
        case (_, pos) => {
          situation.board.pieces.get(pos) match {
            case Some((piece, count)) => piece.player == situation.player && count > 0
            case None                 => false
          }
        }
      }
      .map { case (die, orig) =>
        Lift(
          pos = orig,
          situationBefore = situation,
          after = boardAfter(situation, Some(orig), None, die)
        )
      }

  private def checkLiftsWithLookAhead(situation: Situation) = {
    val baseLifts = generateLifts(situation)
    situation.board.unusedDice.toSet.size match {
      case 2 => {
        val liftsWithLookAhead = baseLifts.map { l => (l, actionCanContinueTurnOrEnd(l)) }
        if (actionsContinueTurnOrEnd(liftsWithLookAhead) || movesCanContinueTurnOrEnd(situation))
          liftsWithLookAhead.filter(_._2).map(_._1)
        else baseLifts.filter(_.diceUsed == baseLifts.map(_.diceUsed).max)
      }
      case _ => baseLifts
    }
  }

  private def liftsCanContinueTurnOrEnd(situation: Situation) =
    canLift(situation) && actionsContinueTurnOrEnd(generateLifts(situation).map { l =>
      (l, actionCanContinueTurnOrEnd(l))
    })

  private def canLift(situation: Situation): Boolean =
    situation.board.unusedDice.nonEmpty && situation.board.piecesCanLift(situation.player)

  def validLifts(situation: Situation): List[Lift] =
    if (canLift(situation))
      checkLiftsWithLookAhead(situation)
        // all of this is to filter out a duplicated lift but with a different dice value
        .map { l => (l.pos, l) }
        .groupBy(_._1)
        .map {
          case (_, v) => {
            val lifts = v.toList.map(_._2)
            lifts.filter(_.diceUsed == lifts.map(_.diceUsed).max)
          }
        }
        .toList
        .flatten
    else List.empty

  private def diceCombinations(diceCount: Int, diceMax: Int = 6): Iterator[List[Int]] =
    List
      .fill(diceCount)((1 to diceMax).toList)
      .flatten
      .combinations(diceCount)
      .flatMap(_.permutations)

  def validDiceRolls(situation: Situation): List[DiceRoll] =
    if (
      situation.board.unusedDice.isEmpty &&
      !situation.board.history.hasRolledDiceThisTurn &&
      situation.board.cubeData.map(_.underOffer) != Some(true)
    )
      diceCombinations(2).toList
        .filter { dr =>
          situation.board.history.firstDiceRollHappened || dr.toSet.size == 2
        }
        .map { dice =>
          DiceRoll(
            dice,
            situation,
            situation.board.setDice(dice)
          )
        }
    else List.empty

  private def diceUsedInHistoricLift(situation: Situation): Option[Int] =
    situation.board.history.lastAction match {
      case Some(l: Uci.Lift) =>
        situation.board.history.currentTurn.headOption match {
          case Some(d: Uci.DiceRoll) =>
            // if we rolled a double, then there was only one number to use on the lift
            if (d.dice.toSet.size == 1) d.dice.headOption
            // if we have only done one action since the roll then just reset unusedDice to the roll
            else if (situation.board.history.currentTurn.size == 2)
              d.dice.diff(situation.board.unusedDice).headOption
            else
              // if we've done two actions since the roll lets look at the action we are not undoing
              situation.board.history.currentTurn.lift(1) match {
                // if the first was a move we know exactly what dice was used, so the lift used the other one
                case Some(m: Uci.Move) => d.dice.diff(List(m.diceUsed)).headOption
                // if both were lifts get the smallest dice that can do the lift that we are undoing
                case Some(_: Uci.Lift) => d.dice.filter(_ >= l.pos.liftDistance(situation.player)).minOption
                case _                 => None
              }
          case _                     => None
        }
      case _                 => None
    }

  def validUndo(situation: Situation): Option[Undo] =
    situation.board.history.lastAction
      .flatMap {
        case a: Uci.Move =>
          Some(boardBefore(situation, Some(a.dest), Some(a.orig), a.diceUsed, a.capture.isDefined))
        case a: Uci.Drop =>
          Some(
            boardBefore(
              situation,
              Some(a.pos),
              None,
              (Pos.barIndex(situation.player) - a.pos.index).abs,
              a.capture.isDefined
            )
          )
        case a: Uci.Lift =>
          // if we dont know the dice we cant reliably undo the lift
          diceUsedInHistoricLift(situation).map(
            boardBefore(
              situation,
              None,
              Some(a.pos),
              _,
              false
            )
          )
        case _           => None
      }
      .map { board =>
        Undo(
          situationBefore = situation,
          after = board
        )
      }

  def validEndTurn(situation: Situation): Option[EndTurn] =
    if (
      ((situation.board.unusedDice.isEmpty || !situation.canUseDice) &&
        situation.board.history.hasRolledDiceThisTurn) ||
      (situation.board.history.lastTurn.isEmpty && situation.board.history.currentTurn.isEmpty)
    )
      Some(
        EndTurn(
          situationBefore = situation,
          after = situation.board.setDice(List())
        )
      )
    else None

  def validCubeActions(situation: Situation): List[CubeAction] =
    if (situation.board.history.hasRolledDiceThisTurn || !situation.board.history.firstDiceRollHappened)
      List.empty
    else
      situation.board.cubeData match {
        case Some(cubeData) =>
          if (cubeData.canOffer(situation.player, situation.board.history.multiPointState))
            List(
              CubeAction(
                interaction = OfferDouble,
                situationBefore = situation,
                after = situation.board.copy(
                  cubeData = Some(cubeData.offer(situation.player))
                )
              )
            )
          else if (cubeData.underOffer && !cubeData.rejected)
            List(
              CubeAction(
                interaction = AcceptDouble,
                situationBefore = situation,
                after = situation.board.copy(
                  cubeData = Some(cubeData.double(situation.player))
                )
              ),
              CubeAction(
                interaction = RejectDouble,
                situationBefore = situation,
                after = situation.board.copy(
                  cubeData = Some(cubeData.reject(situation.player))
                )
              )
            )
          else List.empty
        case None           => List.empty
      }

  def move(
      situation: Situation,
      from: Pos,
      to: Pos
  ): Validated[String, Move] = {
    // Find the move in the variant specific list of valid moves
    situation.moves get from flatMap (_.find(m => m.dest == to)) toValid
      s"Not a valid move: ${from}${to}. Allowed moves: ${situation.moves}"
  }

  def drop(situation: Situation, role: Role, pos: Pos): Validated[String, Drop] =
    validDrops(situation).filter(d => d.piece.role == role && d.pos == pos).headOption match {
      case Some(drop) => Validated.valid(drop)
      case None       => Validated.invalid(s"$situation cannot perform the drop: $role on $pos")
    }

  def lift(
      situation: Situation,
      pos: Pos
  ): Validated[String, Lift] =
    validLifts(situation).filter(_.pos == pos).headOption match {
      case Some(lift) => Validated.valid(lift)
      case None       => Validated.invalid(s"$situation cannot perform the lift from: $pos")
    }

  def diceRoll(situation: Situation, dice: List[Int]): Validated[String, DiceRoll] =
    validDiceRolls(situation).filter(dr => dr.dice == dice).headOption match {
      case Some(dr) => Validated.valid(dr)
      case None     => Validated.invalid(s"$situation cannot do a dice roll of: $dice")
    }

  def undo(situation: Situation): Validated[String, Undo] =
    validUndo(situation) match {
      case Some(u) => Validated.valid(u)
      case None    => Validated.invalid(s"$situation cannot do undo")
    }

  def endTurn(situation: Situation): Validated[String, EndTurn] =
    validEndTurn(situation) match {
      case Some(et) => Validated.valid(et)
      case None     => Validated.invalid(s"$situation cannot do endTurn")
    }

  def cubeAction(
      situation: Situation,
      interaction: CubeInteraction
  ): Validated[String, CubeAction] =
    validCubeActions(situation).filter(ca => ca.interaction == interaction).headOption match {
      case Some(ca) => Validated.valid(ca)
      case None     => Validated.invalid(s"$situation cannot do a cube action of: $interaction")
    }

  def possibleDrops(situation: Situation): Option[List[Pos]] =
    if (dropsVariant && !situation.end)
      validDrops(situation).map(_.pos).some
    else None

  // do we need this. always the same role
  def possibleDropsByRole(situation: Situation): Option[Map[Role, List[Pos]]] =
    if (dropsVariant && !situation.end)
      validDrops(situation)
        .map(drop => (drop.piece.role, drop.pos))
        .groupBy(_._1)
        .map { case (k, v) => (k, v.toList.map(_._2)) }
        .some
    else None

  def cubeRejected(situation: Situation) =
    situation.board.cubeData.map(_.rejected) == Some(true)

  def specialEnd(situation: Situation) =
    (situation.board.history.score.p1 == numStartingPiecesPerPlayer) ||
      (situation.board.history.score.p2 == numStartingPiecesPerPlayer) ||
      (cubeRejected(situation))

  def gammonPosition(situation: Situation, player: Player) =
    situation.board.history.score(player) == 0 &&
      !situation.board.piecesOnBar(player) &&
      !situation.board.pieceInOpponentsHome(player)

  // doesnt check for not a backgammonWin
  def gammonWin(situation: Situation) =
    (situation.board.history.score.p1 == numStartingPiecesPerPlayer && situation.board.history.score.p2 == 0) ||
      (situation.board.history.score.p2 == numStartingPiecesPerPlayer && situation.board.history.score.p1 == 0)

  def backgammonPosition(situation: Situation, player: Player) =
    situation.board.history.score(player) == 0 &&
      (situation.board.piecesOnBar(player) ||
        situation.board.pieceInOpponentsHome(player))

  def backgammonWin(situation: Situation) =
    (
      situation.board.history.score.p1 == numStartingPiecesPerPlayer &&
        situation.board.history.score.p2 == 0 && (
          situation.board.piecesOnBar(P2) || situation.board.pieceInOpponentsHome(P2)
        )
    ) || (
      situation.board.history.score.p2 == numStartingPiecesPerPlayer &&
        situation.board.history.score.p1 == 0 && (
          situation.board.piecesOnBar(P1) || situation.board.pieceInOpponentsHome(P1)
        )
    )

  def winner(situation: Situation): Option[Player] =
    if (cubeRejected(situation)) situation.board.cubeData.fold(None: Option[Player])(_.owner)
    else if (specialEnd(situation)) {
      if (situation.board.history.score.p1 > situation.board.history.score.p2)
        Player.fromName("p1")
      else Player.fromName("p2")
    } else None

  private def gameValue(situation: Situation) =
    situation.board.cubeData.map(_.value).getOrElse(1)

  def pointValue(situation: Situation, player: Option[Player]): Option[Int] =
    (situation.status, player) match {
      case (Some(Status.CubeDropped), _)                    => Some(gameValue(situation))
      case _ if !situation.board.racePosition               => Some(gameValue(situation) * 3)
      case (_, Some(p)) if backgammonPosition(situation, p) => Some(gameValue(situation) * 3)
      case (_, Some(p)) if gammonPosition(situation, p)     => Some(gameValue(situation) * 2)
      case _                                                => Some(gameValue(situation))
    }

  // need to count pieces in pockets so just look at score
  def materialImbalance(board: Board): Int = board.history.score.p2 - board.history.score.p1

  def valid(board: Board, @nowarn strict: Boolean): Boolean =
    board.playerPiecesOnBoardOrInPocket(P1) + board.history.score.p1 == numStartingPiecesPerPlayer &&
      board.playerPiecesOnBoardOrInPocket(P2) + board.history.score.p2 == numStartingPiecesPerPlayer

  val roles: List[Role] = Role.all

  lazy val rolesByPgn: Map[Char, Role] = roles
    .map { r =>
      (r.pgn, r)
    }
    .to(Map)

  def useRuleOfGinOnInsufficientMaterial: Boolean = true

  override def toString = s"Variant($name)"

  override def equals(that: Any): Boolean = this eq that.asInstanceOf[AnyRef]

  override def hashCode: Int = id

  def defaultRole: Role = Role.defaultRole

  def gameFamily: GameFamily
}

object Variant {

  lazy val all: List[Variant] = List(
    Backgammon,
    Nackgammon,
    Hyper
  )
  val byId                    = all map { v =>
    (v.id, v)
  } toMap
  val byKey                   = all map { v =>
    (v.key, v)
  } toMap

  val default = Backgammon

  def apply(id: Int): Option[Variant]     = byId get id
  def apply(key: String): Option[Variant] = byKey get key
  def orDefault(id: Int): Variant         = apply(id) | default
  def orDefault(key: String): Variant     = apply(key) | default

  def byName(name: String): Option[Variant] =
    all find (_.name.toLowerCase == name.toLowerCase)

  def exists(id: Int): Boolean = byId contains id

  val openingSensibleVariants: Set[Variant] = Set(strategygames.backgammon.variant.Backgammon)

  val divisionSensibleVariants: Set[Variant] = Set()

}
