package strategygames.backgammon.variant

import cats.data.Validated
import cats.syntax.option._
import scala.annotation.nowarn

import strategygames.backgammon._
import strategygames.backgammon.format.{ FEN, Forsyth, Uci }
import strategygames.{ GameFamily, Player, Score }

case class BackgammonName(val name: String)

// Correctness depends on singletons for each variant ID
abstract class Variant private[variant] (
    val id: Int,
    val key: String,
    val name: String,
    val standardInitialPosition: Boolean,
    val boardSize: Board.BoardSize
) {

  def exotic = true

  def baseVariant: Boolean      = false
  def fenVariant: Boolean       = false
  def hasAnalysisBoard: Boolean = true
  def hasFishnet: Boolean       = false

  def p1IsBetterVariant: Boolean = false
  def blindModeVariant: Boolean  = true

  def materialImbalanceVariant: Boolean = false

  def dropsVariant: Boolean     = true
  def onlyDropsVariant: Boolean = false
  def hasGameScore: Boolean     = true
  def canOfferDraw: Boolean     = false

  def repetitionEnabled: Boolean = false

  def perfId: Int
  def perfIcon: Char

  def initialFen: FEN = format.FEN("5S,3,3s,1,5s,4,2S/5s,3,3S,1,5S,4,2s[] - - w 0 0 1")

  def pieces: PieceMap = initialFen.pieces

  def startPlayer: Player = P1

  val kingPiece: Option[Role] = None

  // looks like this is only to allow King to be a valid promotion piece
  // in just atomic, so can leave as true for now
  def isValidPromotion(promotion: Option[PromotableRole]): Boolean = true

  private def destFromOrig(pos: Pos, count: Int): Pos =
    (if (count == 1) Pos((pos.index + 1) % 18)
     else Pos((pos.index + count - 1)    % 18)) match {
      case Some(dest) => dest
      case None       => sys.error(s"Invalid dest from orig(${pos.index}) in togyz move(${count})")
    }

  // returns the updated piecemap for pieces on the board and an optional captured piece to put in the pocket
  def piecesAfterAction(
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
      case Some(pos) if pieces.get(pos).isEmpty => p1 + (pos -> (Piece(player, Role.defaultRole), 1));
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
      .withHistory(
        situation.history.copy(
          score = Score(
            // need to actually update here
            situation.history.score.p1,
            situation.history.score.p2
          )
        )
      )
      .useDie(die)
  }

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
          // TODO review if we want to use capture and promotion fields for backgammon or not
          capture = None,
          promotion = None
        )
      }

  private def canMove(situation: Situation): Boolean =
    situation.board.unusedDice.nonEmpty && !situation.board.piecesOnBar(situation.player)

  def validMoves(situation: Situation): Map[Pos, List[Move]] =
    if (canMove(situation))
      generateMoves(situation)
        .map { m => (m.orig, m) }
        .groupBy(_._1)
        .map { case (k, v) => (k, v.toList.map(_._2)) }
    else Map.empty

  def validDrops(situation: Situation): List[Drop] =
    if (situation.board.unusedDice.nonEmpty && situation.board.piecesOnBar(situation.player))
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
            after = boardAfter(situation, None, Some(dest), die)
          )
        }
    else List.empty

  private def canLift(situation: Situation): Boolean =
    situation.board.unusedDice.nonEmpty && !situation.board.piecesOnBar(situation.player) && situation.board
      .piecesOf(situation.player)
      .keys
      .toList
      .diff(Pos.endQuarter(situation.player))
      .isEmpty

  def validLifts(situation: Situation): List[Lift] =
    if (canLift(situation))
      situation.board.unusedDice.distinct
        .flatMap { die =>
          Pos(
            (
              List(
                situation.board.furthestFromHome(situation.player),
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
    else List.empty

  private def diceCombinations(diceCount: Int, diceMax: Int = 6): Iterator[List[Int]] =
    List
      .fill(diceCount)((1 to diceMax).toList)
      .flatten
      .combinations(diceCount)
      .flatMap(_.permutations)

  def validDiceRolls(situation: Situation): List[DiceRoll] =
    if (situation.board.unusedDice.isEmpty)
      diceCombinations(2).toList.map { dice =>
        DiceRoll(
          dice,
          situation,
          situation.board.setDice(dice)
        )
      }
    else List.empty

  def validEndTurn(situation: Situation): Option[EndTurn] =
    if (
      (situation.board.unusedDice.isEmpty || !situation.canUseDice) &&
      situation.board.history.hasRolledDiceThisTurn
    )
      Some(
        EndTurn(
          situationBefore = situation,
          after = situation.board.setDice(List())
        )
      )
    else None

  def move(
      situation: Situation,
      from: Pos,
      to: Pos,
      promotion: Option[PromotableRole]
  ): Validated[String, Move] = {
    // Find the move in the variant specific list of valid moves
    situation.moves get from flatMap (_.find(m => m.dest == to && m.promotion == promotion)) toValid
      s"Not a valid move: ${from}${to} with prom: ${promotion}. Allowed moves: ${situation.moves}"
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

  def endTurn(situation: Situation): Validated[String, EndTurn] =
    validEndTurn(situation) match {
      case Some(et) => Validated.valid(et)
      case None     => Validated.invalid(s"$situation cannot do endTurn")
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

  def stalemateIsDraw = false

  def winner(situation: Situation): Option[Player]

  @nowarn def specialEnd(situation: Situation) = false

  @nowarn def specialDraw(situation: Situation) = false

  def materialImbalance(board: Board): Int =
    board.pieces.values.foldLeft(0) { case (acc, (Piece(player, _), count)) =>
      acc + count * player.fold(1, -1)
    }

  // Some variants have an extra effect on the board on a move. For example, in Atomic, some
  // pieces surrounding a capture explode
  def hasMoveEffects = false

  def addVariantEffect(move: Move): Move = move

  /** Once a move has been decided upon from the available legal moves, the board is finalized
    */
  @nowarn def finalizeBoard(board: Board, uci: format.Uci, captured: Option[Piece]): Board =
    board

  // TODO: implement
  def valid(board: Board, strict: Boolean): Boolean = true

  val roles: List[Role] = Role.all

  lazy val rolesByPgn: Map[Char, Role] = roles
    .map { r =>
      (r.pgn, r)
    }
    .to(Map)

  override def toString = s"Variant($name)"

  override def equals(that: Any): Boolean = this eq that.asInstanceOf[AnyRef]

  override def hashCode: Int = id

  def defaultRole: Role = Role.defaultRole

  def gameFamily: GameFamily
}

object Variant {

  lazy val all: List[Variant] = List(
    Backgammon
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
