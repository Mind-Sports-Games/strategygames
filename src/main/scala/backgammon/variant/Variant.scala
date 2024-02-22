package strategygames.backgammon.variant

import cats.data.Validated
import cats.syntax.option._

import strategygames.backgammon._
import strategygames.backgammon.format.FEN
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

  def canOfferDraw: Boolean       = false
  def ignoreSubmitAction: Boolean = true

  def repetitionEnabled: Boolean = false

  def perfId: Int
  def perfIcon: Char

  def initialFen: FEN = format.FEN("5S,3,3s,1,5s,4,2S/5s,3,3S,1,5S,4,2s[] - - w 0 0 1")

  def pieces: PieceMap = initialFen.pieces

  def startPlayer: Player = P1

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
          after = boardAfter(situation, Some(orig), Some(dest), die)
          // this isn't really used for Backgammon and isnt defined for drop
          // but should work if uncommented
          // capture = situation.board.piecesOf(!situation.player).get(dest).map(_ => dest)
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
    situation.board.unusedDice.nonEmpty && situation.board.piecesCanLift(situation.player)

  def validLifts(situation: Situation): List[Lift] =
    if (canLift(situation))
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
    else List.empty

  private def diceCombinations(diceCount: Int, diceMax: Int = 6): Iterator[List[Int]] =
    List
      .fill(diceCount)((1 to diceMax).toList)
      .flatten
      .combinations(diceCount)
      .flatMap(_.permutations)

  def validDiceRolls(situation: Situation): List[DiceRoll] =
    if (situation.board.unusedDice.isEmpty && !situation.board.history.hasRolledDiceThisTurn)
      diceCombinations(2).toList
        .filter { dr =>
          situation.board.history.didRollDiceLastTurn || dr.toSet.size == 2
        }
        .map { dice =>
          DiceRoll(
            dice,
            situation,
            situation.board.setDice(dice)
          )
        }
    else List.empty

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

  def specialEnd(situation: Situation) =
    (situation.board.history.score.p1 == 15) ||
      (situation.board.history.score.p2 == 15)

  def gammonWin(situation: Situation) =
    (situation.board.history.score.p1 == 15 && situation.board.history.score.p2 == 0) ||
      (situation.board.history.score.p2 == 15 && situation.board.history.score.p1 == 0)

  def backgammonWin(situation: Situation) =
    (
      situation.board.history.score.p1 == 15 &&
        situation.board.history.score.p2 == 0 && (
          situation.board.piecesOnBar(P2) || situation.board.pieceInOpponentsHome(P2)
        )
    ) || (
      situation.board.history.score.p2 == 15 &&
        situation.board.history.score.p1 == 0 && (
          situation.board.piecesOnBar(P1) || situation.board.pieceInOpponentsHome(P1)
        )
    )

  def winner(situation: Situation): Option[Player] =
    if (specialEnd(situation)) {
      if (situation.board.history.score.p1 > situation.board.history.score.p2)
        Player.fromName("p1")
      else Player.fromName("p2")
    } else None

  // need to count pieces in pockets so just look at score
  def materialImbalance(board: Board): Int = board.history.score.p2 - board.history.score.p1

  def valid(board: Board, @nowarn strict: Boolean): Boolean =
    board.playerPiecesOnBoardOrInPocket(P1) + board.history.score.p1 == 15 &&
      board.playerPiecesOnBoardOrInPocket(P2) + board.history.score.p2 == 15

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
    Backgammon,
    Nackgammon
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
