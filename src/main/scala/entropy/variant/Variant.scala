package strategygames.entropy.variant

import cats.data.Validated
import cats.syntax.option._
import scalalib.extensions.*

import strategygames.entropy._
import strategygames.entropy.format.FEN
import strategygames.{ GameFamily, Player }

import scala.annotation.{ nowarn, tailrec }
import scala.util.Random

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

  def dropsVariant: Boolean = true

  def canOfferDraw: Boolean = false

  def repetitionEnabled: Boolean = false

  def perfId: Int
  def perfIcon: Char

  // seven counters of each of the seven colours
  def countersPerColour: Int = 7

  def initialFen: FEN = FEN("7/7/7/7/7/7/7[] w 0 0 1 1")

  def pieces: PieceMap = Map.empty

  def startPlayer: Player = P1

  def rounds: Int = 2

  // what is left to draw, derived rather than stored
  def bag(board: Board): List[Role] = {
    val placed = board.pieces.values.map(_.role).toList
    val held   = Player.all.flatMap(p => board.counterInPocket(p).toList)
    Role.all.flatMap { role =>
      List.fill((countersPerColour - placed.count(_ == role) - held.count(_ == role)).max(0))(role)
    }
  }

  private val directions: Directions = List(_.up, _.down, _.left, _.right)

  // Order slides a counter like a rook, over empty squares only
  private def slide(board: Board, from: Pos, dir: Direction): List[Pos] = {
    @tailrec def go(cur: Pos, acc: List[Pos]): List[Pos] =
      dir(cur) match {
        case Some(next) if board.empty(next) => go(next, next :: acc)
        case _                               => acc
      }
    go(from, Nil)
  }

  def validMoves(situation: Situation): Map[Pos, List[Move]] =
    if (!situation.isOrder || situation.end) Map.empty
    else
      situation.board
        .piecesOf(situation.player)
        .map { case (pos, piece) =>
          pos -> directions
            .flatMap(slide(situation.board, pos, _))
            .flatMap { dest =>
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

  def validDrops(situation: Situation): List[Drop] =
    if (!situation.isChaos || situation.end) List.empty
    else
      for {
        role <- situation.board.counterInPocket(situation.player).toList
        pos  <- situation.board.emptyPositions
        d    <- drop(situation, role, pos).toOption.toList
      } yield d

  def drop(situation: Situation, role: Role, pos: Pos): Validated[String, Drop] =
    if (!situation.isChaos) Validated.invalid("Only Chaos may place a counter")
    else if (!situation.board.empty(pos)) Validated.invalid(s"${pos} is occupied")
    else
      situation.board.counterInPocket(situation.player) match {
        case None                       => Validated.invalid("Nothing has been drawn")
        case Some(held) if held != role =>
          Validated.invalid(s"The drawn counter is ${held}, not ${role}")
        case Some(_)                    =>
          (for {
            pocket  <- situation.board.pocketData
            emptied <- pocket.drop(Piece(situation.player, role))
            // ownership inverts as it lands, so that Order may slide it
            placed  <- situation.board
                         .copy(pocketData = Some(emptied))
                         .place(Piece(situation.board.orderPlayer, role), pos)
          } yield Drop(Piece(situation.player, role), pos, situation, placed))
            .toValid(s"Could not place ${role} on ${pos}")
      }

  def drawCounter(situation: Situation, role: Role): Validated[String, DrawCounter] =
    if (!situation.isChaos) Validated.invalid("Only Chaos draws from the bag")
    else if (!situation.mustDraw) Validated.invalid("A counter has already been drawn this turn")
    else if (!bag(situation.board).contains(role)) Validated.invalid(s"No ${role} left in the bag")
    else
      Validated.valid(
        DrawCounter(
          role,
          situation,
          situation.board.withPocketData(_.store(Piece(situation.player, role)))
        )
      )

  // the counter that comes out is blind; which square it lands on is Chaos's decision
  def randomDraw(situation: Situation): Validated[String, DrawCounter] =
    Random
      .shuffle(bag(situation.board))
      .headOption
      .toValid("The bag is empty")
      .andThen(drawCounter(situation, _))

  def pass(situation: Situation): Validated[String, Pass] =
    if (!situation.isOrder) Validated.invalid("Only Order may pass")
    else Validated.valid(Pass(situation, situation.board))

  def hasMoveEffects = false

  def addVariantEffect(move: Move): Move = move

  def specialEnd(situation: Situation): Boolean = situation.board.round > rounds

  def specialDraw(situation: Situation): Boolean =
    specialEnd(situation) && situation.history.score.p1 == situation.history.score.p2

  def winner(situation: Situation): Option[Player] =
    if (!specialEnd(situation) || specialDraw(situation)) None
    else Some(Player.fromP1(situation.history.score.p1 > situation.history.score.p2))

  def materialImbalance(@nowarn board: Board): Int = 0

  def valid(@nowarn board: Board, @nowarn strict: Boolean): Boolean = true

  val roles: List[Role] = Role.all

  lazy val rolesByPgn: Map[Char, Role] = roles
    .map { r =>
      (r.pgn, r)
    }
    .to(Map)

  def defaultRole: Role = Role.defaultRole

  def gameFamily: GameFamily

  def updatePositionHashes(board: Board, @nowarn action: Action, hash: PositionHash): PositionHash =
    Hash(Situation(board, board.orderPlayer)) ++ hash

  override def toString = s"Variant($name)"

  override def equals(that: Any): Boolean = this eq that.asInstanceOf[AnyRef]

  override def hashCode: Int = id

}

object Variant {

  lazy val all: List[Variant] = List(Entropy)

  val byId  = all map { v =>
    (v.id, v)
  } toMap
  val byKey = all map { v =>
    (v.key, v)
  } toMap

  val default = Entropy

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
