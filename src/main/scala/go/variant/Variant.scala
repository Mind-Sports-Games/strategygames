package strategygames.go.variant

import cats.data.Validated
import cats.syntax.option._
import scala.annotation.nowarn

import strategygames.go._
import strategygames.go.format.{ FEN, Forsyth, Uci }
import strategygames.{ GameFamily, Player }

case class GoName(val name: String)

// Correctness depends on singletons for each variant ID
abstract class Variant private[variant] (
    val id: Int,
    val key: String,
    val name: String,
    val standardInitialPosition: Boolean,
    val boardSize: Board.BoardSize
) {

  def go9x9   = this == Go9x9
  def go13x13 = this == Go13x13
  def go19x19 = this == Go19x19

  def exotic = true

  def baseVariant: Boolean      = false
  def fenVariant: Boolean       = false
  def hasAnalysisBoard: Boolean = true
  def hasFishnet: Boolean       = false

  def p1IsBetterVariant: Boolean = false
  def blindModeVariant: Boolean  = true

  def materialImbalanceVariant: Boolean = false

  def dropsVariant: Boolean     = true
  def onlyDropsVariant: Boolean = true
  def hasGameScore: Boolean     = true

  def repetitionEnabled: Boolean = false

  def perfId: Int
  def perfIcon: Char

  def initialFen: FEN =
    format.FEN("19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19[SSSSSSSSSSssssssssss] b - 0 0 6 1")

  def pieces: PieceMap = Api.pieceMapFromFen(key, initialFen.value)

  def startPlayer: Player = P1
  def plysPerTurn: Int    = 1

  val kingPiece: Option[Role] = None

  // looks like this is only to allow King to be a valid promotion piece
  // in just atomic, so can leave as true for now
  def isValidPromotion(promotion: Option[PromotableRole]): Boolean = false

  def validMoves(situation: Situation) = None // just remove this?

  def validDrops(situation: Situation): List[Drop] =
    situation.board.apiPosition.legalMoves
      .map { dest =>
        (
          dest,
          Api.moveToPos(dest, situation.board.variant)
        )
      }
      .map {
        case (destInt, Some(dest)) => {
          val uciMove       = s"${Role.defaultRole.forsyth}@${dest.key}"
          val previousMoves = situation.board.uciMoves
          val newPosition   = situation.board.apiPosition.makeMovesWithPrevious(List(destInt), previousMoves)
          Drop(
            piece = Piece(situation.player, Role.defaultRole),
            pos = dest,
            situationBefore = situation,
            after = situation.board.copy(
              pieces = newPosition.pieceMap,
              uciMoves = situation.board.uciMoves :+ uciMove,
              pocketData = newPosition.pocketData,
              position = newPosition.some
            )
          )
        }
        case (destInt, dest)       => sys.error(s"Invalid pos from int: ${destInt}, ${dest}")
      }
      .toList

  // def move(
  //     situation: Situation,
  //     from: Pos,
  //     to: Pos,
  //     promotion: Option[PromotableRole]
  // ): Validated[String, Move] = {
  //   // Find the move in the variant specific list of valid moves
  //   situation.moves get from flatMap (_.find(m => m.dest == to && m.promotion == promotion)) toValid
  //     s"Not a valid move: ${from}${to} with prom: ${promotion}. Allowed moves: ${situation.moves}"
  // }

  def drop(situation: Situation, role: Role, pos: Pos): Validated[String, Drop] =
    if (dropsVariant)
      validDrops(situation).filter(d => d.piece.role == role && d.pos == pos).headOption match {
        case Some(drop) => Validated.valid(drop)
        case None       => Validated.invalid(s"$situation cannot perform the drop: $role on $pos")
      }
    else Validated.invalid(s"$this variant cannot drop $situation $role $pos")

  def possibleDrops(situation: Situation): Option[List[Pos]] =
    if (dropsVariant)
      validDrops(situation).map(_.pos).some
    else None

  def possibleDropsByRole(situation: Situation): Option[Map[Role, List[Pos]]] =
    if (dropsVariant)
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
    board.pieces.values.foldLeft(0) { case (acc, Piece(player, role)) =>
      Role.valueOf(role).fold(acc) { value =>
        acc + value * player.fold(1, -1)
      }
    }

  // Some variants have an extra effect on the board on a move. For example, in Atomic, some
  // pieces surrounding a capture explode
  def hasMoveEffects = false

  def addVariantEffect(drop: Drop): Drop = drop // should we affect score/captures here?

  /** Once a move has been decided upon from the available legal moves, the board is finalized
    */
  @nowarn def finalizeBoard(board: Board, uci: format.Uci, captured: Option[Piece]): Board =
    board

  def valid(board: Board, strict: Boolean): Boolean =
    Api.validateFEN(Forsyth.exportBoard(board))

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
    Go9x9,
    Go13x13,
    Go19x19
  )
  val byId                    = all map { v =>
    (v.id, v)
  } toMap
  val byKey                   = all map { v =>
    (v.key, v)
  } toMap

  val default = Go9x9

  def apply(id: Int): Option[Variant]     = byId get id
  def apply(key: String): Option[Variant] = byKey get key
  def orDefault(id: Int): Variant         = apply(id) | default
  def orDefault(key: String): Variant     = apply(key) | default

  def byName(name: String): Option[Variant] =
    all find (_.name.toLowerCase == name.toLowerCase)

  def exists(id: Int): Boolean = byId contains id

  val openingSensibleVariants: Set[Variant] = Set(Go9x9, Go13x13, Go19x19)

  val divisionSensibleVariants: Set[Variant] = Set()

}
