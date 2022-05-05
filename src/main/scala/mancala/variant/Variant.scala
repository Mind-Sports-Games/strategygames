package strategygames.mancala.variant

import cats.data.Validated
import cats.syntax.option._
import scala.annotation.nowarn

import strategygames.mancala._
import strategygames.mancala.format.{ FEN, Forsyth, Uci }
import strategygames.{ GameFamily, Player }

case class MancalaName(val name: String)

// Correctness depends on singletons for each variant ID
abstract class Variant private[variant] (
    val id: Int,
    val key: String,
    val name: String,
    val standardInitialPosition: Boolean,
    val boardSize: Board.BoardSize
) {

  def oware       = this == Oware

  def exotic = true

  def baseVariant: Boolean = false
  def fenVariant: Boolean  = false
  def aiVariant: Boolean   = false

  def p1IsBetterVariant: Boolean = false
  def blindModeVariant: Boolean  = true

  def materialImbalanceVariant: Boolean = false

  def dropsVariant: Boolean     = false
  def onlyDropsVariant: Boolean = false
  def hasGameScore: Boolean = true

  def repetitionEnabled: Boolean = false

  def perfId: Int
  def perfIcon: Char

  def initialFen: FEN = format.FEN("DDDDDD/DDDDDD 0 0 S")

  def pieces: Map[Pos, Piece] = Api.pieceMapFromFen(key, initialFen.value) 

  def startPlayer: Player = P1

  val kingPiece: Option[Role] = None

  //looks like this is only to allow King to be a valid promotion piece
  //in just atomic, so can leave as true for now
  def isValidPromotion(promotion: Option[PromotableRole]): Boolean = true

  def validMoves(situation: Situation): Map[Pos, List[Move]] = {
    situation.board.apiPosition.legalMoves
      .map{ move => 
        val numSeeds = situation.board.apiPosition.fen.owareStoneArray(move)
        (
        move,
        Pos(move),
        Pos((numSeeds + move + (numSeeds - 1)/11) % 12) //dest -- seeds + move position + skipping own house
        )
      }
      .map{ 
        case (move, Some(orig), Some(dest)) => {
          val uciMove     = s"${orig.key}${dest.key}"
          val previousMoves = situation.board.uciMoves
          val newPosition = situation.board.apiPosition.makeMovesWithPrevious(List(move), previousMoves)
            (
              orig,
              Move(
                piece = situation.board.pieces(orig),
                orig = orig,
                dest = dest,
                situationBefore = situation,
                after = situation.board.copy(
                  pieces = newPosition.pieceMap,
                  uciMoves = (situation.board.uciMoves :+ uciMove),
                  position = newPosition.some
                ),
                capture = None,
                promotion = None
              )
            )
        }
        case (_, orig, dest) => sys.error(s"Invalid position from uci: ${orig}${dest}")
      }
      .groupBy(_._1)
      .map { case (k, v) => (k, v.toList.map(_._2)) }
  }

  def move(
      situation: Situation,
      from: Pos,
      to: Pos,
      promotion: Option[PromotableRole]
  ): Validated[String, Move] = {
    // Find the move in the variant specific list of valid moves
    situation.moves get from flatMap (_.find(m => m.dest == to && m.promotion == promotion)) match {
      case Some(move) => Validated.valid(move)
      case None =>
        Validated.invalid(
          s"Not a valid move: ${from}${to} with prom: ${promotion}. Allowed moves: ${situation.moves}"
        )
    }
  }

  def staleMate(situation: Situation): Boolean = situation.moves.isEmpty

  def checkmate(situation: Situation) = situation.moves.isEmpty

  def stalemateIsDraw = false

  // In most variants, the winner is the last player to have played
  // perpetual is the opposite. would need to recheck this for new variants
  def winner(situation: Situation): Option[Player] =
    if (situation.checkMate || situation.staleMate) Option(!situation.player)
    else None

  @nowarn def specialEnd(situation: Situation) = false

  @nowarn def specialDraw(situation: Situation) = false

  /** Returns the material imbalance in pawns (overridden in Antichess)
    */
  def materialImbalance(board: Board): Int =
    board.pieces.values.foldLeft(0) { case (acc, Piece(player, role)) =>
      Role.valueOf(role).fold(acc) { value =>
        acc + value * player.fold(1, -1)
      }
    }

  // Some variants have an extra effect on the board on a move. For example, in Atomic, some
  // pieces surrounding a capture explode
  def hasMoveEffects = false

  def addVariantEffect(move: Move): Move = move

  
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

  def gameFamily: GameFamily
}

object Variant {

  lazy val all: List[Variant] = List(
    Oware
  )
  val byId = all map { v =>
    (v.id, v)
  } toMap
  val byKey = all map { v =>
    (v.key, v)
  } toMap
  
  val default = Oware

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
