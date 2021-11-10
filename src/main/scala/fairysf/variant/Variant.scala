package strategygames.fairysf.variant

import cats.data.Validated
import cats.syntax.option._
import scala.annotation.nowarn

import strategygames.fairysf._
import strategygames.fairysf.format.{ FEN, Forsyth, Uci }
import strategygames.{ Color, GameFamily }

case class FairySFName(val name: String)

// Correctness depends on singletons for each variant ID
abstract class Variant private[variant] (
    val id: Int,
    val key: String,
    val name: String,
    val shortName: String,
    val title: String,
    val standardInitialPosition: Boolean,
    val fairysfName: FairySFName,
    val boardSize: Board.BoardSize
) {

  def shogi   = this == Shogi
  def xiangqi = this == Xiangqi

  def exotic = true

  def baseVariant: Boolean = false
  def fenVariant: Boolean  = false
  def aiVariant: Boolean   = true

  def whiteIsBetterVariant: Boolean = false
  def blindModeVariant: Boolean     = true

  def materialImbalanceVariant: Boolean = false

  def dropsVariant: Boolean = false

  def perfId: Int
  def perfIcon: Char

  def initialFen: FEN = Api.initialFen(fairysfName.name)

  def pieces: Map[Pos, Piece] = Api.pieceMapFromFen(fairysfName.name, initialFen.value)

  def startColor: Color = White

  //looks like this is only to allow King to be a valid promotion piece
  //in just atomic, so can leave as true for now
  def isValidPromotion(promotion: Option[PromotableRole]): Boolean = true

  def validMoves(situation: Situation): Map[Pos, List[Move]] =
    Api.legalMoves(
      fairysfName.name,
      situation.board.variant.initialFen.value,
      situation.board.uciMoves.some
    //Do we need to always filter out the drops?
    ).filterNot(_.contains("@"))
    .map{
      case Uci.Move.moveR(orig, dest, check) => (Pos.fromKey(orig), Pos.fromKey(dest), check)
    }.map{
      case (Some(orig), Some(dest), check) => {
        val uciMoves = (situation.board.uciMoves :+ s"${orig.key}${dest.key}${check}")
        (orig, Move(
          piece = situation.board.pieces(orig),
          orig = orig,
          dest = dest,
          situationBefore = situation,
          after = situation.board.copy(
            pieces = Api.pieceMapFromFen(
              fairysfName.name,
              Api.fenFromMoves(
                fairysfName.name,
                situation.board.variant.initialFen.value,
                uciMoves.some
              ).value
            ),
            uciMoves = uciMoves
          ),
          capture = None,
          promotion = None,
          castle = None,
          enpassant = false
        ))
      }
      case (orig, dest, check) => sys.error(s"Invalid position from uci: ${orig}${dest}${check}")
    }.groupBy(_._1).map { case (k,v) => (k,v.toList.map(_._2))}

  //TODO: test, but think this is right as its based off chess without actor check
  //Consider drops might get passed in through here
  //Update: not checking promotion here yet!
  def move(
      situation: Situation,
      from: Pos,
      to: Pos,
      promotion: Option[PromotableRole]
  ): Validated[String, Move] = {
    // Find the move in the variant specific list of valid moves
    situation.moves get from flatMap (_.find(_.dest == to)) match {
      case Some(move) => Validated.valid(move)
      case None => Validated.invalid(s"Not a valid move: ${from}${to}")
    }
    //for {
    //  m1 <- findMove(from, to) toValid "Piece on " + from + " cannot move to " + to
    //  m2 <- m1 withPromotion promotion toValid "Piece on " + from + " cannot promote to " + promotion
    //  m3 <-
    //    if (isValidPromotion(promotion)) Validated.valid(m2)
    //    else Validated.invalid("Cannot promote to " + promotion + " in this game mode")
    //} yield m3
  }

  def drop(situation: Situation, role: Role, pos: Pos): Validated[String, Drop] =
    Validated.invalid(s"$this variant cannot drop $situation $role $pos")

  def staleMate(situation: Situation): Boolean = !situation.check && situation.moves.isEmpty

  def checkmate(situation: Situation) = situation.check && situation.moves.isEmpty

  // In most variants, the winner is the last player to have played and there is a possibility of either a traditional
  // checkmate or a variant end condition
  def winner(situation: Situation): Option[Color] =
    if (situation.checkMate || specialEnd(situation)) Option(!situation.color) else None

  @nowarn def specialEnd(situation: Situation) = false

  @nowarn def specialDraw(situation: Situation) = false

  /** Returns the material imbalance in pawns (overridden in Antichess)
    */
  def materialImbalance(board: Board): Int =
    board.pieces.values.foldLeft(0) { case (acc, Piece(color, role)) =>
      Role.valueOf(role).fold(acc) { value =>
        acc + value * color.fold(1, -1)
      }
    }

  // Some variants have an extra effect on the board on a move. For example, in Atomic, some
  // pieces surrounding a capture explode
  def hasMoveEffects = false

  /** Applies a variant specific effect to the move. This helps decide whether a king is endangered by a move, for
    * example
    */
  def addVariantEffect(move: Move): Move = move

  def fiftyMoves(history: History): Boolean = history.halfMoveClock >= 100

  //can the move legally be 'undone' in a future move?
  //can the board return to the state before this move
  //only used in Move, and probably not going to be used
  //def isIrreversible(move: Move): Boolean = false

  /** Once a move has been decided upon from the available legal moves, the board is finalized
    */
  @nowarn def finalizeBoard(board: Board, uci: format.Uci, captured: Option[Piece]): Board = board

  def valid(board: Board, strict: Boolean): Boolean =
    Api.validateFEN(fairysfName.name, Forsyth.exportBoard(board))

  val roles: List[Role] = Role.all

  val promotableRoles: List[PromotableRole] = Role.allPromotable

  lazy val rolesByPgn: Map[Char, Role] = roles
    .map { r =>
      (r.pgn, r)
    }
    .to(Map)

  lazy val rolesPromotableByPgn: Map[Char, PromotableRole] =
    promotableRoles
      .map { r =>
        (r.pgn, r)
      }
      .to(Map)

  def isUnmovedPawn(color: Color, pos: Pos) = pos.rank == color.fold(Rank.Second, Rank.Seventh)

  override def toString = s"Variant($name)"

  override def equals(that: Any): Boolean = this eq that.asInstanceOf[AnyRef]

  override def hashCode: Int = id

  def gameFamily: GameFamily = GameFamily.Chess()
}

object Variant {

  val all: List[Variant] = List(
    Shogi,
    Xiangqi
  )
  val byId = all map { v =>
    (v.id, v)
  } toMap
  val byKey = all map { v =>
    (v.key, v)
  } toMap

  val default = Shogi

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
