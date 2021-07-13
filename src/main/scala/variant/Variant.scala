package strategygames.variant

import cats.data.Validated
import cats.syntax.option._
import scala.annotation.nowarn

import strategygames._
import strategygames.format.FEN

// Correctness depends on singletons for each variant ID
abstract class Variant private[variant] (
    val id: Int,
    val key: String,
    val name: String,
    val shortName: String,
    val title: String,
    val standardInitialPosition: Boolean,
    val gameType: Option[Int] = None
    //not handling draughts.boardSize... (yet)
) {

  def pieces: Map[Pos, Piece]

  def exotic: Boolean

  //def initialFen: FEN   = format.Forsyth.initial

  def isValidPromotion(promotion: Option[PromotableRole]): Boolean

  // In most variants, the winner is the last player to have played and there is a possibility of either a traditional
  // checkmate or a variant end condition
  def winner(situation: Situation): Option[Color] =
    if (situation.checkMate || specialEnd(situation)) Option(!situation.color) else None

  @nowarn def specialEnd(situation: Situation) = false

  @nowarn def specialDraw(situation: Situation) = false

  // Some variants have an extra effect on the board on a move. For example, in Atomic, some
  // pieces surrounding a capture explode
  def hasMoveEffects = false

  /** Applies a variant specific effect to the move. This helps decide whether a king is endangered by a move, for
    * example
    */
  def addVariantEffect(move: Move): Move = move

  def valid(board: Board, strict: Boolean): Boolean

  val roles: List[Role]

  override def toString = s"Variant($name)"

  override def equals(that: Any): Boolean = this eq that.asInstanceOf[AnyRef]

  override def hashCode: Int = id

}

object Variant {

  class Chess(v: chess.variant.Variant) extends Variant(
    id = v.id,
    key = v.key,
    name = v.name,
    shortName = v.shortName,
    title = v.title,
    standardInitialPosition = v.standardInitialPosition
  ) {

    def exotic: Boolean = v.exotic

    def isValidPromotion(promotion: Option[PromotableRole]): Boolean = promotion match {
      case Some(ChessPromotableRole(pr)) => v.isValidPromotion(pr.some)
      case None                     => v.isValidPromotion(None)
      case _ => sys.error("Not passed Chess objects")
    }

    def valid(board: Board, strict: Boolean): Boolean = board match {
      case Board.Chess(board) => v.valid(board, strict)
      case _ => sys.error("Not passed Chess objects")
    }

    val roles: List[Role] = v.roles.map(ChessRole(_))

  }

  class Draughts(v: draughts.variant.Variant) extends Variant(
    id = v.id,
    key = v.key,
    name = v.name,
    shortName = v.shortName,
    title = v.title,
    standardInitialPosition = v.standardInitialPosition
    gameType = v.gameType
  ) {

    def exotic: Boolean = v.exotic

    def isValidPromotion(promotion: Option[PromotableRole]): Boolean = promotion match {
      case Some(DraughtsPromotableRole(pr)) => v.isValidPromotion(pr.some)
      case None                             => v.isValidPromotion(None)
      case _ => sys.error("Not passed Draughts objects")
    }

    def valid(board: Board, strict: Boolean): Boolean = board match {
      case Board.Draughts(board) => v.valid(board, strict)
      case _ => sys.error("Not passed Draughts objects")
    }

    val roles: List[Role] = v.roles.map(DraughtsRole)

  }

  def all(lib: GameLib): List[Variant] = lib match {
    case GameLib.Draughts() => v.all.map(Draughts)
    case GameLib.Chess()    => v.all.map(Chess)
  }

  val byId = all map { v =>
    (v.id, v)
  } toMap
  val byKey = all map { v =>
    (v.key, v)
  } toMap

  def default(lib: GameLib): Variant = lib match {
    case GameLib.Draughts() => Draughts(draughts.Variant.default)
    case GameLib.Chess()    => Chess(chess.Variant.default)
    case _ => sys.error("Mismatched gamelib types")
  }

  def apply(id: Int): Option[Variant]     = byId get id
  def apply(key: String): Option[Variant] = byKey get key
  def orDefault(id: Int): Variant         = apply(id) | default
  def orDefault(key: String): Variant     = apply(key) | default

  def byName(name: String): Option[Variant] =
    all find (_.name.toLowerCase == name.toLowerCase)

  def exists(id: Int): Boolean = byId contains id

  def openingSensibleVariants(lib: GameLib): Set[Variant] = lib match {
    case GameLib.Draughts() => v.openingSensibleVariants.map(Draughts)
    case GameLib.Chess()    => v.openingSensibleVariants.map(Chess)
  }

  def divisionSensibleVariants(lib: GameLib): Set[Variant] = lib match {
    case GameLib.Draughts() => v.divisionSensibleVariants.map(Draughts)
    case GameLib.Chess()    => v.divisionSensibleVariants.map(Chess)
  }

}
