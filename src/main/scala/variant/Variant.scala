package strategygames.variant

import cats.data.Validated
import cats.syntax.option._
import scala.annotation.nowarn

import strategygames._
import strategygames.format.FEN

// Correctness depends on singletons for each variant ID
abstract class Variant (
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

  def checkmate(situation: Situation): Boolean

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

  case class Chess(v: chess.variant.Variant) extends Variant(
    id = v.id,
    key = v.key,
    name = v.name,
    shortName = v.shortName,
    title = v.title,
    standardInitialPosition = v.standardInitialPosition
  ) {

    def pieces: Map[Pos, Piece] =
      v.pieces.map{case (pos, piece) => (Pos.Chess(pos), Piece.Chess(piece))}

    def exotic: Boolean = v.exotic

    def isValidPromotion(promotion: Option[PromotableRole]): Boolean = promotion match {
      case Some(Role.ChessPromotableRole(pr)) => v.isValidPromotion(pr.some)
      case None                               => v.isValidPromotion(None)
      case _ => sys.error("Not passed Chess objects")
    }

    def checkmate(situation: Situation): Boolean = situation match {
      case Situation.Chess(situation) => v.checkmate(situation)
      case _ => sys.error("Not passed Chess objects")
    }

    def valid(board: Board, strict: Boolean): Boolean = board match {
      case Board.Chess(board) => v.valid(board, strict)
      case _ => sys.error("Not passed Chess objects")
    }

    val roles: List[Role] = v.roles.map(Role.ChessRole)

  }

  case class Draughts(v: draughts.variant.Variant) extends Variant(
    id = v.id,
    key = v.key,
    name = v.name,
    shortName = v.shortName,
    title = v.title,
    standardInitialPosition = v.standardInitialPosition,
    gameType = Option(v.gameType)
  ) {

    def pieces: Map[Pos, Piece] =
      v.pieces.map{case (pos, piece) => (Pos.Draughts(pos), Piece.Draughts(piece))}

    def exotic: Boolean = v.exotic

    def isValidPromotion(promotion: Option[PromotableRole]): Boolean = promotion match {
      case Some(Role.DraughtsPromotableRole(pr)) => v.isValidPromotion(pr.some)
      case None                                  => v.isValidPromotion(None)
      case _ => sys.error("Not passed Draughts objects")
    }

    def checkmate(situation: Situation): Boolean = situation match {
      case Situation.Draughts(situation) => v.checkmate(situation)
      case _ => sys.error("Not passed Draughts objects")
    }

    def valid(board: Board, strict: Boolean): Boolean = board match {
      case Board.Draughts(board) => v.valid(board, strict)
      case _ => sys.error("Not passed Draughts objects")
    }

    val roles: List[Role] = v.roles.map(Role.DraughtsRole)

  }

  def all(lib: GameLib): List[Variant] = lib match {
    case GameLib.Draughts() => draughts.variant.Variant.all.map(Draughts)
    case GameLib.Chess()    => chess.variant.Variant.all.map(Chess)
  }

  def byId(lib: GameLib) = all(lib) map { v =>
    (v.id, v)
  } toMap

  def byKey(lib: GameLib) = all(lib) map { v =>
    (v.key, v)
  } toMap

  def default(lib: GameLib): Variant = lib match {
    case GameLib.Draughts() => Draughts(draughts.variant.Variant.default)
    case GameLib.Chess()    => Chess(chess.variant.Variant.default)
    case _ => sys.error("Mismatched gamelib types")
  }

  def apply(lib: GameLib, id: Int): Option[Variant]     = byId(lib) get id
  def apply(lib: GameLib, key: String): Option[Variant] = byKey(lib) get key
  def orDefault(lib: GameLib, id: Int): Variant         = apply(lib, id) | default(lib)
  def orDefault(lib: GameLib, key: String): Variant     = apply(lib, key) | default(lib)

  def byName(lib: GameLib, name: String): Option[Variant] =
    all(lib) find (_.name.toLowerCase == name.toLowerCase)

  def exists(lib: GameLib, id: Int): Boolean = byId(lib) contains id

  def openingSensibleVariants(lib: GameLib): Set[Variant] = lib match {
    case GameLib.Draughts() => draughts.variant.Variant.openingSensibleVariants.map(Draughts)
    case GameLib.Chess()    => chess.variant.Variant.openingSensibleVariants.map(Chess)
  }

  def divisionSensibleVariants(lib: GameLib): Set[Variant] = lib match {
    case GameLib.Draughts() => draughts.variant.Variant.divisionSensibleVariants.map(Draughts)
    case GameLib.Chess()    => chess.variant.Variant.divisionSensibleVariants.map(Chess)
  }

}
