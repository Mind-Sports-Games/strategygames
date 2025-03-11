package strategygames.dameo.variant

import cats.data.Validated
import cats.syntax.option._

import strategygames.dameo._
import strategygames.dameo.format.FEN
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

  def baseVariant: Boolean        = false
  def fenVariant: Boolean         = false
  def variableInitialFen: Boolean = true

  def hasAnalysisBoard: Boolean = false
  def hasFishnet: Boolean       = false

  def p1IsBetterVariant: Boolean = false
  def blindModeVariant: Boolean  = false

  def materialImbalanceVariant: Boolean = false

  def dropsVariant: Boolean = false

  def canOfferDraw: Boolean = true

  def repetitionEnabled: Boolean = true

  def perfId: Int
  def perfIcon: Char

  // TODO Dameo set this
  def initialFen: FEN = FEN("")

  def pieces: PieceMap = initialFen.pieces

  def startPlayer: Player = P1

  // TODO Dameo implement this, possibly using Actor move generation
  def validMoves(@nowarn situation: Situation): Map[Pos, List[Move]] = Map.empty

  def move(
      situation: Situation,
      from: Pos,
      to: Pos,
      promotion: Option[PromotableRole],
      capture: Option[Pos]
  ): Validated[String, Move] = {
    // Find the move in the variant specific list of valid moves
    situation.moves get from flatMap (_.find(m => m.dest == to)) toValid
      s"Not a valid move: ${from}${to}${promotion}${capture}. Allowed moves: ${situation.moves}"
  }

  def hasMoveEffects = false

  def addVariantEffect(move: Move): Move = move

  // TODO Dameo set this if relevant
  def maxDrawingMoves(@nowarn board: Board): Option[Int] = None

  // TODO Dameo set this
  def variantEnd(@nowarn situation: Situation) = false

  def specialEnd(@nowarn situation: Situation)  = false
  def specialDraw(@nowarn situation: Situation) = false

  // TODO Dameo set this
  def winner(@nowarn situation: Situation): Option[Player] = None

  def materialImbalance(@nowarn board: Board): Int = 0

  // TODO Dameo if there are sensible things to check put them here
  def valid(@nowarn board: Board, @nowarn strict: Boolean): Boolean = false

  val roles: List[Role] = Role.all

  lazy val rolesByPdn: Map[Char, Role] = roles
    .map { r =>
      (r.pdn, r)
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
    Dameo
  )
  val byId                    = all map { v =>
    (v.id, v)
  } toMap
  val byKey                   = all map { v =>
    (v.key, v)
  } toMap

  val default = Dameo

  def apply(id: Int): Option[Variant]     = byId get id
  def apply(key: String): Option[Variant] = byKey get key
  def orDefault(id: Int): Variant         = apply(id) | default
  def orDefault(key: String): Variant     = apply(key) | default

  def byName(name: String): Option[Variant] =
    all find (_.name.toLowerCase == name.toLowerCase)

  def exists(id: Int): Boolean = byId contains id

  val openingSensibleVariants: Set[Variant] = Set(strategygames.dameo.variant.Dameo)

  val divisionSensibleVariants: Set[Variant] = Set()

}
