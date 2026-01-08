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
  def variableInitialFen: Boolean = false

  def hasAnalysisBoard: Boolean = true
  def hasFishnet: Boolean       = false

  def p1IsBetterVariant: Boolean = false
  def blindModeVariant: Boolean  = false

  def materialImbalanceVariant: Boolean = false

  def dropsVariant: Boolean = false

  def canOfferDraw: Boolean = true

  def repetitionEnabled: Boolean = true

  def perfId: Int
  def perfIcon: Char

  def initialFen: FEN = FEN(
    "W:Wa1,b1,b2,c1,c2,c3,d1,d2,d3,e1,e2,e3,f1,f2,f3,g1,g2,h1:Ba8,b7,b8,c6,c7,c8,d6,d7,d8,e6,e7,e8,f6,f7,f8,g7,g8,h8:H0:F1"
  )

  def pieces: PieceMap = initialFen.pieces

  def startPlayer: Player = P1

  def validMoves(situation: Situation): Map[Pos, List[Move]] = {
    var bestLineValue = 0
    var captureMap    = Map[Pos, List[Move]]()
    for (actor <- situation.actors) {
      val (capts, lineValue) = actor.capturesWithLineval
      if (capts.nonEmpty) {
        if (lineValue > bestLineValue) {
          bestLineValue = lineValue
          captureMap = Map(actor.pos -> capts)
        } else if (lineValue == bestLineValue)
          captureMap = captureMap + (actor.pos -> capts)
      }
    }

    if (captureMap.nonEmpty) captureMap
    else
      situation.actors
        .collect {
          case actor if actor.noncaptures.nonEmpty =>
            actor.pos -> actor.noncaptures
        }
        .to(Map)
  }

  def move(
      situation: Situation,
      from: Pos,
      to: Pos,
      promotion: Option[PromotableRole]
  ): Validated[String, Move] = {
    // Find the move in the variant specific list of valid moves
    situation.moves get from flatMap (_.find(m => m.dest == to)) toValid
      s"Not a valid move: ${from}${to}${promotion}. Allowed moves: ${situation.moves}"
  }

  def hasMoveEffects = false

  def addVariantEffect(move: Move): Move = move

  def variantEnd(situation: Situation) = situation.moves.isEmpty

  def specialEnd(@nowarn situation: Situation)  = false
  def specialDraw(@nowarn situation: Situation) = false

  def winner(situation: Situation): Option[Player] =
    Option.when(situation.variantEnd)(!situation.player)

  def materialImbalance(@nowarn board: Board): Int = 0

  def valid(@nowarn board: Board, @nowarn strict: Boolean): Boolean = true

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

  def updatePositionHashes(board: Board, move: Move, hash: PositionHash): PositionHash

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
