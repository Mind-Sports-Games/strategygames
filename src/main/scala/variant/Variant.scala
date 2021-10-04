package strategygames.variant

import cats.data.Validated
import cats.syntax.option._
import scala.annotation.nowarn

import strategygames._
import strategygames.format.FEN

// Correctness depends on singletons for each variant ID
abstract class Variant(
    val id: Int,
    val key: String,
    val name: String,
    val shortName: String,
    val title: String,
    val standardInitialPosition: Boolean,
    val gameType: Option[Int] = None
    //not handling draughts.boardSize... (yet)
) {

  def toChess: chess.variant.Variant
  def toDraughts: draughts.variant.Variant

  def pieces: Map[Pos, Piece]

  // An abstraction leak, we probably won't need this long term
  // but in the short term it helps us with the port.
  def standard: Boolean
  def chess960: Boolean
  def fromPosition: Boolean
  def kingOfTheHill: Boolean
  def threeCheck: Boolean
  def antichess: Boolean
  def atomic: Boolean
  def horde: Boolean
  def racingKings: Boolean
  def crazyhouse: Boolean
  def linesOfAction: Boolean

  def draughtsStandard: Boolean
  def frisian: Boolean
  def frysk: Boolean
  def antidraughts: Boolean
  def breakthrough: Boolean
  def russian: Boolean
  def brazilian: Boolean
  def pool: Boolean
  def draughtsFromPosition: Boolean

  def standardVariant: Boolean
  def fromPositionVariant: Boolean
  def frisianVariant: Boolean
  def draughts64Variant: Boolean

  def exotic: Boolean

  //used in lila setup/src/main/Config.scala
  def baseVariant: Boolean
  def fenVariant: Boolean
  def aiVariant: Boolean
  //used in lila modules/game/src/main/Game.scala
  def whiteIsBetterVariant: Boolean
  def blindModeVariant: Boolean
  //used in lila modules/playban/src/main/RageSit.scala
  def materialImbalanceVariant: Boolean

  def perfId: Int
  def perfIcon: Char

  def initialFen: FEN
  def startColor: Color

  def isValidPromotion(promotion: Option[PromotableRole]): Boolean

  def checkmate(situation: Situation): Boolean

  // In most variants, the winner is the last player to have played and there is a possibility of either a traditional
  // checkmate or a variant end condition
  def winner(situation: Situation): Option[Color]

  @nowarn def specialEnd(situation: Situation): Boolean

  @nowarn def specialDraw(situation: Situation): Boolean

  // Some variants have an extra effect on the board on a move. For example, in Atomic, some
  // pieces surrounding a capture explode
  def hasMoveEffects: Boolean

  /** Applies a variant specific effect to the move. This helps decide whether a king is endangered by a move, for
    * example
    */
  def addVariantEffect(move: Move): Move

  def valid(board: Board, strict: Boolean): Boolean

  val roles: List[Role]

  override def toString = s"Variant($name)"

  override def equals(that: Any): Boolean = this eq that.asInstanceOf[AnyRef]

  override def hashCode: Int = id

  // TODO: this function lies, it can't always turn itself into a chess variant,
  //       so sometimes it doesn't, and it calls sys.error instead. Yes. I know.
  def chessVariant: chess.variant.Variant

  def gameLogic: GameLogic
  def gameFamily: GameFamily

}

object Variant {

  case class Chess(v: chess.variant.Variant)
      extends Variant(
        id = v.id,
        key = v.key,
        name = v.name,
        shortName = v.shortName,
        title = v.title,
        standardInitialPosition = v.standardInitialPosition
      ) {

    def toChess: chess.variant.Variant = v
    def toDraughts = sys.error("Can't convert chess to draughts")

    def pieces: Map[Pos, Piece] =
      v.pieces.map { case (pos, piece) => (Pos.Chess(pos), Piece.Chess(piece)) }

    def standard: Boolean      = v.standard
    def chess960: Boolean      = v.chess960
    def fromPosition: Boolean  = v.fromPosition
    def kingOfTheHill: Boolean = v.kingOfTheHill
    def threeCheck: Boolean    = v.threeCheck
    def antichess: Boolean     = v.antichess
    def atomic: Boolean        = v.atomic
    def horde: Boolean         = v.horde
    def racingKings: Boolean   = v.racingKings
    def crazyhouse: Boolean    = v.crazyhouse
    def linesOfAction: Boolean = v.linesOfAction

    def draughtsStandard: Boolean     = false
    def frisian: Boolean              = false
    def frysk: Boolean                = false
    def antidraughts: Boolean         = false
    def breakthrough: Boolean         = false
    def russian: Boolean              = false
    def brazilian: Boolean            = false
    def pool: Boolean                 = false
    def draughtsFromPosition: Boolean = false

    def standardVariant: Boolean      = v.standard
    def fromPositionVariant: Boolean  = v.fromPosition
    def frisianVariant: Boolean       = false
    def draughts64Variant: Boolean    = false

    def exotic: Boolean = v.exotic

    def baseVariant: Boolean = v.baseVariant
    def fenVariant: Boolean  = v.fenVariant
    def aiVariant: Boolean   = v.aiVariant

    def whiteIsBetterVariant: Boolean = v.whiteIsBetterVariant
    def blindModeVariant: Boolean     = v.blindModeVariant

    def materialImbalanceVariant: Boolean = v.materialImbalanceVariant

    def perfId: Int    = v.perfId
    def perfIcon: Char = v.perfIcon

    def initialFen: FEN = format.Forsyth.initial(GameLogic.Chess())
    def startColor: Color = v.startColor

    def isValidPromotion(promotion: Option[PromotableRole]): Boolean = promotion match {
      case Some(Role.ChessPromotableRole(pr)) => v.isValidPromotion(pr.some)
      case None                               => v.isValidPromotion(None)
      case _                                  => sys.error("Not passed Chess objects")
    }

    def checkmate(situation: Situation): Boolean = situation match {
      case Situation.Chess(situation) => v.checkmate(situation)
      case _                          => sys.error("Not passed Chess objects")
    }

    def winner(situation: Situation): Option[Color] = situation match {
      case Situation.Chess(situation) => v.winner(situation)
      case _                          => sys.error("Not passed Chess objects")
    }

    @nowarn def specialEnd(situation: Situation): Boolean = situation match {
      case Situation.Chess(situation) => v.specialEnd(situation)
      case _                          => sys.error("Not passed Chess objects")
    }

    @nowarn def specialDraw(situation: Situation): Boolean = situation match {
      case Situation.Chess(situation) => v.specialDraw(situation)
      case _                          => sys.error("Not passed Chess objects")
    }

    def hasMoveEffects: Boolean = v.hasMoveEffects

    def addVariantEffect(move: Move): Move = move match {
      case Move.Chess(move) => Move.Chess(v.addVariantEffect(move))
      case _                => sys.error("Not passed Chess objects")
    }

    def valid(board: Board, strict: Boolean): Boolean = board match {
      case Board.Chess(board) => v.valid(board, strict)
      case _                  => sys.error("Not passed Chess objects")
    }

    val roles: List[Role] = v.roles.map(Role.ChessRole)

    override def equals(that: Any): Boolean = that match {
      case Chess(v2) => v2.equals(v)
      case _ => false
    }

    def chessVariant: chess.variant.Variant = v
    def gameLogic: GameLogic = GameLogic.Chess()
    def gameFamily: GameFamily = v.gameFamily

  }

  case class Draughts(v: draughts.variant.Variant)
      extends Variant(
        id = v.id,
        key = v.key,
        name = v.name,
        shortName = v.shortName,
        title = v.title,
        standardInitialPosition = v.standardInitialPosition,
        gameType = Option(v.gameType)
      ) {

    def toChess = sys.error("Can't convert draughts to chess")
    def toDraughts = v

    def pieces: Map[Pos, Piece] =
      v.pieces.map { case (pos, piece) => (Pos.Draughts(pos), Piece.Draughts(piece)) }

    def standard: Boolean      = false
    def chess960: Boolean      = false
    def fromPosition: Boolean  = false
    def kingOfTheHill: Boolean = false
    def threeCheck: Boolean    = false
    def antichess: Boolean     = false
    def atomic: Boolean        = false
    def horde: Boolean         = false
    def racingKings: Boolean   = false
    def crazyhouse: Boolean    = false
    def linesOfAction: Boolean = false

    def draughtsStandard: Boolean     = v.standard
    def frisian: Boolean              = v.frisian
    def frysk: Boolean                = v.frysk
    def antidraughts: Boolean         = v.antidraughts
    def breakthrough: Boolean         = v.breakthrough
    def russian: Boolean              = v.russian
    def brazilian: Boolean            = v.brazilian
    def pool: Boolean                 = v.pool
    def draughtsFromPosition: Boolean = v.fromPosition

    def standardVariant: Boolean      = v.standard
    def fromPositionVariant: Boolean  = v.fromPosition
    def frisianVariant: Boolean       = v.frisianVariant
    def draughts64Variant: Boolean    = v.draughts64Variant

    def exotic: Boolean = v.exotic

    def baseVariant: Boolean = v.baseVariant
    def fenVariant: Boolean  = v.fenVariant
    def aiVariant: Boolean   = v.aiVariant

    def whiteIsBetterVariant: Boolean = v.whiteIsBetterVariant
    def blindModeVariant: Boolean     = v.blindModeVariant

    def materialImbalanceVariant: Boolean = v.materialImbalanceVariant

    def perfId: Int    = v.perfId
    def perfIcon: Char = v.perfIcon

    def initialFen: FEN = format.Forsyth.initial(GameLogic.Draughts())
    def startColor: Color = v.startColor

    def isValidPromotion(promotion: Option[PromotableRole]): Boolean = promotion match {
      case Some(Role.DraughtsPromotableRole(pr)) => v.isValidPromotion(pr.some)
      case None                                  => v.isValidPromotion(None)
      case _                                     => sys.error("Not passed Draughts objects")
    }

    def checkmate(situation: Situation): Boolean = situation match {
      case Situation.Draughts(situation) => v.checkmate(situation)
      case _                             => sys.error("Not passed Draughts objects")
    }

    def winner(situation: Situation): Option[Color] = situation match {
      case Situation.Draughts(situation) => v.winner(situation)
      case _                             => sys.error("Not passed Draughts objects")
    }

    @nowarn def specialEnd(situation: Situation): Boolean = situation match {
      case Situation.Draughts(situation) => v.specialEnd(situation)
      case _                             => sys.error("Not passed Draughts objects")
    }

    @nowarn def specialDraw(situation: Situation): Boolean = situation match {
      case Situation.Draughts(situation) => v.specialDraw(situation)
      case _                             => sys.error("Not passed Draughts objects")
    }

    def hasMoveEffects: Boolean = v.hasMoveEffects

    def addVariantEffect(move: Move): Move = move match {
      case Move.Draughts(move) => Move.Draughts(v.addVariantEffect(move))
      case _                   => sys.error("Not passed Draughts objects")
    }
    def valid(board: Board, strict: Boolean): Boolean = board match {
      case Board.Draughts(board) => v.valid(board, strict)
      case _                     => sys.error("Not passed Draughts objects")
    }

    val roles: List[Role] = v.roles.map(Role.DraughtsRole)

    override def equals(that: Any): Boolean = that match {
      case Draughts(v2) => v2.equals(v)
      case _ => false
    }

    def chessVariant: chess.variant.Variant = sys.error("Unimplemented for Draughts")
    def gameLogic: GameLogic = GameLogic.Draughts()
    def gameFamily: GameFamily = v.gameFamily
  }

  case class FairySF(v: fairysf.variant.Variant)
      extends Variant(
        id = v.id,
        key = v.key,
        name = v.name,
        shortName = v.shortName,
        title = v.title,
        standardInitialPosition = v.standardInitialPosition,
        gameType = Option(v.gameType)
      ) {

    def toChess = sys.error("Can't convert fairysf to chess")
    def toDraughts = sys.error("Can't convert fairysf to draughts")

    def pieces: Map[Pos, Piece] = ???
      //v.pieces.map { case (pos, piece) => (Pos.Draughts(pos), Piece.Draughts(piece)) }

    def standard: Boolean      = false
    def chess960: Boolean      = false
    def fromPosition: Boolean  = false
    def kingOfTheHill: Boolean = false
    def threeCheck: Boolean    = false
    def antichess: Boolean     = false
    def atomic: Boolean        = false
    def horde: Boolean         = false
    def racingKings: Boolean   = false
    def crazyhouse: Boolean    = false
    def linesOfAction: Boolean = false

    def draughtsStandard: Boolean     = false
    def frisian: Boolean              = false
    def frysk: Boolean                = false
    def antidraughts: Boolean         = false
    def breakthrough: Boolean         = false
    def russian: Boolean              = false
    def brazilian: Boolean            = false
    def pool: Boolean                 = false
    def draughtsFromPosition: Boolean = false

    def standardVariant: Boolean      = standard || draughtsStandard
    def fromPositionVariant: Boolean  = fromPosition || draughtsFromPosition
    def frisianVariant: Boolean       = false
    def draughts64Variant: Boolean    = false

    def exotic: Boolean = v.exotic

    def baseVariant: Boolean = v.baseVariant
    def fenVariant: Boolean  = v.fenVariant
    def aiVariant: Boolean   = v.aiVariant

    def whiteIsBetterVariant: Boolean = v.whiteIsBetterVariant
    def blindModeVariant: Boolean     = v.blindModeVariant

    def materialImbalanceVariant: Boolean = v.materialImbalanceVariant

    def initialFen: FEN = format.Forsyth.initial(GameLogic.Draughts())
    def startColor: Color = v.startColor

    def isValidPromotion(promotion: Option[PromotableRole]): Boolean = ???
    //promotion match {
    //  case Some(Role.DraughtsPromotableRole(pr)) => v.isValidPromotion(pr.some)
    //  case None                                  => v.isValidPromotion(None)
    //  case _                                     => sys.error("Not passed Draughts objects")
    //}

    def checkmate(situation: Situation): Boolean = ???
    //situation match {
    //  case Situation.Draughts(situation) => v.checkmate(situation)
    //  case _                             => sys.error("Not passed Draughts objects")
    //}

    def winner(situation: Situation): Option[Color] = ???
    //situation match {
    //  case Situation.Draughts(situation) => v.winner(situation)
    //  case _                             => sys.error("Not passed Draughts objects")
    //}

    @nowarn def specialEnd(situation: Situation): Boolean = ???
    //situation match {
    //  case Situation.Draughts(situation) => v.specialEnd(situation)
    //  case _                             => sys.error("Not passed Draughts objects")
    //}

    @nowarn def specialDraw(situation: Situation): Boolean = ???
    //situation match {
    //  case Situation.Draughts(situation) => v.specialDraw(situation)
    //  case _                             => sys.error("Not passed Draughts objects")
    //}

    def hasMoveEffects: Boolean = v.hasMoveEffects

    def addVariantEffect(move: Move): Move = ???
    //move match {
    //  case Move.Draughts(move) => Move.Draughts(v.addVariantEffect(move))
    //  case _                   => sys.error("Not passed Draughts objects")
    //}
    def valid(board: Board, strict: Boolean): Boolean = ???
    //board match {
    //  case Board.Draughts(board) => v.valid(board, strict)
    //  case _                     => sys.error("Not passed Draughts objects")
    //}

    val roles: List[Role] = ???
      //v.roles.map(Role.DraughtsRole)

    override def equals(that: Any): Boolean = that match {
      case Draughts(v2) => v2.equals(v)
      case _ => false
    }

    def chessVariant: chess.variant.Variant = sys.error("Unimplemented for Draughts")
    def gameLogic: GameLogic = GameLogic.FairySF()
    def gameFamily: GameFamily = v.gameFamily
  }

  def all: List[Variant] =
    chess.variant.Variant.all.map(Chess) :::
    draughts.variant.Variant.all.map(Draughts) :::
    fairysf.variant.Variant.all.map(FairySF)

  def byId = all map { v => (v.id, v)} toMap

  def byKey = all map { v => (v.key, v)} toMap

  def all(lib: GameLogic): List[Variant] = lib match {
    case GameLogic.Draughts() => draughts.variant.Variant.all.map(Draughts)
    case GameLogic.Chess()    => chess.variant.Variant.all.map(Chess)
  }

  def byId(lib: GameLogic) = all(lib) map { v =>
    (v.id, v)
  } toMap

  def byKey(lib: GameLogic) = all(lib) map { v =>
    (v.key, v)
  } toMap

  def default(lib: GameLogic): Variant = lib match {
    case GameLogic.Draughts() => Draughts(draughts.variant.Variant.default)
    case GameLogic.Chess()    => Chess(chess.variant.Variant.default)
  }

  def apply(lib: GameLogic, id: Int): Option[Variant]     = byId(lib) get id
  def apply(lib: GameLogic, key: String): Option[Variant] = byKey(lib) get key
  def apply(key: String): Option[Variant]                 = byKey get key
  def orDefault(lib: GameLogic, id: Int): Variant         = apply(lib, id) | default(lib)
  def orDefault(lib: GameLogic, key: String): Variant     = apply(lib, key) | default(lib)
  def orDefault(key: String): Variant                     = apply(key) | default(GameLogic.Chess())

  def byName(lib: GameLogic, name: String): Option[Variant] =
    all(lib) find (_.name.toLowerCase == name.toLowerCase)

  def exists(lib: GameLogic, id: Int): Boolean = byId(lib) contains id

  def openingSensibleVariants(lib: GameLogic): Set[Variant] = lib match {
    case GameLogic.Draughts() => draughts.variant.Variant.openingSensibleVariants.map(Draughts)
    case GameLogic.Chess()    => chess.variant.Variant.openingSensibleVariants.map(Chess)
  }

  def divisionSensibleVariants(lib: GameLogic): Set[Variant] = lib match {
    case GameLogic.Draughts() => draughts.variant.Variant.divisionSensibleVariants.map(Draughts)
    case GameLogic.Chess()    => chess.variant.Variant.divisionSensibleVariants.map(Chess)
  }

  def libStandard(lib: GameLogic): Variant = lib match {
    case GameLogic.Draughts() => Variant.Draughts(draughts.variant.Standard)
    case GameLogic.Chess()    => Variant.Chess(chess.variant.Standard)
  }

  def libFromPosition(lib: GameLogic): Variant = lib match {
    case GameLogic.Draughts() => Variant.Draughts(draughts.variant.FromPosition)
    case GameLogic.Chess()    => Variant.Chess(chess.variant.FromPosition)
  }


  def wrap(v: chess.variant.Variant) = Chess(v)
  def wrap(v: draughts.variant.Variant) = Draughts(v)

}
