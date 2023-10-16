package strategygames.variant

import cats.syntax.option._
import scala.annotation.nowarn

import strategygames._
import strategygames.format.FEN

// Correctness depends on singletons for each variant ID
abstract class Variant(
    val id: Int,
    val key: String,
    val fishnetKey: String,
    val name: String,
    val standardInitialPosition: Boolean,
    val gameType: Option[Int] = None
    // not handling draughts.boardSize... (yet)
) {

  def toChess: chess.variant.Variant
  def toDraughts: draughts.variant.Variant
  def toFairySF: fairysf.variant.Variant
  def toSamurai: samurai.variant.Variant
  def toTogyzkumalak: strategygames.togyzkumalak.variant.Variant
  def toGo: go.variant.Variant

  def pieces: PieceMap

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
  def fiveCheck: Boolean
  def noCastling: Boolean
  def scrambledEggs: Boolean

  def draughtsStandard: Boolean
  def frisian: Boolean
  def frysk: Boolean
  def antidraughts: Boolean
  def breakthrough: Boolean
  def russian: Boolean
  def brazilian: Boolean
  def pool: Boolean
  def portuguese: Boolean
  def english: Boolean
  def draughtsFromPosition: Boolean

  def shogi: Boolean
  def xiangqi: Boolean
  def minishogi: Boolean
  def minixiangqi: Boolean
  def flipello: Boolean
  def flipello10: Boolean
  def amazons: Boolean

  def oware: Boolean

  def togyzkumalak: Boolean

  def go9x9: Boolean
  def go13x13: Boolean
  def go19x19: Boolean

  def standardVariant: Boolean
  def fromPositionVariant: Boolean
  def exoticChessVariant: Boolean
  def frisianVariant: Boolean
  def draughts64Variant: Boolean

  def exotic: Boolean

  // used in lila setup/src/main/Config.scala
  def baseVariant: Boolean
  def fenVariant: Boolean
  def hasAnalysisBoard: Boolean
  def hasFishnet: Boolean
  // used in lila modules/game/src/main/Game.scala
  def p1IsBetterVariant: Boolean
  def blindModeVariant: Boolean
  // used in lila modules/playban/src/main/RageSit.scala
  def materialImbalanceVariant: Boolean

  def dropsVariant: Boolean
  def onlyDropsVariant: Boolean
  def hasGameScore: Boolean
  def canOfferDraw: Boolean

  def perfId: Int
  def perfIcon: Char

  def initialFen: FEN
  def startPlayer: Player
  def plysPerTurn: Int

  def isValidPromotion(promotion: Option[PromotableRole]): Boolean

  def checkmate(situation: Situation): Boolean

  def stalemateIsDraw: Boolean

  // In most variants, the winner is the last player to have played and there is a possibility of either a traditional
  // checkmate or a variant end condition
  def winner(situation: Situation): Option[Player]

  @nowarn def specialEnd(situation: Situation): Boolean

  @nowarn def specialDraw(situation: Situation): Boolean

  // Some variants have an extra effect on the board on a move. For example, in Atomic, some
  // pieces surrounding a capture explode
  def hasMoveEffects: Boolean

  /** Applies a variant specific effect to the move. This helps decide whether a king is endangered by a move,
    * for example
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

  def playerNames: Map[Player, String]
  def playerColors: Map[Player, String]

}

object Variant {

  case class Chess(v: chess.variant.Variant)
      extends Variant(
        id = v.id,
        key = v.key,
        fishnetKey = v.fishnetKey,
        name = v.name,
        standardInitialPosition = v.standardInitialPosition
      ) {

    def toChess: chess.variant.Variant = v
    def toDraughts                     = sys.error("Can't convert chess to draughts")
    def toFairySF                      = sys.error("Can't convert chess to fairysf")
    def toSamurai                      = sys.error("Can't convert chess to samurai")
    def toTogyzkumalak                 = sys.error("Can't convert chess to togyzkumalak")
    def toGo                           = sys.error("Can't convert chess to go")

    def pieces: PieceMap =
      v.pieces.map { case (pos, piece) => (Pos.Chess(pos), (Piece.Chess(piece), 1)) }

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
    def fiveCheck: Boolean     = v.fiveCheck
    def noCastling: Boolean    = v.noCastling
    def scrambledEggs: Boolean = v.scrambledEggs

    def draughtsStandard: Boolean     = false
    def frisian: Boolean              = false
    def frysk: Boolean                = false
    def antidraughts: Boolean         = false
    def breakthrough: Boolean         = false
    def russian: Boolean              = false
    def brazilian: Boolean            = false
    def pool: Boolean                 = false
    def portuguese: Boolean           = false
    def english: Boolean              = false
    def draughtsFromPosition: Boolean = false

    def shogi: Boolean       = false
    def xiangqi: Boolean     = false
    def minishogi: Boolean   = false
    def minixiangqi: Boolean = false
    def flipello: Boolean    = false
    def flipello10: Boolean  = false
    def amazons: Boolean     = false

    def oware: Boolean = false

    def togyzkumalak: Boolean        = false
    def go9x9: Boolean               = false
    def go13x13: Boolean             = false
    def go19x19: Boolean             = false
    def standardVariant: Boolean     = v.standard
    def fromPositionVariant: Boolean = v.fromPosition
    def exoticChessVariant: Boolean  = v.exoticChessVariant
    def frisianVariant: Boolean      = false
    def draughts64Variant: Boolean   = false

    def exotic: Boolean = v.exotic

    def baseVariant: Boolean      = v.baseVariant
    def fenVariant: Boolean       = v.fenVariant
    def hasAnalysisBoard: Boolean = v.hasAnalysisBoard
    def hasFishnet: Boolean       = v.hasFishnet

    def p1IsBetterVariant: Boolean = v.p1IsBetterVariant
    def blindModeVariant: Boolean  = v.blindModeVariant

    def materialImbalanceVariant: Boolean = v.materialImbalanceVariant

    def dropsVariant: Boolean     = v.dropsVariant
    def onlyDropsVariant: Boolean = false
    def hasGameScore: Boolean     = false
    def canOfferDraw: Boolean     = v.canOfferDraw

    def perfId: Int    = v.perfId
    def perfIcon: Char = v.perfIcon

    def initialFen: FEN     = FEN.Chess(v.initialFen)
    def startPlayer: Player = v.startPlayer
    def plysPerTurn: Int    = v.plysPerTurn

    def isValidPromotion(promotion: Option[PromotableRole]): Boolean = promotion match {
      case Some(Role.ChessPromotableRole(pr)) => v.isValidPromotion(pr.some)
      case None                               => v.isValidPromotion(None)
      case _                                  => sys.error("Not passed Chess objects")
    }

    def checkmate(situation: Situation): Boolean = situation match {
      case Situation.Chess(situation) => v.checkmate(situation)
      case _                          => sys.error("Not passed Chess objects")
    }

    def stalemateIsDraw: Boolean = v.stalemateIsDraw

    def winner(situation: Situation): Option[Player] = situation match {
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
      case _         => false
    }

    def chessVariant: chess.variant.Variant = v
    def gameLogic: GameLogic                = GameLogic.Chess()
    def gameFamily: GameFamily              = v.gameFamily

    def playerNames: Map[Player, String]  = gameFamily.playerNames
    def playerColors: Map[Player, String] = gameFamily.playerColors
  }

  case class Draughts(v: draughts.variant.Variant)
      extends Variant(
        id = v.id,
        key = v.key,
        fishnetKey = v.key,
        name = v.name,
        standardInitialPosition = v.standardInitialPosition,
        gameType = Option(v.gameType)
      ) {

    def toChess        = sys.error("Can't convert draughts to chess")
    def toDraughts     = v
    def toFairySF      = sys.error("Can't convert draughts to fairysf")
    def toSamurai      = sys.error("Can't convert draughts to samurai")
    def toTogyzkumalak = sys.error("Can't convert draughts to togyzkumalak")
    def toGo           = sys.error("Can't convert draughts to go")

    def pieces: PieceMap =
      v.pieces.map { case (pos, piece) => (Pos.Draughts(pos), (Piece.Draughts(piece), 1)) }

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
    def fiveCheck: Boolean     = false
    def noCastling: Boolean    = false
    def scrambledEggs: Boolean = false

    def draughtsStandard: Boolean     = v.standard
    def frisian: Boolean              = v.frisian
    def frysk: Boolean                = v.frysk
    def antidraughts: Boolean         = v.antidraughts
    def breakthrough: Boolean         = v.breakthrough
    def russian: Boolean              = v.russian
    def brazilian: Boolean            = v.brazilian
    def pool: Boolean                 = v.pool
    def portuguese: Boolean           = v.portuguese
    def english: Boolean              = v.english
    def draughtsFromPosition: Boolean = v.fromPosition

    def shogi: Boolean       = false
    def xiangqi: Boolean     = false
    def minishogi: Boolean   = false
    def minixiangqi: Boolean = false
    def flipello: Boolean    = false
    def flipello10: Boolean  = false
    def amazons: Boolean     = false

    def oware: Boolean = false

    def togyzkumalak: Boolean = false
    def go9x9: Boolean        = false
    def go13x13: Boolean      = false
    def go19x19: Boolean      = false

    def standardVariant: Boolean     = v.standard
    def fromPositionVariant: Boolean = v.fromPosition
    def exoticChessVariant: Boolean  = false
    def frisianVariant: Boolean      = v.frisianVariant
    def draughts64Variant: Boolean   = v.draughts64Variant

    def exotic: Boolean = v.exotic

    def baseVariant: Boolean      = v.baseVariant
    def fenVariant: Boolean       = v.fenVariant
    def hasAnalysisBoard: Boolean = v.hasAnalysisBoard
    def hasFishnet: Boolean       = v.hasFishnet

    def p1IsBetterVariant: Boolean = v.p1IsBetterVariant
    def blindModeVariant: Boolean  = v.blindModeVariant

    def materialImbalanceVariant: Boolean = v.materialImbalanceVariant

    def dropsVariant: Boolean     = false
    def onlyDropsVariant: Boolean = false
    def hasGameScore: Boolean     = false
    def canOfferDraw: Boolean     = v.canOfferDraw

    def perfId: Int    = v.perfId
    def perfIcon: Char = v.perfIcon

    def initialFen: FEN     = FEN.Draughts(v.initialFen)
    def startPlayer: Player = v.startPlayer
    def plysPerTurn: Int    = v.plysPerTurn

    def isValidPromotion(promotion: Option[PromotableRole]): Boolean = promotion match {
      case Some(Role.DraughtsPromotableRole(pr)) => v.isValidPromotion(pr.some)
      case None                                  => v.isValidPromotion(None)
      case _                                     => sys.error("Not passed Draughts objects")
    }

    def checkmate(situation: Situation): Boolean = situation match {
      case Situation.Draughts(situation) => v.checkmate(situation)
      case _                             => sys.error("Not passed Draughts objects")
    }

    // stalemate not referenced in draughts
    def stalemateIsDraw: Boolean = true

    def winner(situation: Situation): Option[Player] = situation match {
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

    def addVariantEffect(move: Move): Move            = move match {
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
      case _            => false
    }

    def chessVariant: chess.variant.Variant = sys.error("Unimplemented for Draughts")
    def gameLogic: GameLogic                = GameLogic.Draughts()
    def gameFamily: GameFamily              = v.gameFamily

    def playerNames: Map[Player, String]  = v.playerNames
    def playerColors: Map[Player, String] = v.playerColors
  }

  case class FairySF(v: fairysf.variant.Variant)
      extends Variant(
        id = v.id,
        key = v.key,
        fishnetKey = v.fairysfName.name,
        name = v.name,
        standardInitialPosition = v.standardInitialPosition
      ) {

    def toChess          = sys.error("Can't convert fairysf to chess")
    def toDraughts       = sys.error("Can't convert fairysf to draughts")
    def toFairySF        = v
    def toSamurai        = sys.error("Can't convert fairysf to samurai")
    def toTogyzkumalak   = sys.error("Can't convert fairysf to togyzkumalak")
    def toGo             = sys.error("Can't convert fairysf to go")
    def pieces: PieceMap =
      v.pieces.map { case (pos, piece) => (Pos.FairySF(pos), (Piece.FairySF(piece), 1)) }

    def standard: Boolean      = false
    def chess960: Boolean      = false
    def fromPosition: Boolean  = false
    def kingOfTheHill: Boolean = false
    def threeCheck: Boolean    = false
    def fiveCheck: Boolean     = false
    def antichess: Boolean     = false
    def atomic: Boolean        = false
    def horde: Boolean         = false
    def racingKings: Boolean   = false
    def crazyhouse: Boolean    = false
    def linesOfAction: Boolean = false
    def noCastling: Boolean    = false
    def scrambledEggs: Boolean = false

    def draughtsStandard: Boolean     = false
    def frisian: Boolean              = false
    def frysk: Boolean                = false
    def antidraughts: Boolean         = false
    def breakthrough: Boolean         = false
    def russian: Boolean              = false
    def brazilian: Boolean            = false
    def pool: Boolean                 = false
    def portuguese: Boolean           = false
    def english: Boolean              = false
    def draughtsFromPosition: Boolean = false

    def shogi: Boolean       = v.shogi
    def xiangqi: Boolean     = v.xiangqi
    def minishogi: Boolean   = v.minishogi
    def minixiangqi: Boolean = v.minixiangqi
    def flipello: Boolean    = v.flipello
    def flipello10: Boolean  = v.flipello10
    def amazons: Boolean     = v.amazons

    def oware: Boolean = false

    def togyzkumalak: Boolean = false
    def go9x9: Boolean        = false
    def go13x13: Boolean      = false
    def go19x19: Boolean      = false

    def standardVariant: Boolean     = standard || draughtsStandard
    def fromPositionVariant: Boolean = fromPosition || draughtsFromPosition
    def exoticChessVariant: Boolean  = false
    def frisianVariant: Boolean      = false
    def draughts64Variant: Boolean   = false

    def exotic: Boolean = v.exotic

    def baseVariant: Boolean      = v.baseVariant
    def fenVariant: Boolean       = v.fenVariant
    def hasAnalysisBoard: Boolean = v.hasAnalysisBoard
    def hasFishnet: Boolean       = v.hasFishnet

    def p1IsBetterVariant: Boolean = v.p1IsBetterVariant
    def blindModeVariant: Boolean  = v.blindModeVariant

    def materialImbalanceVariant: Boolean = v.materialImbalanceVariant

    def dropsVariant: Boolean     = v.dropsVariant
    def onlyDropsVariant: Boolean = v.onlyDropsVariant
    def hasGameScore: Boolean     = v.hasGameScore
    def canOfferDraw: Boolean     = v.canOfferDraw

    def perfId: Int    = v.perfId
    def perfIcon: Char = v.perfIcon

    def initialFen: FEN     = FEN.FairySF(v.initialFen)
    def startPlayer: Player = v.startPlayer
    def plysPerTurn: Int    = v.plysPerTurn

    def isValidPromotion(promotion: Option[PromotableRole]): Boolean = promotion match {
      case Some(Role.FairySFPromotableRole(pr)) => v.isValidPromotion(pr.some)
      case None                                 => v.isValidPromotion(None)
      case _                                    => sys.error("Not passed FairySF objects")
    }

    def checkmate(situation: Situation): Boolean = situation match {
      case Situation.FairySF(situation) => v.checkmate(situation)
      case _                            => sys.error("Not passed FairySF objects")
    }

    def stalemateIsDraw: Boolean = v.stalemateIsDraw

    def winner(situation: Situation): Option[Player] = situation match {
      case Situation.FairySF(situation) => v.winner(situation)
      case _                            => sys.error("Not passed FairySF objects")
    }

    @nowarn def specialEnd(situation: Situation): Boolean = situation match {
      case Situation.FairySF(situation) => v.specialEnd(situation)
      case _                            => sys.error("Not passed FairySF objects")
    }

    @nowarn def specialDraw(situation: Situation): Boolean = situation match {
      case Situation.FairySF(situation) => v.specialDraw(situation)
      case _                            => sys.error("Not passed FairySF objects")
    }

    def hasMoveEffects: Boolean = v.hasMoveEffects

    def addVariantEffect(move: Move): Move            = move match {
      case Move.FairySF(move) => Move.FairySF(v.addVariantEffect(move))
      case _                  => sys.error("Not passed FairySF objects")
    }
    def valid(board: Board, strict: Boolean): Boolean = board match {
      case Board.FairySF(board) => v.valid(board, strict)
      case _                    => sys.error("Not passed FairySF objects")
    }

    val roles: List[Role] = v.roles.map(Role.FairySFRole)

    override def equals(that: Any): Boolean = that match {
      case FairySF(v2) => v2.equals(v)
      case _           => false
    }

    def chessVariant: chess.variant.Variant = sys.error("Unimplemented for FairySF")
    def gameLogic: GameLogic                = GameLogic.FairySF()
    def gameFamily: GameFamily              = v.gameFamily

    def playerNames: Map[Player, String]  = gameFamily.playerNames
    def playerColors: Map[Player, String] = gameFamily.playerColors
  }

  case class Samurai(v: samurai.variant.Variant)
      extends Variant(
        id = v.id,
        key = v.key,
        fishnetKey = v.key,
        name = v.name,
        standardInitialPosition = v.standardInitialPosition
      ) {

    def toChess          = sys.error("Can't convert samurai to chess")
    def toDraughts       = sys.error("Can't convert samurai to draughts")
    def toFairySF        = sys.error("Can't convert samurai to fairysf")
    def toSamurai        = v
    def toTogyzkumalak   = sys.error("Can't convert samurai to togyzkumalak")
    def toGo             = sys.error("Can't convert samurai to go")
    def pieces: PieceMap = v.pieces.map { case (pos, (piece, count)) =>
      (Pos.Samurai(pos), (Piece.Samurai(piece), count))
    }

    def standard: Boolean      = false
    def chess960: Boolean      = false
    def fromPosition: Boolean  = false
    def kingOfTheHill: Boolean = false
    def threeCheck: Boolean    = false
    def fiveCheck: Boolean     = false
    def antichess: Boolean     = false
    def atomic: Boolean        = false
    def horde: Boolean         = false
    def racingKings: Boolean   = false
    def crazyhouse: Boolean    = false
    def linesOfAction: Boolean = false
    def noCastling: Boolean    = false
    def scrambledEggs: Boolean = false

    def draughtsStandard: Boolean     = false
    def frisian: Boolean              = false
    def frysk: Boolean                = false
    def antidraughts: Boolean         = false
    def breakthrough: Boolean         = false
    def russian: Boolean              = false
    def brazilian: Boolean            = false
    def pool: Boolean                 = false
    def portuguese: Boolean           = false
    def english: Boolean              = false
    def draughtsFromPosition: Boolean = false

    def shogi: Boolean       = false
    def xiangqi: Boolean     = false
    def minishogi: Boolean   = false
    def minixiangqi: Boolean = false
    def flipello: Boolean    = false
    def flipello10: Boolean  = false
    def amazons: Boolean     = false

    def oware: Boolean = v.oware

    def togyzkumalak: Boolean = false
    def go9x9: Boolean        = false
    def go13x13: Boolean      = false
    def go19x19: Boolean      = false

    def standardVariant: Boolean     = standard || draughtsStandard
    def fromPositionVariant: Boolean = fromPosition || draughtsFromPosition
    def exoticChessVariant: Boolean  = false
    def frisianVariant: Boolean      = false
    def draughts64Variant: Boolean   = false

    def exotic: Boolean = v.exotic

    def baseVariant: Boolean      = v.baseVariant
    def fenVariant: Boolean       = v.fenVariant
    def hasAnalysisBoard: Boolean = v.hasAnalysisBoard
    def hasFishnet: Boolean       = v.hasFishnet

    def p1IsBetterVariant: Boolean = v.p1IsBetterVariant
    def blindModeVariant: Boolean  = v.blindModeVariant

    def materialImbalanceVariant: Boolean = v.materialImbalanceVariant

    def dropsVariant: Boolean     = false
    def onlyDropsVariant: Boolean = false
    def hasGameScore: Boolean     = true
    def canOfferDraw: Boolean     = v.canOfferDraw

    def perfId: Int    = v.perfId
    def perfIcon: Char = v.perfIcon

    def initialFen: FEN     = FEN.Samurai(v.initialFen)
    def startPlayer: Player = v.startPlayer
    def plysPerTurn: Int    = v.plysPerTurn

    def isValidPromotion(promotion: Option[PromotableRole]): Boolean = false

    def checkmate(situation: Situation): Boolean = situation match {
      case Situation.Samurai(situation) => false
      case _                            => sys.error("Not passed Samurai objects")
    }

    def stalemateIsDraw: Boolean = v.stalemateIsDraw

    def winner(situation: Situation): Option[Player] = situation match {
      case Situation.Samurai(situation) => v.winner(situation)
      case _                            => sys.error("Not passed Samurai objects")
    }

    @nowarn def specialEnd(situation: Situation): Boolean = situation match {
      case Situation.Samurai(situation) => v.specialEnd(situation)
      case _                            => sys.error("Not passed Samurai objects")
    }

    @nowarn def specialDraw(situation: Situation): Boolean = situation match {
      case Situation.Samurai(situation) => v.specialDraw(situation)
      case _                            => sys.error("Not passed Samurai objects")
    }

    def hasMoveEffects: Boolean = v.hasMoveEffects

    def addVariantEffect(move: Move): Move            = move match {
      case Move.Samurai(move) => Move.Samurai(v.addVariantEffect(move))
      case _                  => sys.error("Not passed Samurai objects")
    }
    def valid(board: Board, strict: Boolean): Boolean = board match {
      case Board.Samurai(board) => v.valid(board, strict)
      case _                    => sys.error("Not passed Samurai objects")
    }

    val roles: List[Role] = v.roles.map(Role.SamuraiRole)

    override def equals(that: Any): Boolean = that match {
      case Samurai(v2) => v2.equals(v)
      case _           => false
    }

    def chessVariant: chess.variant.Variant = sys.error("Unimplemented for Samurai")
    def gameLogic: GameLogic                = GameLogic.Samurai()
    def gameFamily: GameFamily              = v.gameFamily

    def playerNames: Map[Player, String]  = gameFamily.playerNames
    def playerColors: Map[Player, String] = gameFamily.playerColors
  }

  case class Togyzkumalak(v: togyzkumalak.variant.Variant)
      extends Variant(
        id = v.id,
        key = v.key,
        fishnetKey = v.key,
        name = v.name,
        standardInitialPosition = v.standardInitialPosition
      ) {

    def toChess          = sys.error("Can't convert togyzkumalak to chess")
    def toDraughts       = sys.error("Can't convert togyzkumalak to draughts")
    def toFairySF        = sys.error("Can't convert togyzkumalak to fairysf")
    def toSamurai        = sys.error("Can't convert togyzkumalak to samurai")
    def toTogyzkumalak   = v
    def toGo             = sys.error("Can't convert togyzkumalak to go")
    def pieces: PieceMap = v.pieces.map { case (pos, (piece, count)) =>
      (Pos.Togyzkumalak(pos), (Piece.Togyzkumalak(piece), count))
    }

    def standard: Boolean      = false
    def chess960: Boolean      = false
    def fromPosition: Boolean  = false
    def kingOfTheHill: Boolean = false
    def threeCheck: Boolean    = false
    def fiveCheck: Boolean     = false
    def antichess: Boolean     = false
    def atomic: Boolean        = false
    def horde: Boolean         = false
    def racingKings: Boolean   = false
    def crazyhouse: Boolean    = false
    def linesOfAction: Boolean = false
    def noCastling: Boolean    = false
    def scrambledEggs: Boolean = false

    def draughtsStandard: Boolean     = false
    def frisian: Boolean              = false
    def frysk: Boolean                = false
    def antidraughts: Boolean         = false
    def breakthrough: Boolean         = false
    def russian: Boolean              = false
    def brazilian: Boolean            = false
    def pool: Boolean                 = false
    def portuguese: Boolean           = false
    def english: Boolean              = false
    def draughtsFromPosition: Boolean = false

    def shogi: Boolean       = false
    def xiangqi: Boolean     = false
    def minishogi: Boolean   = false
    def minixiangqi: Boolean = false
    def flipello: Boolean    = false
    def flipello10: Boolean  = false
    def amazons: Boolean     = false

    def oware: Boolean = false

    def togyzkumalak: Boolean = v.togyzkumalak
    def go9x9: Boolean        = false
    def go13x13: Boolean      = false
    def go19x19: Boolean      = false

    def standardVariant: Boolean     = standard || draughtsStandard
    def fromPositionVariant: Boolean = fromPosition || draughtsFromPosition
    def exoticChessVariant: Boolean  = false
    def frisianVariant: Boolean      = false
    def draughts64Variant: Boolean   = false

    def exotic: Boolean = v.exotic

    def baseVariant: Boolean      = v.baseVariant
    def fenVariant: Boolean       = v.fenVariant
    def hasAnalysisBoard: Boolean = v.hasAnalysisBoard
    def hasFishnet: Boolean       = v.hasFishnet

    def p1IsBetterVariant: Boolean = v.p1IsBetterVariant
    def blindModeVariant: Boolean  = v.blindModeVariant

    def materialImbalanceVariant: Boolean = v.materialImbalanceVariant

    def dropsVariant: Boolean     = false
    def onlyDropsVariant: Boolean = false
    def hasGameScore: Boolean     = true
    def canOfferDraw: Boolean     = v.canOfferDraw

    def perfId: Int    = v.perfId
    def perfIcon: Char = v.perfIcon

    def initialFen: FEN     = FEN.Togyzkumalak(v.initialFen)
    def startPlayer: Player = v.startPlayer
    def plysPerTurn: Int    = v.plysPerTurn

    def isValidPromotion(promotion: Option[PromotableRole]): Boolean = false

    def checkmate(situation: Situation): Boolean = situation match {
      case Situation.Togyzkumalak(situation) => false
      case _                                 => sys.error("Not passed Togyzkumalak objects")
    }

    def stalemateIsDraw: Boolean = v.stalemateIsDraw

    def winner(situation: Situation): Option[Player] = situation match {
      case Situation.Togyzkumalak(situation) => v.winner(situation)
      case _                                 => sys.error("Not passed Togyzkumalak objects")
    }

    @nowarn def specialEnd(situation: Situation): Boolean = situation match {
      case Situation.Togyzkumalak(situation) => v.specialEnd(situation)
      case _                                 => sys.error("Not passed Togyzkumalak objects")
    }

    @nowarn def specialDraw(situation: Situation): Boolean = situation match {
      case Situation.Togyzkumalak(situation) => v.specialDraw(situation)
      case _                                 => sys.error("Not passed Togyzkumalak objects")
    }

    def hasMoveEffects: Boolean = v.hasMoveEffects

    def addVariantEffect(move: Move): Move            = move match {
      case Move.Togyzkumalak(move) => Move.Togyzkumalak(v.addVariantEffect(move))
      case _                       => sys.error("Not passed Togyzkumalak objects")
    }
    def valid(board: Board, strict: Boolean): Boolean = board match {
      case Board.Togyzkumalak(board) => v.valid(board, strict)
      case _                         => sys.error("Not passed Togyzkumalak objects")
    }

    val roles: List[Role] = v.roles.map(Role.TogyzkumalakRole)

    override def equals(that: Any): Boolean = that match {
      case Togyzkumalak(v2) => v2.equals(v)
      case _                => false
    }

    def chessVariant: chess.variant.Variant = sys.error("Unimplemented for Togyzkumalak")
    def gameLogic: GameLogic                = GameLogic.Togyzkumalak()
    def gameFamily: GameFamily              = v.gameFamily

    def playerNames: Map[Player, String]  = gameFamily.playerNames
    def playerColors: Map[Player, String] = gameFamily.playerColors
  }

  case class Go(v: go.variant.Variant)
      extends Variant(
        id = v.id,
        key = v.key,
        fishnetKey = v.key,
        name = v.name,
        standardInitialPosition = v.standardInitialPosition
      ) {

    def toChess          = sys.error("Can't convert go to chess")
    def toDraughts       = sys.error("Can't convert go to draughts")
    def toFairySF        = sys.error("Can't convert go to fairysf")
    def toSamurai        = sys.error("Can't convert go to samurai")
    def toTogyzkumalak   = sys.error("Can't convert go to togyzkumalak")
    def toGo             = v
    def pieces: PieceMap =
      v.pieces.map { case (pos, piece) => (Pos.Go(pos), (Piece.Go(piece), 1)) }

    def standard: Boolean      = false
    def chess960: Boolean      = false
    def fromPosition: Boolean  = false
    def kingOfTheHill: Boolean = false
    def threeCheck: Boolean    = false
    def fiveCheck: Boolean     = false
    def antichess: Boolean     = false
    def atomic: Boolean        = false
    def horde: Boolean         = false
    def racingKings: Boolean   = false
    def crazyhouse: Boolean    = false
    def linesOfAction: Boolean = false
    def noCastling: Boolean    = false
    def scrambledEggs: Boolean = false

    def draughtsStandard: Boolean     = false
    def frisian: Boolean              = false
    def frysk: Boolean                = false
    def antidraughts: Boolean         = false
    def breakthrough: Boolean         = false
    def russian: Boolean              = false
    def brazilian: Boolean            = false
    def pool: Boolean                 = false
    def portuguese: Boolean           = false
    def english: Boolean              = false
    def draughtsFromPosition: Boolean = false

    def shogi: Boolean       = false
    def xiangqi: Boolean     = false
    def minishogi: Boolean   = false
    def minixiangqi: Boolean = false
    def flipello: Boolean    = false
    def flipello10: Boolean  = false
    def amazons: Boolean     = false

    def oware: Boolean = false

    def togyzkumalak: Boolean = false
    def go9x9: Boolean        = v.go9x9
    def go13x13: Boolean      = v.go13x13
    def go19x19: Boolean      = v.go19x19

    def standardVariant: Boolean     = standard || draughtsStandard
    def fromPositionVariant: Boolean = fromPosition || draughtsFromPosition
    def exoticChessVariant: Boolean  = false
    def frisianVariant: Boolean      = false
    def draughts64Variant: Boolean   = false

    def exotic: Boolean = v.exotic

    def baseVariant: Boolean      = v.baseVariant
    def fenVariant: Boolean       = v.fenVariant
    def hasAnalysisBoard: Boolean = v.hasAnalysisBoard
    def hasFishnet: Boolean       = v.hasFishnet

    def p1IsBetterVariant: Boolean = v.p1IsBetterVariant
    def blindModeVariant: Boolean  = v.blindModeVariant

    def materialImbalanceVariant: Boolean = v.materialImbalanceVariant

    def dropsVariant: Boolean     = true
    def onlyDropsVariant: Boolean = true
    def hasGameScore: Boolean     = true
    def canOfferDraw: Boolean     = v.canOfferDraw

    def perfId: Int    = v.perfId
    def perfIcon: Char = v.perfIcon

    def initialFen: FEN     = FEN.Go(v.initialFen)
    def startPlayer: Player = v.startPlayer
    def plysPerTurn: Int    = v.plysPerTurn

    def isValidPromotion(promotion: Option[PromotableRole]): Boolean = false

    def checkmate(situation: Situation): Boolean = situation match {
      case Situation.Go(situation) => false
      case _                       => sys.error("Not passed Go objects")
    }

    def stalemateIsDraw: Boolean = v.stalemateIsDraw

    def winner(situation: Situation): Option[Player] = situation match {
      case Situation.Go(situation) => v.winner(situation)
      case _                       => sys.error("Not passed Go objects")
    }

    @nowarn def specialEnd(situation: Situation): Boolean = situation match {
      case Situation.Go(situation) => v.specialEnd(situation)
      case _                       => sys.error("Not passed Go objects")
    }

    @nowarn def specialDraw(situation: Situation): Boolean = situation match {
      case Situation.Go(situation) => v.specialDraw(situation)
      case _                       => sys.error("Not passed Go objects")
    }

    def hasMoveEffects: Boolean = v.hasMoveEffects

    def addVariantEffect(move: Move): Move = sys.error("No moves in Go")

    def addVariantEffect(drop: Drop): Drop            = drop match {
      case Drop.Go(drop) => Drop.Go(v.addVariantEffect(drop))
      case _             => sys.error("Not passed Go objects")
    }
    def valid(board: Board, strict: Boolean): Boolean = board match {
      case Board.Go(board) => v.valid(board, strict)
      case _               => sys.error("Not passed Go objects")
    }

    val roles: List[Role] = v.roles.map(Role.GoRole)

    override def equals(that: Any): Boolean = that match {
      case Go(v2) => v2.equals(v)
      case _      => false
    }

    def chessVariant: chess.variant.Variant = sys.error("Unimplemented for Go")
    def gameLogic: GameLogic                = GameLogic.Go()
    def gameFamily: GameFamily              = v.gameFamily

    def playerNames: Map[Player, String]  = gameFamily.playerNames
    def playerColors: Map[Player, String] = gameFamily.playerColors
  }

  def all: List[Variant] =
    chess.variant.Variant.all.map(Chess) :::
      draughts.variant.Variant.all.map(Draughts) :::
      fairysf.variant.Variant.all.map(FairySF) :::
      samurai.variant.Variant.all.map(Samurai) :::
      togyzkumalak.variant.Variant.all.map(Togyzkumalak) :::
      go.variant.Variant.all.map(Go)

  def byId = all map { v => (v.id, v) } toMap

  def byKey = all map { v => (v.key, v) } toMap

  def all(lib: GameLogic): List[Variant] = lib match {
    case GameLogic.Draughts()     => draughts.variant.Variant.all.map(Draughts)
    case GameLogic.Chess()        => chess.variant.Variant.all.map(Chess)
    case GameLogic.FairySF()      => fairysf.variant.Variant.all.map(FairySF)
    case GameLogic.Samurai()      => samurai.variant.Variant.all.map(Samurai)
    case GameLogic.Togyzkumalak() => togyzkumalak.variant.Variant.all.map(Togyzkumalak)
    case GameLogic.Go()           => go.variant.Variant.all.map(Go)
  }

  def byId(lib: GameLogic) = all(lib) map { v =>
    (v.id, v)
  } toMap

  def byKey(lib: GameLogic) = all(lib) map { v =>
    (v.key, v)
  } toMap

  def default(lib: GameLogic): Variant = lib match {
    case GameLogic.Draughts()     => Draughts(draughts.variant.Variant.default)
    case GameLogic.Chess()        => Chess(chess.variant.Variant.default)
    case GameLogic.FairySF()      => FairySF(fairysf.variant.Variant.default)
    case GameLogic.Samurai()      => Samurai(samurai.variant.Variant.default)
    case GameLogic.Togyzkumalak() => Togyzkumalak(togyzkumalak.variant.Variant.default)
    case GameLogic.Go()           => Go(go.variant.Variant.default)
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
    case GameLogic.Draughts()     => draughts.variant.Variant.openingSensibleVariants.map(Draughts)
    case GameLogic.Chess()        => chess.variant.Variant.openingSensibleVariants.map(Chess)
    case GameLogic.FairySF()      => fairysf.variant.Variant.openingSensibleVariants.map(FairySF)
    case GameLogic.Samurai()      => samurai.variant.Variant.openingSensibleVariants.map(Samurai)
    case GameLogic.Togyzkumalak() => togyzkumalak.variant.Variant.openingSensibleVariants.map(Togyzkumalak)
    case GameLogic.Go()           => go.variant.Variant.openingSensibleVariants.map(Go)
  }

  def divisionSensibleVariants(lib: GameLogic): Set[Variant] = lib match {
    case GameLogic.Draughts()     => draughts.variant.Variant.divisionSensibleVariants.map(Draughts)
    case GameLogic.Chess()        => chess.variant.Variant.divisionSensibleVariants.map(Chess)
    case GameLogic.FairySF()      => fairysf.variant.Variant.divisionSensibleVariants.map(FairySF)
    case GameLogic.Samurai()      => samurai.variant.Variant.divisionSensibleVariants.map(Samurai)
    case GameLogic.Togyzkumalak() => togyzkumalak.variant.Variant.divisionSensibleVariants.map(Togyzkumalak)
    case GameLogic.Go()           => go.variant.Variant.divisionSensibleVariants.map(Go)
  }

  def libStandard(lib: GameLogic): Variant = lib match {
    case GameLogic.Draughts()     => Variant.Draughts(draughts.variant.Standard)
    case GameLogic.Chess()        => Variant.Chess(chess.variant.Standard)
    case GameLogic.FairySF()      => Variant.FairySF(fairysf.variant.Shogi)
    case GameLogic.Samurai()      => Variant.Samurai(samurai.variant.Oware)
    case GameLogic.Togyzkumalak() => Variant.Togyzkumalak(togyzkumalak.variant.Togyzkumalak)
    case GameLogic.Go()           => Variant.Go(go.variant.Go19x19)
  }

  // todo all games will be allowed from position (go has 3 variants already!)
  @deprecated("this method will be removed")
  def libFromPosition(lib: GameLogic): Variant = lib match {
    case GameLogic.Draughts()     => Variant.Draughts(draughts.variant.FromPosition)
    case GameLogic.Chess()        => Variant.Chess(chess.variant.FromPosition)
    // TODO: Decide how we do from position for other game logics
    case GameLogic.FairySF()      => Variant.FairySF(fairysf.variant.Shogi)
    case GameLogic.Samurai()      => Variant.Samurai(samurai.variant.Oware)
    case GameLogic.Togyzkumalak() => Variant.Togyzkumalak(togyzkumalak.variant.Togyzkumalak)
    case GameLogic.Go()           => Variant.Go(go.variant.Go19x19)
  }

  def wrap(v: chess.variant.Variant)        = Chess(v)
  def wrap(v: draughts.variant.Variant)     = Draughts(v)
  def wrap(v: fairysf.variant.Variant)      = FairySF(v)
  def wrap(v: samurai.variant.Variant)      = Samurai(v)
  def wrap(v: togyzkumalak.variant.Variant) = Togyzkumalak(v)
  def wrap(v: go.variant.Variant)           = Go(v)

}
