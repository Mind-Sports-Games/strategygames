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
  def toBackgammon: backgammon.variant.Variant
  def toAbalone: abalone.variant.Variant

  def pieces: PieceMap

  def standardVariant: Boolean
  def fromPositionVariant: Boolean
  def exoticChessVariant: Boolean
  def frisianVariant: Boolean
  def draughts64Variant: Boolean

  def exotic: Boolean

  // used in lila setup/src/main/Config.scala
  def baseVariant: Boolean
  def fenVariant: Boolean
  def variableInitialFen: Boolean

  def hasAnalysisBoard: Boolean
  def hasFishnet: Boolean

  // used in lila modules/game/src/main/Game.scala
  def p1IsBetterVariant: Boolean
  def blindModeVariant: Boolean
  // used in lila modules/playban/src/main/RageSit.scala
  def materialImbalanceVariant: Boolean

  def dropsVariant: Boolean
  def onlyDropsVariant: Boolean
  def hasDetachedPocket: Boolean
  def hasGameScore: Boolean

  def canOfferDraw: Boolean
  def ignoreSubmitAction: Boolean

  def perfId: Int
  def perfIcon: Char

  def initialFen: FEN
  def initialFens: List[FEN]
  def startPlayer: Player

  def recalcStartPlayerForStats: Boolean

  def isValidPromotion(promotion: Option[PromotableRole]): Boolean

  def checkmate(situation: Situation): Boolean

  def stalemateIsDraw: Boolean

  def useRuleOfGinOnInsufficientMaterial: Boolean

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
    def toBackgammon                   = sys.error("Can't convert chess to backgammon")
    def toAbalone                      = sys.error("Can't convert chess to abalone")

    def pieces: PieceMap =
      v.pieces.map { case (pos, piece) => (Pos.Chess(pos), (Piece.Chess(piece), 1)) }

    def standardVariant: Boolean     = v == chess.variant.Standard
    def fromPositionVariant: Boolean = v == chess.variant.FromPosition
    def exoticChessVariant: Boolean  = v.exoticChessVariant
    def frisianVariant: Boolean      = false
    def draughts64Variant: Boolean   = false

    def exotic: Boolean = v.exotic

    def baseVariant: Boolean        = v.baseVariant
    def fenVariant: Boolean         = v.fenVariant
    def variableInitialFen: Boolean = v.variableInitialFen

    def hasAnalysisBoard: Boolean = v.hasAnalysisBoard
    def hasFishnet: Boolean       = v.hasFishnet

    def p1IsBetterVariant: Boolean = v.p1IsBetterVariant
    def blindModeVariant: Boolean  = v.blindModeVariant

    def materialImbalanceVariant: Boolean = v.materialImbalanceVariant

    def dropsVariant: Boolean      = v.dropsVariant
    def onlyDropsVariant: Boolean  = false
    def hasDetachedPocket: Boolean = v.hasDetachedPocket
    def hasGameScore: Boolean      = false

    def canOfferDraw: Boolean       = v.canOfferDraw
    def ignoreSubmitAction: Boolean = false

    def perfId: Int    = v.perfId
    def perfIcon: Char = v.perfIcon

    def initialFen: FEN        = FEN.Chess(v.initialFen)
    def initialFens: List[FEN] = List(initialFen)
    def startPlayer: Player    = v.startPlayer

    def recalcStartPlayerForStats: Boolean = false

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

    def useRuleOfGinOnInsufficientMaterial: Boolean = false

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
    def toBackgammon   = sys.error("Can't convert draughts to backgammon")
    def toAbalone      = sys.error("Can't convert draughts to abalone")

    def pieces: PieceMap =
      v.pieces.map { case (pos, piece) => (Pos.Draughts(pos), (Piece.Draughts(piece), 1)) }

    def standardVariant: Boolean     = v == draughts.variant.Standard
    def fromPositionVariant: Boolean = v == draughts.variant.FromPosition
    def exoticChessVariant: Boolean  = false
    def frisianVariant: Boolean      = v.frisianVariant
    def draughts64Variant: Boolean   = v.draughts64Variant

    def exotic: Boolean = v.exotic

    def baseVariant: Boolean        = v.baseVariant
    def fenVariant: Boolean         = v.fenVariant
    def variableInitialFen: Boolean = v.variableInitialFen

    def hasAnalysisBoard: Boolean = v.hasAnalysisBoard
    def hasFishnet: Boolean       = v.hasFishnet

    def p1IsBetterVariant: Boolean = v.p1IsBetterVariant
    def blindModeVariant: Boolean  = v.blindModeVariant

    def materialImbalanceVariant: Boolean = v.materialImbalanceVariant

    def dropsVariant: Boolean      = false
    def onlyDropsVariant: Boolean  = false
    def hasDetachedPocket: Boolean = false
    def hasGameScore: Boolean      = false

    def canOfferDraw: Boolean       = v.canOfferDraw
    def ignoreSubmitAction: Boolean = false

    def perfId: Int    = v.perfId
    def perfIcon: Char = v.perfIcon

    def initialFen: FEN        = FEN.Draughts(v.initialFen)
    def initialFens: List[FEN] = List(initialFen)
    def startPlayer: Player    = v.startPlayer

    def recalcStartPlayerForStats: Boolean = false

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

    def useRuleOfGinOnInsufficientMaterial: Boolean = false

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

    def toChess        = sys.error("Can't convert fairysf to chess")
    def toDraughts     = sys.error("Can't convert fairysf to draughts")
    def toFairySF      = v
    def toSamurai      = sys.error("Can't convert fairysf to samurai")
    def toTogyzkumalak = sys.error("Can't convert fairysf to togyzkumalak")
    def toGo           = sys.error("Can't convert fairysf to go")
    def toBackgammon   = sys.error("Can't convert fairysf to backgammon")
    def toAbalone      = sys.error("Can't convert fairysf to abalone")

    def pieces: PieceMap =
      v.pieces.map { case (pos, piece) => (Pos.FairySF(pos), (Piece.FairySF(piece), 1)) }

    def standardVariant: Boolean     = false
    def fromPositionVariant: Boolean = false
    def exoticChessVariant: Boolean  = false
    def frisianVariant: Boolean      = false
    def draughts64Variant: Boolean   = false

    def exotic: Boolean = v.exotic

    def baseVariant: Boolean        = v.baseVariant
    def fenVariant: Boolean         = v.fenVariant
    def variableInitialFen: Boolean = v.variableInitialFen

    def hasAnalysisBoard: Boolean = v.hasAnalysisBoard
    def hasFishnet: Boolean       = v.hasFishnet

    def p1IsBetterVariant: Boolean = v.p1IsBetterVariant
    def blindModeVariant: Boolean  = v.blindModeVariant

    def materialImbalanceVariant: Boolean = v.materialImbalanceVariant

    def dropsVariant: Boolean      = v.dropsVariant
    def onlyDropsVariant: Boolean  = v.onlyDropsVariant
    def hasDetachedPocket: Boolean = v.hasDetachedPocket
    def hasGameScore: Boolean      = v.hasGameScore

    def canOfferDraw: Boolean       = v.canOfferDraw
    def ignoreSubmitAction: Boolean = false

    def perfId: Int    = v.perfId
    def perfIcon: Char = v.perfIcon

    def initialFen: FEN        = FEN.FairySF(v.initialFen)
    def initialFens: List[FEN] = List(initialFen)
    def startPlayer: Player    = v.startPlayer

    def recalcStartPlayerForStats: Boolean = false

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

    def useRuleOfGinOnInsufficientMaterial: Boolean = false

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

    def toChess        = sys.error("Can't convert samurai to chess")
    def toDraughts     = sys.error("Can't convert samurai to draughts")
    def toFairySF      = sys.error("Can't convert samurai to fairysf")
    def toSamurai      = v
    def toTogyzkumalak = sys.error("Can't convert samurai to togyzkumalak")
    def toGo           = sys.error("Can't convert samurai to go")
    def toBackgammon   = sys.error("Can't convert samurai to backgammon")
    def toAbalone      = sys.error("Can't convert samurai to abalone")

    def pieces: PieceMap = v.pieces.map { case (pos, (piece, count)) =>
      (Pos.Samurai(pos), (Piece.Samurai(piece), count))
    }

    def standardVariant: Boolean     = false
    def fromPositionVariant: Boolean = false
    def exoticChessVariant: Boolean  = false
    def frisianVariant: Boolean      = false
    def draughts64Variant: Boolean   = false

    def exotic: Boolean = v.exotic

    def baseVariant: Boolean        = v.baseVariant
    def fenVariant: Boolean         = v.fenVariant
    def variableInitialFen: Boolean = v.variableInitialFen

    def hasAnalysisBoard: Boolean = v.hasAnalysisBoard
    def hasFishnet: Boolean       = v.hasFishnet

    def p1IsBetterVariant: Boolean = v.p1IsBetterVariant
    def blindModeVariant: Boolean  = v.blindModeVariant

    def materialImbalanceVariant: Boolean = v.materialImbalanceVariant

    def dropsVariant: Boolean      = v.dropsVariant
    def onlyDropsVariant: Boolean  = v.onlyDropsVariant
    def hasDetachedPocket: Boolean = false
    def hasGameScore: Boolean      = v.hasGameScore

    def canOfferDraw: Boolean       = v.canOfferDraw
    def ignoreSubmitAction: Boolean = false

    def perfId: Int    = v.perfId
    def perfIcon: Char = v.perfIcon

    def initialFen: FEN        = FEN.Samurai(v.initialFen)
    def initialFens: List[FEN] = List(initialFen)
    def startPlayer: Player    = v.startPlayer

    def recalcStartPlayerForStats: Boolean = false

    def isValidPromotion(promotion: Option[PromotableRole]): Boolean = false

    def checkmate(situation: Situation): Boolean = situation match {
      case Situation.Samurai(_) => false
      case _                    => sys.error("Not passed Samurai objects")
    }

    def stalemateIsDraw: Boolean = v.stalemateIsDraw

    def useRuleOfGinOnInsufficientMaterial: Boolean = false

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

    def toChess        = sys.error("Can't convert togyzkumalak to chess")
    def toDraughts     = sys.error("Can't convert togyzkumalak to draughts")
    def toFairySF      = sys.error("Can't convert togyzkumalak to fairysf")
    def toSamurai      = sys.error("Can't convert togyzkumalak to samurai")
    def toTogyzkumalak = v
    def toGo           = sys.error("Can't convert togyzkumalak to go")
    def toBackgammon   = sys.error("Can't convert togyzkumalak to backgammon")
    def toAbalone      = sys.error("Can't convert togyzkumalak to abalone")

    def pieces: PieceMap = v.pieces.map { case (pos, (piece, count)) =>
      (Pos.Togyzkumalak(pos), (Piece.Togyzkumalak(piece), count))
    }

    def standardVariant: Boolean     = false
    def fromPositionVariant: Boolean = false
    def exoticChessVariant: Boolean  = false
    def frisianVariant: Boolean      = false
    def draughts64Variant: Boolean   = false

    def exotic: Boolean = v.exotic

    def baseVariant: Boolean        = v.baseVariant
    def fenVariant: Boolean         = v.fenVariant
    def variableInitialFen: Boolean = v.variableInitialFen

    def hasAnalysisBoard: Boolean = v.hasAnalysisBoard
    def hasFishnet: Boolean       = v.hasFishnet

    def p1IsBetterVariant: Boolean = v.p1IsBetterVariant
    def blindModeVariant: Boolean  = v.blindModeVariant

    def materialImbalanceVariant: Boolean = v.materialImbalanceVariant

    def dropsVariant: Boolean      = v.dropsVariant
    def onlyDropsVariant: Boolean  = v.onlyDropsVariant
    def hasDetachedPocket: Boolean = false
    def hasGameScore: Boolean      = v.hasGameScore

    def canOfferDraw: Boolean       = v.canOfferDraw
    def ignoreSubmitAction: Boolean = false

    def perfId: Int    = v.perfId
    def perfIcon: Char = v.perfIcon

    def initialFen: FEN        = FEN.Togyzkumalak(v.initialFen)
    def initialFens: List[FEN] = List(initialFen)
    def startPlayer: Player    = v.startPlayer

    def recalcStartPlayerForStats: Boolean = false

    def isValidPromotion(promotion: Option[PromotableRole]): Boolean = false

    def checkmate(situation: Situation): Boolean = situation match {
      case Situation.Togyzkumalak(_) => false
      case _                         => sys.error("Not passed Togyzkumalak objects")
    }

    def stalemateIsDraw: Boolean = v.stalemateIsDraw

    def useRuleOfGinOnInsufficientMaterial: Boolean = false

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

    def toChess        = sys.error("Can't convert go to chess")
    def toDraughts     = sys.error("Can't convert go to draughts")
    def toFairySF      = sys.error("Can't convert go to fairysf")
    def toSamurai      = sys.error("Can't convert go to samurai")
    def toTogyzkumalak = sys.error("Can't convert go to togyzkumalak")
    def toGo           = v
    def toBackgammon   = sys.error("Can't convert go to backgammon")
    def toAbalone      = sys.error("Can't convert go to abalone")

    def pieces: PieceMap =
      v.pieces.map { case (pos, piece) => (Pos.Go(pos), (Piece.Go(piece), 1)) }

    def standardVariant: Boolean     = false
    def fromPositionVariant: Boolean = false
    def exoticChessVariant: Boolean  = false
    def frisianVariant: Boolean      = false
    def draughts64Variant: Boolean   = false

    def exotic: Boolean = v.exotic

    def baseVariant: Boolean        = v.baseVariant
    def fenVariant: Boolean         = v.fenVariant
    def variableInitialFen: Boolean = v.variableInitialFen

    def hasAnalysisBoard: Boolean = v.hasAnalysisBoard
    def hasFishnet: Boolean       = v.hasFishnet

    def p1IsBetterVariant: Boolean = v.p1IsBetterVariant
    def blindModeVariant: Boolean  = v.blindModeVariant

    def materialImbalanceVariant: Boolean = v.materialImbalanceVariant

    def dropsVariant: Boolean      = v.dropsVariant
    def onlyDropsVariant: Boolean  = v.onlyDropsVariant
    def hasDetachedPocket: Boolean = false
    def hasGameScore: Boolean      = v.hasGameScore

    def canOfferDraw: Boolean       = v.canOfferDraw
    def ignoreSubmitAction: Boolean = false

    def perfId: Int    = v.perfId
    def perfIcon: Char = v.perfIcon

    def initialFen: FEN        = FEN.Go(v.initialFen)
    def initialFens: List[FEN] = List(initialFen)
    def startPlayer: Player    = v.startPlayer

    def recalcStartPlayerForStats: Boolean = false

    def isValidPromotion(promotion: Option[PromotableRole]): Boolean = false

    def checkmate(situation: Situation): Boolean = situation match {
      case Situation.Go(_) => false
      case _               => sys.error("Not passed Go objects")
    }

    def stalemateIsDraw: Boolean = v.stalemateIsDraw

    def useRuleOfGinOnInsufficientMaterial: Boolean = false

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

  case class Backgammon(v: backgammon.variant.Variant)
      extends Variant(
        id = v.id,
        key = v.key,
        fishnetKey = v.key,
        name = v.name,
        standardInitialPosition = v.standardInitialPosition
      ) {

    def toChess        = sys.error("Can't convert backgammon to chess")
    def toDraughts     = sys.error("Can't convert backgammon to draughts")
    def toFairySF      = sys.error("Can't convert backgammon to fairysf")
    def toSamurai      = sys.error("Can't convert backgammon to samurai")
    def toTogyzkumalak = sys.error("Can't convert backgammon to togyzkumalak")
    def toGo           = sys.error("Can't convert backgammon to go")
    def toBackgammon   = v
    def toAbalone      = sys.error("Can't convert backgammon to abalone")

    def pieces: PieceMap = v.pieces.map { case (pos, (piece, count)) =>
      (Pos.Backgammon(pos), (Piece.Backgammon(piece), count))
    }

    def standardVariant: Boolean     = false
    def fromPositionVariant: Boolean = false
    def exoticChessVariant: Boolean  = false
    def frisianVariant: Boolean      = false
    def draughts64Variant: Boolean   = false

    def exotic: Boolean = v.exotic

    def baseVariant: Boolean        = v.baseVariant
    def fenVariant: Boolean         = v.fenVariant
    def variableInitialFen: Boolean = v.variableInitialFen

    def hasAnalysisBoard: Boolean = v.hasAnalysisBoard
    def hasFishnet: Boolean       = v.hasFishnet

    def p1IsBetterVariant: Boolean = v.p1IsBetterVariant
    def blindModeVariant: Boolean  = v.blindModeVariant

    def materialImbalanceVariant: Boolean = v.materialImbalanceVariant

    def dropsVariant: Boolean      = v.dropsVariant
    def onlyDropsVariant: Boolean  = v.onlyDropsVariant
    def hasDetachedPocket: Boolean = false
    def hasGameScore: Boolean      = v.hasGameScore

    def canOfferDraw: Boolean       = v.canOfferDraw
    def ignoreSubmitAction: Boolean = v.ignoreSubmitAction

    def perfId: Int    = v.perfId
    def perfIcon: Char = v.perfIcon

    def initialFen: FEN        = FEN.Backgammon(v.initialFen)
    def initialFens: List[FEN] = v.initialFens.map(FEN.Backgammon)
    def startPlayer: Player    = v.startPlayer

    def recalcStartPlayerForStats: Boolean = v.recalcStartPlayerForStats

    def isValidPromotion(promotion: Option[PromotableRole]): Boolean = false

    def checkmate(situation: Situation): Boolean = situation match {
      case Situation.Backgammon(_) => false
      case _                       => sys.error("Not passed Backgammon objects")
    }

    def stalemateIsDraw: Boolean = false

    def useRuleOfGinOnInsufficientMaterial: Boolean = v.useRuleOfGinOnInsufficientMaterial

    def winner(situation: Situation): Option[Player] = situation match {
      case Situation.Backgammon(situation) => v.winner(situation)
      case _                               => sys.error("Not passed Backgammon objects")
    }

    @nowarn def specialEnd(situation: Situation): Boolean = situation match {
      case Situation.Backgammon(situation) => v.specialEnd(situation)
      case _                               => sys.error("Not passed Backgammon objects")
    }

    def specialDraw(situation: Situation): Boolean = situation match {
      case Situation.Backgammon(_) => false
      case _                       => sys.error("Not passed Backgammon objects")
    }

    // backgammon has no variant effects for any action
    def hasMoveEffects: Boolean            = false
    def addVariantEffect(move: Move): Move = move

    def valid(board: Board, strict: Boolean): Boolean = board match {
      case Board.Backgammon(board) => v.valid(board, strict)
      case _                       => sys.error("Not passed Backgammon objects")
    }

    val roles: List[Role] = v.roles.map(Role.BackgammonRole)

    override def equals(that: Any): Boolean = that match {
      case Backgammon(v2) => v2.equals(v)
      case _              => false
    }

    def chessVariant: chess.variant.Variant = sys.error("Unimplemented for Backgammon")
    def gameLogic: GameLogic                = GameLogic.Backgammon()
    def gameFamily: GameFamily              = v.gameFamily

    def playerNames: Map[Player, String]  = gameFamily.playerNames
    def playerColors: Map[Player, String] = gameFamily.playerColors
  }

  case class Abalone(v: abalone.variant.Variant)
      extends Variant(
        id = v.id,
        key = v.key,
        fishnetKey = v.key,
        name = v.name,
        standardInitialPosition = v.standardInitialPosition
      ) {

    def toChess        = sys.error("Can't convert abalone to chess")
    def toDraughts     = sys.error("Can't convert abalone to draughts")
    def toFairySF      = sys.error("Can't convert abalone to fairysf")
    def toSamurai      = sys.error("Can't convert abalone to samurai")
    def toTogyzkumalak = sys.error("Can't convert abalone to togyzkumalak")
    def toGo           = sys.error("Can't convert abalone to go")
    def toBackgammon   = sys.error("Can't convert abalone to backgammon")
    def toAbalone      = v

    def pieces: PieceMap =
      v.pieces.map { case (pos, piece) => (Pos.Abalone(pos), (Piece.Abalone(piece), 1)) }

    def standardVariant: Boolean     = false
    def fromPositionVariant: Boolean = false
    def exoticChessVariant: Boolean  = false
    def frisianVariant: Boolean      = false
    def draughts64Variant: Boolean   = false

    def exotic: Boolean = v.exotic

    def baseVariant: Boolean        = v.baseVariant
    def fenVariant: Boolean         = v.fenVariant
    def variableInitialFen: Boolean = v.variableInitialFen

    def hasAnalysisBoard: Boolean = v.hasAnalysisBoard
    def hasFishnet: Boolean       = v.hasFishnet

    def p1IsBetterVariant: Boolean = v.p1IsBetterVariant
    def blindModeVariant: Boolean  = v.blindModeVariant

    def materialImbalanceVariant: Boolean = v.materialImbalanceVariant

    def dropsVariant: Boolean      = false
    def onlyDropsVariant: Boolean  = false
    def hasDetachedPocket: Boolean = false
    def hasGameScore: Boolean      = true

    def canOfferDraw: Boolean       = v.canOfferDraw
    def ignoreSubmitAction: Boolean = false

    def perfId: Int    = v.perfId
    def perfIcon: Char = v.perfIcon

    def initialFen: FEN     = FEN.Abalone(v.initialFen)
    def initialFens: List[FEN] = List(initialFen)
    def startPlayer: Player = v.startPlayer

    def recalcStartPlayerForStats: Boolean = false

    def isValidPromotion(promotion: Option[PromotableRole]): Boolean = false

    def checkmate(situation: Situation): Boolean = situation match {
      case Situation.Abalone(_) => false
      case _                    => sys.error("Not passed Abalone objects")
    }

    def stalemateIsDraw: Boolean = v.stalemateIsDraw

    def useRuleOfGinOnInsufficientMaterial: Boolean = false

    def winner(situation: Situation): Option[Player] = situation match {
      case Situation.Abalone(situation) => v.winner(situation)
      case _                            => sys.error("Not passed Abalone objects")
    }

    @nowarn def specialEnd(situation: Situation): Boolean = situation match {
      case Situation.Abalone(situation) => v.specialEnd(situation)
      case _                            => sys.error("Not passed Abalone objects")
    }

    @nowarn def specialDraw(situation: Situation): Boolean = situation match {
      case Situation.Abalone(situation) => v.specialDraw(situation)
      case _                            => sys.error("Not passed Abalone objects")
    }

    def hasMoveEffects: Boolean = v.hasMoveEffects

    def addVariantEffect(move: Move): Move            = move match {
      case Move.Abalone(move) => Move.Abalone(v.addVariantEffect(move))
      case _                  => sys.error("Not passed Abalone objects")
    }
    def valid(board: Board, strict: Boolean): Boolean = board match {
      case Board.Abalone(board) => v.valid(board, strict)
      case _                    => sys.error("Not passed Abalone objects")
    }

    val roles: List[Role] = v.roles.map(Role.AbaloneRole)

    override def equals(that: Any): Boolean = that match {
      case Abalone(v2) => v2.equals(v)
      case _           => false
    }

    def chessVariant: chess.variant.Variant = sys.error("Unimplemented for Abalone")
    def gameLogic: GameLogic                = GameLogic.Abalone()
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
      go.variant.Variant.all.map(Go) :::
      backgammon.variant.Variant.all.map(Backgammon) :::
      abalone.variant.Variant.all.map(Abalone)

  def byId = all map { v => (v.id, v) } toMap

  def byKey = all map { v => (v.key, v) } toMap

  def all(lib: GameLogic): List[Variant] = lib match {
    case GameLogic.Draughts()     => draughts.variant.Variant.all.map(Draughts)
    case GameLogic.Chess()        => chess.variant.Variant.all.map(Chess)
    case GameLogic.FairySF()      => fairysf.variant.Variant.all.map(FairySF)
    case GameLogic.Samurai()      => samurai.variant.Variant.all.map(Samurai)
    case GameLogic.Togyzkumalak() => togyzkumalak.variant.Variant.all.map(Togyzkumalak)
    case GameLogic.Go()           => go.variant.Variant.all.map(Go)
    case GameLogic.Backgammon()   => backgammon.variant.Variant.all.map(Backgammon)
    case GameLogic.Abalone()      => abalone.variant.Variant.all.map(Abalone)
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
    case GameLogic.Backgammon()   => Backgammon(backgammon.variant.Variant.default)
    case GameLogic.Abalone()      => Abalone(abalone.variant.Variant.default)
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
    case GameLogic.Backgammon()   => backgammon.variant.Variant.openingSensibleVariants.map(Backgammon)
    case GameLogic.Abalone()      => abalone.variant.Variant.openingSensibleVariants.map(Abalone)
  }

  def divisionSensibleVariants(lib: GameLogic): Set[Variant] = lib match {
    case GameLogic.Draughts()     => draughts.variant.Variant.divisionSensibleVariants.map(Draughts)
    case GameLogic.Chess()        => chess.variant.Variant.divisionSensibleVariants.map(Chess)
    case GameLogic.FairySF()      => fairysf.variant.Variant.divisionSensibleVariants.map(FairySF)
    case GameLogic.Samurai()      => samurai.variant.Variant.divisionSensibleVariants.map(Samurai)
    case GameLogic.Togyzkumalak() => togyzkumalak.variant.Variant.divisionSensibleVariants.map(Togyzkumalak)
    case GameLogic.Go()           => go.variant.Variant.divisionSensibleVariants.map(Go)
    case GameLogic.Backgammon()   => backgammon.variant.Variant.divisionSensibleVariants.map(Backgammon)
    case GameLogic.Abalone()      => abalone.variant.Variant.divisionSensibleVariants.map(Abalone)
  }

  def libStandard(lib: GameLogic): Variant = lib match {
    case GameLogic.Draughts()     => Variant.Draughts(draughts.variant.Standard)
    case GameLogic.Chess()        => Variant.Chess(chess.variant.Standard)
    case GameLogic.FairySF()      => Variant.FairySF(fairysf.variant.Shogi)
    case GameLogic.Samurai()      => Variant.Samurai(samurai.variant.Oware)
    case GameLogic.Togyzkumalak() => Variant.Togyzkumalak(togyzkumalak.variant.Togyzkumalak)
    case GameLogic.Go()           => Variant.Go(go.variant.Go19x19)
    case GameLogic.Backgammon()   => Variant.Backgammon(backgammon.variant.Backgammon)
    case GameLogic.Abalone()      => Variant.Abalone(abalone.variant.Abalone)
  }

  // todo all games will be allowed from position (go has 3 variants already!)
  @deprecated("this method will be removed", "10.2.1-pstrat98")
  def libFromPosition(lib: GameLogic): Variant = lib match {
    case GameLogic.Draughts()     => Variant.Draughts(draughts.variant.FromPosition)
    case GameLogic.Chess()        => Variant.Chess(chess.variant.FromPosition)
    // TODO: Decide how we do from position for other game logics
    case GameLogic.FairySF()      => Variant.FairySF(fairysf.variant.Shogi)
    case GameLogic.Samurai()      => Variant.Samurai(samurai.variant.Oware)
    case GameLogic.Togyzkumalak() => Variant.Togyzkumalak(togyzkumalak.variant.Togyzkumalak)
    case GameLogic.Go()           => Variant.Go(go.variant.Go19x19)
    case GameLogic.Backgammon()   => Variant.Backgammon(backgammon.variant.Backgammon)
    case GameLogic.Abalone()      => Variant.Abalone(abalone.variant.Abalone)
  }

  def wrap(v: chess.variant.Variant)        = Chess(v)
  def wrap(v: draughts.variant.Variant)     = Draughts(v)
  def wrap(v: fairysf.variant.Variant)      = FairySF(v)
  def wrap(v: samurai.variant.Variant)      = Samurai(v)
  def wrap(v: togyzkumalak.variant.Variant) = Togyzkumalak(v)
  def wrap(v: go.variant.Variant)           = Go(v)
  def wrap(v: backgammon.variant.Variant)   = Backgammon(v)
  def wrap(v: abalone.variant.Variant)      = Abalone(v)

}
