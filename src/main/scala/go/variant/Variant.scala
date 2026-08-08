package strategygames.go.variant

import cats.data.Validated
import cats.syntax.option._
import scala.annotation.nowarn
import scalalib.extensions.*

import strategygames.go._
import strategygames.go.format.{ FEN, Forsyth }
import strategygames.{ GameFamily, Player, Score }

case class GoName(val name: String)

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
  def fenVariant: Boolean         = true
  def variableInitialFen: Boolean = true

  def hasAnalysisBoard: Boolean = true
  def hasFishnet: Boolean       = false

  def p1IsBetterVariant: Boolean = false
  def blindModeVariant: Boolean  = true

  def materialImbalanceVariant: Boolean = false

  def dropsVariant: Boolean     = true
  def onlyDropsVariant: Boolean = true
  def hasGameScore: Boolean     = true
  def canOfferDraw: Boolean     = false

  def repetitionEnabled: Boolean = false

  def perfId: Int
  def perfIcon: Char

  def initialFen: FEN = format.Forsyth.initial

  def komi: Double = 7.5

  def fenFromSetupConfig(handicap: Int, komi: Int): FEN = {

    val p1Score = if (komi > 0) handicap * 10 else handicap * 10 - komi
    val p2Score = if (komi > 0) komi else 0
    val turn    = if (handicap == 0) "b" else "w"

    val board  = boardFenFromHandicap(handicap)
    val pocket = "[SSSSSSSSSSssssssssss]"
    FEN(s"${board}${pocket} ${turn} - ${p1Score} ${p2Score} 0 0 ${komi} 0 1")
  }

  def boardFenFromHandicap(@nowarn handicap: Int): String = initialFen.board

  def setupInfo(fen: FEN): Option[String] = {
    val komi     = fen.komi
    val handicap = fen.handicap.getOrElse(0)
    Some(s"Handicap (${handicap}), komi (${komi})".replace(".0", ""))
  }

  def pieces: PieceMap = initialFen.pieces

  def startPlayer: Player = P1

  // looks like this is only to allow King to be a valid promotion piece
  // in just atomic, so can leave as true for now
  def isValidPromotion(@nowarn promotion: Option[PromotableRole]): Boolean = false

  def validMoves(@nowarn situation: Situation) = None // just remove this?

  def canDrop(situation: Situation): Boolean =
    !situation.end && boardSize.validPos.exists(isPlayable(situation, _))

  def validDrops(situation: Situation): List[Drop] =
    playablePoints(situation).map(pos =>
      Drop(
        piece = Piece(situation.player, defaultRole),
        pos = pos,
        situationBefore = situation,
        autoEndTurn = true
      )
    )

  private def playablePoints(situation: Situation): List[Pos] =
    if (situation.end) List()
    else boardSize.validPos.filter(isPlayable(situation, _))

  private def isPlayable(situation: Situation, point: Pos): Boolean =
    !situation.board.pieces.contains(point) &&
      !situation.board.ko.contains(point) &&
      Chain
        .capturesUnlessSuicide(situation.board, situation.player, point)
        .exists(captured => !recreatesAnEarlierPosition(situation, point, captured))

  private def recreatesAnEarlierPosition(
      situation: Situation,
      point: Pos,
      captured: Set[Pos]
  ): Boolean =
    captured.nonEmpty && situation.history.hasOccurred(
      hashAfterPlacing(situation.board, Piece(situation.player, defaultRole), point, captured)
    )

  def validPass(situation: Situation): Pass =
    Pass(situationBefore = situation, autoEndTurn = true)

  def boardAfterPass(situation: Situation): Board =
    if (settlesByPassing(situation))
      situation.board.withHistory(afterOnePly(situation.history)).settled
    else situation.board.passed.withHistory(afterOnePly(situation.history))

  private def settlesByPassing(situation: Situation): Boolean =
    situation.board.consecutivePasses + 1 >= Variant.passesSettlingTheGame

  private def afterOnePly(history: History): History =
    history.copy(halfMoveClock = history.halfMoveClock + 1)

  def createSelectSquares(situation: Situation, squares: List[Pos]): SelectSquares =
    SelectSquares(squares = squares, situationBefore = situation, autoEndTurn = true)

  def boardAfterSelectSquares(situation: Situation, squares: List[Pos]): Board = {
    val stonesAfterLifting = situation.board.copy(pieces = situation.board.pieces -- squares)
    stonesAfterLifting
      .withHistory(afterOnePly(situation.history).copy(score = areaScore(stonesAfterLifting)))
      .settled
  }

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
    if (!dropsVariant) Validated.invalid(s"$this variant cannot drop $situation $role $pos")
    else if (role == defaultRole && !situation.end && isPlayable(situation, pos))
      Validated.valid(
        Drop(
          piece = Piece(situation.player, role),
          pos = pos,
          situationBefore = situation,
          autoEndTurn = true
        )
      )
    else Validated.invalid(s"$situation cannot perform the drop: $role on $pos")

  def pass(situation: Situation): Validated[String, Pass] =
    if (situation.end) Validated.invalid(s"$this variant cannot pass a finished $situation")
    else Validated.valid(validPass(situation))

  def selectSquares(situation: Situation, squares: List[Pos]) =
    if (situation.canSelectSquares) {
      Validated.valid(createSelectSquares(situation, squares))
    } else {
      Validated.invalid(s"$this variant cannot selectSquares $situation $squares")
    }

  def possibleDrops(situation: Situation): Option[List[Pos]] =
    if (dropsVariant && !situation.end)
      validDrops(situation).map(_.pos).some
    else None

  def possibleDropsByRole(situation: Situation): Option[Map[Role, List[Pos]]] =
    if (dropsVariant && !situation.end)
      validDrops(situation)
        .map(drop => (drop.piece.role, drop.pos))
        .groupBy(_._1)
        .map { case (k, v) => (k, v.toList.map(_._2)) }
        .some
    else None

  def stalemateIsDraw = false

  def winner(situation: Situation): Option[Player] =
    Option.when(specialEnd(situation))(areaScore(situation.board)).flatMap { score =>
      Option.when(score.p1 != score.p2)(if (score.p1 > score.p2) P1 else P2)
    }

  def specialEnd(situation: Situation) = situation.board.deadStonesSelected

  def specialDraw(situation: Situation) = {
    val score = areaScore(situation.board)
    score.p1 == score.p2
  }

  def boardAfter(situation: Situation, pos: Pos): Board = {
    val stone              = Piece(situation.player, defaultRole)
    val captured           = Chain.capturedBy(situation.board, situation.player, pos)
    val stonesAfterPlacing =
      situation.board.copy(pieces = situation.board.pieces -- captured + (pos -> stone))
    stonesAfterPlacing.stonePlaced
      .withKo(koPointAfter(stonesAfterPlacing, pos, captured))
      .withHistory(
        situation.history
          .copy(
            score = areaScore(stonesAfterPlacing),
            captures = situation.history.captures.add(situation.player, captured.size),
            halfMoveClock = situation.history.halfMoveClock + 1
          )
          .afterPosition(hashAfterPlacing(situation.board, stone, pos, captured))
      )
  }

  private def koPointAfter(placed: Board, at: Pos, captured: Set[Pos]): Option[Pos] = {
    val placedChain = Chain.at(placed, at)
    Option.when(
      captured.size == 1 && placedChain.size == 1 && Chain.liberties(placed, placedChain).size == 1
    )(captured.head)
  }

  private def hashAfterPlacing(before: Board, stone: Piece, at: Pos, captured: Set[Pos]): Long =
    captured.foldLeft(
      before.history.currentPosition.getOrElse(before.positionHash) ^ Hash.mask(stone, at)
    ) { (hash, pos) =>
      hash ^ Hash.mask(before.pieces(pos), pos)
    }

  def areaScore(board: Board): Score = {
    val enclosedArea = enclosedAreaByPlayer(board)

    def areaOf(player: Player): Int =
      board.playerPiecesOnBoardCount(player) + enclosedArea.getOrElse(player, 0)

    def fenTenthsOf(player: Player): Int = areaOf(player) * Variant.fenTenthsPerPoint

    Score(
      fenTenthsOf(P1),
      fenTenthsOf(P2) + Math.round(board.komi * Variant.fenTenthsPerPoint).toInt
    )
  }

  private def enclosedAreaByPlayer(board: Board): Map[Player, Int] =
    emptyRegionsOf(board)
      .flatMap(region => soleBorderingPlayer(board, region).map((_, region.size)))
      .groupMapReduce(_._1)(_._2)(_ + _)

  private def emptyRegionsOf(board: Board): List[Set[Pos]] = {
    val isEmpty = (pos: Pos) => !board.pieces.contains(pos)
    board.variant.boardSize.validPos
      .filter(isEmpty)
      .foldLeft((List.empty[Set[Pos]], Set.empty[Pos])) { case ((regions, alreadyInARegion), point) =>
        if (alreadyInARegion(point)) (regions, alreadyInARegion)
        else {
          val region = Chain.regionFrom(board, point)(isEmpty)
          (region :: regions, alreadyInARegion ++ region)
        }
      }
      ._1
  }

  private def soleBorderingPlayer(board: Board, region: Set[Pos]): Option[Player] = {
    val bordering = region.flatMap(borderingPlayersAt(board, _))
    Option.when(bordering.size == 1)(bordering.head)
  }

  private def borderingPlayersAt(board: Board, point: Pos): List[Player] =
    board.variant.boardSize.neighbours(point.index).flatMap(board.pieces.get).map(_.player)

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

  def valid(board: Board, @nowarn strict: Boolean): Boolean =
    Api.validateFEN(board.variant, Forsyth.exportBoard(board))

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

  private val fenTenthsPerPoint = 10

  private val passesSettlingTheGame = 4

  lazy val all: List[Variant] = List(
    Go19x19,
    Go13x13,
    Go9x9
  )
  val byId                    = all map { v =>
    (v.id, v)
  } toMap
  val byKey                   = all map { v =>
    (v.key, v)
  } toMap

  val default = Go19x19

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
