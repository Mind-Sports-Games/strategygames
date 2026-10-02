package strategygames.go.variant

import cats.data.Validated
import cats.syntax.option._
import scala.annotation.nowarn
import scalalib.extensions.*

import strategygames.go._
import strategygames.go.format.FEN
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
    situation.board.stoneGrid(point.index) == Board.emptyPoint &&
      !situation.board.ko.contains(point) &&
      Chain
        .capturesUnlessSuicide(situation.board, situation.player, point)
        .exists(captured => !recreatesAnEarlierPosition(situation, point, captured))

  private def recreatesAnEarlierPosition(
      situation: Situation,
      point: Pos,
      captured: Set[Pos]
  ): Boolean =
    situation.history.hasOccurred(
      hashAfterPlacing(situation, Piece(situation.player, defaultRole), point, captured)
    )

  def validPass(situation: Situation): Pass =
    Pass(
      situationBefore = situation,
      after =
        if (settlesByPassing(situation)) boardAfterPassingOut(situation)
        else boardAfterPass(situation),
      autoEndTurn = true
    )

  def boardAfterPass(situation: Situation): Board =
    situation.board.passed.withHistory(afterOnePly(situation.history))

  private def boardAfterPassingOut(situation: Situation): Board =
    situation.board.withHistory(afterOnePly(situation.history)).settled(!situation.player)

  private def settlesByPassing(situation: Situation): Boolean =
    situation.board.consecutivePasses + 1 >= Variant.passesSettlingTheGame

  private def afterOnePly(history: History): History =
    history.copy(halfMoveClock = history.halfMoveClock + 1)

  def createSelectSquares(situation: Situation, squares: List[Pos]): SelectSquares =
    SelectSquares(squares = squares, situationBefore = situation, autoEndTurn = true)

  // NOTE: `.settled` restarts the position history, so it has to come last.
  //
  // NOTE: a settlement records no captures, and this is the only place that decides so. Lifting stones
  // both players have agreed are dead is not a capture, and nothing displays it as one: lila shows the
  // area score from the ply a settlement becomes possible onwards, so the capture counter has already
  // been replaced by the time one lands. The loaders that fold action strings used to add
  // `lifted + 1` on top of this — the `+ 1` a placement needs and a settlement does not — while live
  // play and uci replay added nothing, so a game loaded two ways carried two totals.
  def boardAfterSelectSquares(situation: Situation, squares: List[Pos]): Board =
    situation.board
      .withPieces(situation.board.pieces -- squares)
      .withHistory(afterOnePly(situation.history))
      .settled(!situation.player)

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
    Option.when(specialEnd(situation))(situation.board.areaScore).flatMap { score =>
      Option.when(score.p1 != score.p2)(if (score.p1 > score.p2) P1 else P2)
    }

  def specialEnd(situation: Situation) = situation.board.deadStonesSelected

  def specialDraw(situation: Situation) = {
    val score = situation.board.areaScore
    score.p1 == score.p2
  }

  def boardAfter(situation: Situation, pos: Pos): Board = {
    val stone              = Piece(situation.player, defaultRole)
    val captured           = Chain.capturedBy(situation.board, situation.player, pos)
    val stonesAfterPlacing =
      situation.board.withPieces(situation.board.pieces -- captured + (pos -> stone))
    stonesAfterPlacing.stonePlaced
      .withKo(koPointAfter(situation, pos, captured))
      .withHistory(
        situation.history
          .copy(
            captures = situation.history.captures.add(situation.player, captured.size),
            halfMoveClock = situation.history.halfMoveClock + 1
          )
          .afterPosition(hashAfterPlacing(situation, stone, pos, captured))
      )
  }

  // NOTE: a game resumed from a fen has a position history that begins there, so simple ko is
  // enforced in its own right and its coordinate travels in the fen.
  private def koPointAfter(before: Situation, at: Pos, captured: Set[Pos]): Option[Pos] =
    Option.when(captured.size == 1 && surroundedByOpponent(before, at))(captured.head)

  private def surroundedByOpponent(before: Situation, at: Pos): Boolean = {
    val stones   = before.board.stoneGrid
    val opponent = Board.stoneCode(!before.player)
    before.board.variant.boardSize.neighbourIndices(at.index).forall(stones(_) == opponent)
  }

  private def hashAfterPlacing(
      before: Situation,
      stone: Piece,
      at: Pos,
      captured: Set[Pos]
  ): Long =
    captured.foldLeft(
      before.positionHash ^
        Hash.turnMask(before.player) ^ Hash.turnMask(!before.player) ^
        Hash.mask(stone, at)
    ) { (hash, pos) =>
      hash ^ Hash.mask(before.board.pieces(pos), pos)
    }

  // NOTE: this Score is in tenths of a point rather than points, because the fen writes both scores
  // that way and `strategygames.History.score` passes the number straight through to lila. Every
  // other game logic's `Score` is a plain count.
  // TODO(lila): score in points here and scale at the fen boundary, once lila reads the unit it wants.
  def areaScore(board: Board): Score = {
    val width    = board.variant.boardSize.width
    val height   = board.variant.boardSize.height
    val p1Rows   = new Array[Int](height)
    val p2Rows   = new Array[Int](height)
    var p1Stones = 0
    var p2Stones = 0
    board.pieces.foreachEntry { (pos, piece) =>
      val onBoard = pos.file.index < width && pos.rank.index < height
      if (piece.player == P1) {
        p1Stones += 1
        if (onBoard) p1Rows(pos.rank.index) |= 1 << pos.file.index
      } else {
        p2Stones += 1
        if (onBoard) p2Rows(pos.rank.index) |= 1 << pos.file.index
      }
    }
    val enclosed = enclosedAreaByPlayer(width, p1Rows, p2Rows)

    Score(
      (p1Stones + enclosed.p1) * 10,
      (p2Stones + enclosed.p2) * 10 + Math.round(board.komi * 10).toInt
    )
  }

  private def enclosedAreaByPlayer(width: Int, p1Rows: Array[Int], p2Rows: Array[Int]): Score = {
    val height     = p1Rows.length
    val wholeRow   = (1 << width) - 1
    val maxRuns    = height * ((width + 1) / 2)
    val parent     = new Array[Int](maxRuns)
    val runSize    = new Array[Int](maxRuns)
    val bordering  = new Array[Int](maxRuns)
    val runPoints  = new Array[Int](maxRuns)
    var runs       = 0
    var rowBelow   = 0
    var rowBelowTo = 0
    var rank       = 0
    while (rank < height) {
      val p1Around  = rowsTouching(p1Rows, rank)
      val p2Around  = rowsTouching(p2Rows, rank)
      var empty     = wholeRow & ~(p1Rows(rank) | p2Rows(rank))
      val rowStarts = runs
      while (empty != 0) {
        val run   = empty & ~(empty + (empty & -empty))
        empty &= ~run
        val sides = ((run << 1) | (run >>> 1)) & wholeRow
        val id    = runs
        runs += 1
        parent(id) = id
        runSize(id) = Integer.bitCount(run)
        bordering(id) = (if (((p1Rows(rank) & sides) | (p1Around & run)) != 0) Board.p1Stone else 0) |
          (if (((p2Rows(rank) & sides) | (p2Around & run)) != 0) Board.p2Stone else 0)
        runPoints(id) = run
        var below = rowBelow
        while (below < rowBelowTo) {
          if ((runPoints(below) & run) != 0) {
            val kept   = rootOf(parent, below)
            val merged = rootOf(parent, id)
            if (kept != merged) {
              parent(merged) = kept
              runSize(kept) += runSize(merged)
              bordering(kept) |= bordering(merged)
            }
          }
          below += 1
        }
      }
      rowBelow = rowStarts
      rowBelowTo = runs
      rank += 1
    }
    var p1Area     = 0
    var p2Area     = 0
    var id         = 0
    while (id < runs) {
      if (parent(id) == id) {
        if (bordering(id) == Board.p1Stone) p1Area += runSize(id)
        else if (bordering(id) == Board.p2Stone) p2Area += runSize(id)
      }
      id += 1
    }
    Score(p1Area, p2Area)
  }

  private def rowsTouching(rows: Array[Int], rank: Int): Int =
    (if (rank > 0) rows(rank - 1) else 0) | (if (rank + 1 < rows.length) rows(rank + 1) else 0)

  @annotation.tailrec
  private def rootOf(parent: Array[Int], run: Int): Int =
    if (parent(run) == run) run else rootOf(parent, parent(run))

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
    board.pieces.keys.forall(boardSize.onBoard)

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
