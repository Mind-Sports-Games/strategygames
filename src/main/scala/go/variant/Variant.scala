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

  // the whole of go legality, in the order that answers cheapest first: the point is empty, it is
  // not the ko point, the placement is not suicide, and it does not recreate a position this game
  // has already held. `Chain` answers the middle two together — see `capturesUnlessSuicide`.
  private def isPlayable(situation: Situation, point: Pos): Boolean =
    !situation.board.pieces.contains(point) &&
      !situation.board.ko.contains(point) &&
      Chain
        .capturesUnlessSuicide(situation.board, situation.player, point)
        .exists(captured => !recreatesAnEarlierPosition(situation, point, captured))

  /** Positional superko: no placement may recreate a position the game has already held.
    *
    * `captured.nonEmpty` is the restriction the deleted engine carried, kept unchanged, so no placement this
    * refuses or allows differs from what it did before. The reason to keep it is cost: probing a
    * non-capturing placement means a full-board hash and a scan of the whole history for every empty point of
    * every `validDrops` call.
    *
    * It is believed to lose nothing. A placement that captures nothing leaves every stone already on the
    * board where it stands and adds one, so matching an earlier position would need the game to have unwound
    * to a strict subset of that position first — and every step of an unwind is a capture, which leaves its
    * own capturing stone behind. That is an argument and not a proof; nothing here rests on it, because the
    * restriction is inherited rather than derived.
    *
    * Positional, not situational: whose turn it is does not enter the hash. `docs/go-engine.md` carries the
    * rules contract, `docs/adr/0001-pure-scala-go-engine.md` why positional was chosen.
    */
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

  // four passes in a row end the game with the stones where they lie — nobody is going to select
  // dead stones, so the position is taken as it stands. This lives on the board-transition path
  // rather than in `validPass`, which is what makes a replayed game reach the same end a played one
  // does; before this refactor only the played path knew about it.
  private def settlesByPassing(situation: Situation): Boolean =
    situation.board.consecutivePasses + 1 >= Variant.passesSettlingTheGame

  private def afterOnePly(history: History): History =
    history.copy(halfMoveClock = history.halfMoveClock + 1)

  def createSelectSquares(situation: Situation, squares: List[Pos]): SelectSquares =
    SelectSquares(squares = squares, situationBefore = situation, autoEndTurn = true)

  /** Agreeing the dead stones: lift them, and the game is over.
    *
    * The one rules implementation of a settlement, reached by every path — and it deliberately leaves
    * `history.captures` alone, which is what the played path has always done. `Replay.addSettlement` adds the
    * count on top for the two loaders that fold action strings, and for nothing else. Those two and the rest
    * disagree, on purpose; the note there says which is which and why.
    *
    * `.settled` restarts the position history at this board, so superko forgets everything before the
    * settlement. Nothing follows a settlement, so nothing can observe it — but the ordering matters: settle
    * last, or `withHistory` overwrites the restart.
    */
  def boardAfterSelectSquares(situation: Situation, squares: List[Pos]): Board =
    situation.board
      .copy(pieces = situation.board.pieces -- squares)
      .withHistory(afterOnePly(situation.history))
      .settled

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

  /** Play `situation.player`'s stone onto `pos` and hand back the board that follows.
    *
    * The only place a stone is ever placed. `Drop.after` is this and nothing else, so every drop in the suite
    * exercises it, and a rule that belongs on the placement path has nowhere else to hide.
    *
    * Assumes the placement is legal — `drop` and `validDrops` decide that, through `isPlayable`. Order
    * matters within it: the captured stones come off before the ko point and the position hash are worked
    * out, because both describe the board as it stands afterwards.
    */
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
            captures = situation.history.captures.add(situation.player, captured.size),
            halfMoveClock = situation.history.halfMoveClock + 1
          )
          .afterPosition(hashAfterPlacing(situation.board, stone, pos, captured))
      )
  }

  /** The point an immediate recapture is forbidden on, if this placement created one.
    *
    * Simple ko is the shape where the two players could take and retake the same stone forever. All three
    * conditions are needed to recognise it, and each one on its own admits a position where the recapture is
    * perfectly legal:
    *
    *   - exactly one stone captured — capture two and the recapture cannot restore the position
    *   - the placed stone stands alone — if it joined a chain, taking it back leaves that chain behind and
    *     the board has moved on
    *   - that lone stone has exactly one liberty — with two, the opponent taking it back does not return the
    *     position either; this is the condition that separates ko from snapback
    *
    * The forbidden point is the one the captured stone stood on. Superko would refuse the same recapture in a
    * game played through from the start, but not in one resumed from a FEN, whose position history begins at
    * the resumed position and remembers nothing before it. The ko coordinate travels in the FEN and is
    * enforced on its own, which is what makes a resume safe.
    */
  private def koPointAfter(placed: Board, at: Pos, captured: Set[Pos]): Option[Pos] = {
    val placedChain = Chain.at(placed, at)
    Option.when(
      captured.size == 1 && placedChain.size == 1 && Chain.liberties(placed, placedChain).size == 1
    )(captured.head)
  }

  // zobrist, so the hash after a placement is the hash before it XOR the masks of every stone that
  // changed. The `getOrElse` is the one case the history has nothing to build on — the first action
  // of a game resumed from a FEN — and pays a full-board recompute once.
  private def hashAfterPlacing(before: Board, stone: Piece, at: Pos, captured: Set[Pos]): Long =
    captured.foldLeft(
      before.history.currentPosition.getOrElse(before.positionHash) ^ Hash.mask(stone, at)
    ) { (hash, pos) =>
      hash ^ Hash.mask(before.pieces(pos), pos)
    }

  /** Chinese area scoring: your stones on the board, plus the empty points only you surround.
    *
    * Reported in tenths of a point, because that is what the FEN's score fields carry and komi is routinely a
    * half point. Komi goes to p2 and is added here rather than by the caller, so every reader of a score sees
    * the same number.
    *
    * The rule is on the variant; the memo is `Board.areaScore`. Same split as `materialImbalance`.
    */
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

  // an empty region scores for a player only if that player is the only colour touching it. A
  // region both colours touch is dame and scores for nobody, and so is a region no colour touches
  // at all — an empty board is worth nothing to either player. There is no seki handling because
  // area scoring does not need any: a shared-liberty region is dame by this rule already.
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

  // the cheap structural invariant the wrapper leans on, asked directly. It used to export the
  // board to a FEN and regex the result, which could not fail for a Board built in process — the
  // export drops off-size stones on the way out, so the round trip only ever validated a string it
  // had just sanitised. "no stone may hold more than one point" needs no check: PieceMap is keyed
  // by Pos. `strict` draws no distinction here, as in every other go variant.
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
