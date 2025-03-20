package strategygames.abalone.variant

import cats.data.Validated
import cats.syntax.option._
import strategygames.abalone._
import strategygames.abalone.format.FEN
import strategygames.{GameFamily, Player}

import scala.annotation.nowarn

// Correctness depends on singletons for each variant ID
abstract class Variant private[variant] (
    val id: Int,
    val key: String,
    val name: String,
    val standardInitialPosition: Boolean,
    val boardType: BoardType
) {
  def exotic = true

  def baseVariant: Boolean = false

  def fenVariant: Boolean = false

  def variableInitialFen: Boolean = false

  def hasAnalysisBoard: Boolean = true

  def hasFishnet: Boolean = false

  def p1IsBetterVariant: Boolean = true

  def blindModeVariant: Boolean = true

  def materialImbalanceVariant: Boolean = false

  def dropsVariant: Boolean = false

  def onlyDropsVariant: Boolean = false

  def hasGameScore: Boolean = true

  def canOfferDraw: Boolean = true

  def repetitionEnabled: Boolean = true

  def perfId: Int

  def perfIcon: Char

  def recalcStartPlayerForStats: Boolean = false

  // pieces, scoreP1, scoreP2, turn, halfMovesSinceLastCapture (triggering condition could be when == 100 && total moves > 50 ? => draw), total moves
  def initialFen: FEN

  def pieces: PieceMap = initialFen.pieces(boardType)

  def startPlayer: Player = P1

  def validMoves(sit: Situation): Map[Pos, List[Move]] =
    (validMoves_line(sit).toList ++ validMoves_jump(sit).toList)
      .groupBy(_._1)
      .map { case (k, v) => k -> v.map(_._2).flatten }

  def validMoves_line(sit: Situation): Map[Pos, List[Move]] = {
    sit.board.pieces.filter(t => isUsable(sit, t._2)).map { case (a, _) =>
      (
        a,
        boardType.norm
          .getNeigh(a)
          .map { case (vect, b) =>
            var dest = Option.empty[Pos]
            var out  = false

            var c           = Pos.copy(b)
            var cp          = sit.board.getPiece(c)
            var hasProperty = true
            var u           = 1
            var max         = false
            while (hasProperty && !max && cp.isDefined) {
              hasProperty = isUsable(sit, cp.get)

              if (hasProperty) {
                u += 1
                max = maxUsable.isDefined && u > maxUsable.get

                c += vect
                cp = sit.board.getPiece(c)
              }
            }

            if (!max) {
              var p = 0
              hasProperty = true
              while (hasProperty && !max && cp.isDefined) {
                hasProperty = isPushable(sit, cp.get)

                if (hasProperty) {
                  p += 1
                  max = p >= u

                  c += vect
                  cp = sit.board.getPiece(c)
                }
              }

              if (!max && cp.isEmpty) { // is cp.isDefined, there is an immovable piece that blocks the line
                out = !boardType.isCell(c)

                if (out) {
                  c -= vect
                  if (isEjectable(sit, sit.board.getPiece(c).get)) dest = Option(c)
                } else {
                  dest = Option(c)
                }
              }
            }

            (dest, out)
          }
          .filter(_._1.isDefined)
          .map(b => computeMove(a, b._1.get, sit, capture = if (b._2) b._1 else None))
          .toList
      )
    }
  }

  def validMoves_jump(sit: Situation): Map[Pos, List[Move]] = {
    sit.board.pieces.filter(t => isUsable(sit, t._2)).map { case (a, _) =>
      (
        a,
        boardType.norm
          .getNeigh(a)
          .flatMap { case (vect, b) =>
            var dests = List[Pos]()

            val pvect = boardType.norm.getPrev(vect)
            val nvect = boardType.norm.getNext(vect)

            var pj = canJumpTo(sit, a + pvect)
            var nj = canJumpTo(sit, a + nvect)

            if (pj || nj) {
              var c   = Pos.copy(b)
              var u   = 1
              var max = false
              var cp  = sit.board.getPiece(c)
              while (!max && (pj || nj) && cp.isDefined) {
                if (isUsable(sit, cp.get)) {
                  u += 1 // When u = 1, the only possible moves are already accounted for as in-line
                  max = maxUsable.isDefined && u > maxUsable.get

                  if (!max) {
                    if (pj) {
                      val d = c + pvect

                      if (canJumpTo(sit, d)) dests :+= d
                      else pj = false
                    }
                    if (nj) {
                      val d = c + nvect

                      if (canJumpTo(sit, d)) dests :+= d
                      else nj = false
                    }

                    c += vect
                    cp = sit.board.getPiece(c)
                  }
                } else {
                  max = true
                }
              }
            }

            dests
          }
          .map(b => computeMove(a, b, sit))
          .toList
      )
    }
  }

  def computeMove(orig: Pos, dest: Pos, sit: Situation, capture: Option[Pos] = Option.empty): Move =
    Move(
      sit.player,
      orig,
      dest,
      sit,
      boardAfter(sit, orig, dest),
      capture = capture,
      autoEndTurn = isAutoEndTurn(orig, dest, sit, capture)
    )

  def isAutoEndTurn(orig: Pos, dest: Pos, sit: Situation, capture: Option[Pos]): Boolean = true

  private def canJumpTo(sit: Situation, a: Pos): Boolean = !sit.board.isPiece(a) && boardType.isCell(a)

  // Move pieces on the board. Other bits (including score) are handled by Move.finalizeAfter()
  def boardAfter(sit: Situation, orig: Pos, dest: Pos): Board = {
    sit.board.copy(pieces = boardAfter_pieces(sit.board.pieces, orig, dest))
  }

  protected def boardAfter_pieces(pieces: PieceMap, orig: Pos, dest: Pos): PieceMap = {
    var res = pieces - orig

    var vector    = dest - orig
    var n         = boardType.norm(vector) // Assumed > 0, always the case if the move is legal
    val neighVect = boardType.norm.neighVectors.find(vect => vect * n == vector)

    if (neighVect.isEmpty) { // Jump: n > 1
      // If the move is legal, exactly one vector matches the conditions below
      n -= 1
      val vvector = boardType.norm.neighVectors
        .filter(vect => boardType.norm.dist(vector, vect * n) == 1)
        .find(vect => {
          val to = orig + vect
          boardType.isCell(to) && pieces.contains(to)
        })
        .get
      vector =
        if (boardType.norm.cross(vvector, vector) > 0) boardType.norm.getNext(vvector)
        else boardType.norm.getPrev(vvector)

      (0 to n).foreach(i => {
        val from = orig + vvector * i
        val to   = from + vector

        if (i > 0) res = res - from
        res += (to -> pieces(from))
      })
    } else { // Line
      vector /= n
      n -= 1
      (0 to n).foreach(i => {
        val from = orig + vector * i
        val to   = from + vector

        if (i < n || boardType.isCell(to)) res += (to -> pieces(from))
      })
    }

    res
  }

  //  /** Once a move has been decided upon amongst the available legal ones, the board is finalized. */
  //  @nowarn def finalizeBoard(board: Board, uci: Uci, captured: Option[Piece]): Board = board

  final def finalizeBoardAfter(move: Move): Board = {
    // Update position hashes last, only after updating the board.
    finalizeBoardAfter_hash(finalizeBoardAfter_core(finalizeBoardAfter_hist(move), move), move)
  }

  protected def finalizeBoardAfter_hist(move: Move): Board =
    move.after.updateHistory { h =>
      h.copy(
        lastTurn = if (move.autoEndTurn) h.currentTurn :+ move.toUci else h.lastTurn,
        currentTurn = if (move.autoEndTurn) List() else h.currentTurn :+ move.toUci,
        prevPlayer = Option(move.player),
        prevMove = Option(move),
        score = if (move.captures) h.score.add(move.player) else h.score,
        halfMoveClock = if (move.captures) 0 else h.halfMoveClock + 1
      )
    }

  @nowarn protected def finalizeBoardAfter_core(board: Board, move: Move): Board = board

  protected def finalizeBoardAfter_hash(board: Board, move: Move): Board =
    board.updateHistory { h =>
      val prevPositionHashes =
        if (isIrreversible(move)) Array.empty: PositionHash
        else if (h.positionHashes.isEmpty) Hash(move.situationBefore)
        else h.positionHashes

      h.copy(positionHashes =
        if (move.autoEndTurn) Hash(move.after.situationOf(!move.player)) ++ prevPositionHashes
        else prevPositionHashes
      )
    }

  def move(sit: Situation, from: Pos, to: Pos): Validated[String, Move] = {
    // Find the move in the variant specific list of valid moves !
    sit.moves.get(from).flatMap(_.find(m => m.dest == to)) toValid
      s"Not a valid move: $from$to. Allowed moves: ${sit.moves}"
  }

  /** If a player runs out of move, the match is a draw. */
  def stalemateIsDraw = true

  def maxUsable: Option[Int] = Option(3)

  def winningScore = 6

  def winner(sit: Situation): Option[Player] = {
    if (sit.board.history.score.p1 >= winningScore) Some(P1)
    else if (sit.board.history.score.p2 >= winningScore) Some(P2)
    else None
  }

  def specialEnd(sit: Situation) = winner(sit).isDefined

  def specialDraw(sit: Situation) = sit.moves.size == 0

  // TODO Abalone Set
  //  def materialImbalance(@nowarn board: Board): Int = 0

  def materialImbalance(@nowarn board: Board): Int = 0

  // Some variants have an extra effect on the board on a move. For example, in Atomic, some
  // pieces surrounding a capture explode
  def hasMoveEffects = false

  def addVariantEffect(move: Move): Move = move

  def valid(@nowarn board: Board, @nowarn strict: Boolean): Boolean = true

  def isIrreversible(move: Move): Boolean = move.capture.nonEmpty

  /** Indicates whether the previous player should be remembered to asses a situation. */
  def hasPrevPlayer: Boolean = false

  def turnCountFromFen(fenTurnCount: Int, player: Player) =
    fenTurnCount * 2 - player.fold(2, 1)

  def pliesFromFen(fenTurnCount: Int, player: Player, currentTurnPlies: Int = 0) =
    turnCountFromFen(fenTurnCount, player) + currentTurnPlies

  def defaultRole: Role = Role.defaultRole

  def gameFamily: GameFamily = GameFamily.Abalone()

  override def toString = s"Variant($name)"

  override def equals(that: Any): Boolean = this eq that.asInstanceOf[AnyRef]

  override def hashCode: Int = id

  protected def isUsable(sit: Situation, piece: Piece): Boolean = piece.is(sit.player)

  protected def isPushable(sit: Situation, piece: Piece): Boolean = isEjectable(sit, piece)

  protected def isEjectable(sit: Situation, piece: Piece): Boolean =
    piece.isNot(sit.player) && Player.all.find(p => piece.is(p)).isDefined

  val roles: List[Role] = Role.all

  lazy val rolesByPgn: Map[Char, Role] = roles
    .map { r =>
      (r.pgn, r)
    }
    .to(Map)
}

object Variant {
  def apply(id: Int): Option[Variant] = byId get id

  def apply(key: String): Option[Variant] = byKey get key

  def orDefault(id: Int): Variant = apply(id) | default

  def orDefault(key: String): Variant = apply(key) | default

  def byName(name: String): Option[Variant] =
    all find (_.name.toLowerCase == name.toLowerCase)

  def exists(id: Int): Boolean = byId contains id

  val openingSensibleVariants: Set[Variant] = Set(Abalone, GrandAbalone)

  val divisionSensibleVariants: Set[Variant] = Set()

  val byId  = all map { v =>
    (v.id, v)
  } toMap
  val byKey = all map { v =>
    (v.key, v)
  } toMap

  val default = Abalone

  lazy val all: List[Variant] = List(Abalone, GrandAbalone)
}
