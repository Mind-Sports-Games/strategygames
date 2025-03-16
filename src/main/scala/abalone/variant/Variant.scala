package strategygames.abalone.variant

import cats.data.Validated
import cats.syntax.option._
import strategygames.abalone._
import strategygames.abalone.format.{FEN, Uci}
import strategygames.{GameFamily, Player}

import scala.annotation.nowarn

// Correctness depends on singletons for each variant ID
abstract class Variant private[variant](
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

  private def validMoves_line(sit: Situation): Map[Pos, List[Move]] = {
    sit.board.pieces.filter(t => isUsable(sit, t._2)).map { case (a, pa) =>
      (a, boardType.norm.getNeigh(a).map { case (vect, b) =>
        var dest = Option.empty[Pos]
        var out = false

        var c = Pos.copy(b)
        var cp = sit.board.getPiece(c)
        var hasProperty = true
        var u = 1
        var max = false
        while (hasProperty && !max && cp.isDefined) {
          hasProperty = isUsable(sit, cp.get)

          if (hasProperty) {
            u += 1
            max = maxUsable.isDefined && u > maxUsable.get
          }

          c += vect
          cp = sit.board.getPiece(c)
        }

        if (!max) {
          var p = 0
          hasProperty = true
          while (hasProperty && !max && cp.isDefined) {
            hasProperty = isPushable(sit, cp.get)

            if (hasProperty) {
              p += 1
              max = p >= u
            }

            c += vect
            cp = sit.board.getPiece(c)
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
        .map(b => Move(pa, a, b._1.get, sit, boardAfter(sit, a, b._1.get), capture = if (b._2) b._1 else None, autoEndTurn = true))
        .toList
      )
    }
  }

  private def validMoves_jump(sit: Situation): Map[Pos, List[Move]] = {
    sit.board.pieces.filter(t => isUsable(sit, t._2)).map { case (a, pa) =>
      (a, boardType.norm.getNeigh(a).flatMap { case (vect, b) =>
        var dests = List[Pos]()

        val pvect = boardType.norm.getPrev(vect)
        val nvect = boardType.norm.getNext(vect)

        var pj = canJumpTo(sit, a + pvect)
        var nj = canJumpTo(sit, a + nvect)

        if (pj || nj) {
          var c = Pos.copy(b)
          var u = 1
          var max = false
          var cp = sit.board.getPiece(c)
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
        .map(b => Move(pa, a, b, sit, boardAfter(sit, a, b), autoEndTurn = true))
        .toList
      )
    }
  }

  private def canJumpTo(sit: Situation, a: Pos): Boolean = !sit.board.isPiece(a) && boardType.isCell(a)

  // Move pieces on the board. Other bits (including score) are handled by Move.finalizeAfter()
  def boardAfter(sit: Situation, orig: Pos, dest: Pos): Board = {
    sit.board.copy(pieces = piecesAfterAction(sit.board.pieces, orig, dest))
  }

  /*
    How to move pieces based on orig, dest :
      A. Find "globalDir" direction between orig and dest (in case of a sideMove this will always be "upY" or "downX")
      B. Determine the type of move
      C. play the move
        - side move :
          Based on the <globalDir> computed in A, determine correct combination of <lineDir> and <sideDir> (the direction that is to be applied to each marble)
            allowing to land on <dest>.
            Example:
               _ A B _ _
              _ x _ x C _
             _ _ x x _ D _
            _ _ _ Y x x _ _
          Y can do a side move to A B C D (if the "x" of some of the other axis are missing)
          origToDestDir: "upRight"
          3 potentialSideMoveDirsFromDirs : "upLeft", "upRight", "right"
            A: lineDir: upLeft, sideDir: upRight
            B: lineDir: upRight, sideDir: upLeft
            C: lineDir: upRight, sideDir: right
            D: lineDir: right, sideDir: upRight

        - else:
          - push :
            - dest contains a marble
              - move dest marble to the next available square (or off the board)
              - do the line move
          - line move :
            - move from orig to dest
   */
  private def piecesAfterAction(pieces: PieceMap, orig: Pos, dest: Pos): PieceMap = {
    var res = pieces - orig

    var vector = dest - orig
    var n = boardType.norm(vector) // Assumed > 0, always the case if the move is legal
    val neighVect = boardType.norm.neighVectors.find(vect => vect * n == vector)

    if (neighVect.isEmpty) { // Jump: n > 1
      // If the move is legal, exactly one vector matches the conditions below
      n -= 1
      val vvector = boardType.norm.neighVectors
        .filter(vect => boardType.norm.dist(vector, vect * n) == 1)
        .find(vect => {
          val to = orig + vect
          boardType.isCell(to) && pieces.contains(to)
        }).get
      vector = if (boardType.norm.cross(vvector, vector) > 0) boardType.norm.getNext(vvector)
      else boardType.norm.getPrev(vvector)

      (0 to n).foreach(i => {
        val from = orig + vvector * i
        val to = from + vector

        if (i > 0) res = res - from
        res += (to -> pieces(from))
      })
    } else { // Line
      vector /= n
      n -= 1
      (0 to n).foreach(i => {
        val from = orig + vector * i
        val to = from + vector

        if (i < n || boardType.isCell(to)) res += (to -> pieces(from))
      })
    }

    res
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

  /** Once a move has been decided upon from the available legal moves, the board is finalized. */
  //  @nowarn def finalizeBoard(board: Board, uci: Uci, captured: Option[Piece]): Board = board

  /** Once a move has been decided upon amongst the available legal ones, the board is finalized. */
  @nowarn def finalizeBoard(board: Board, uci: Uci, captured: Option[Piece]): Board = board

  def valid(@nowarn board: Board, @nowarn strict: Boolean): Boolean = true

  def isIrreversible(move: Move): Boolean = move.capture.nonEmpty

  def defaultRole: Role = Role.defaultRole

  def gameFamily: GameFamily = GameFamily.Abalone()

  override def toString = s"Variant($name)"

  override def equals(that: Any): Boolean = this eq that.asInstanceOf[AnyRef]

  override def hashCode: Int = id

  protected def isUsable(sit: Situation, piece: Piece): Boolean = piece.is(sit.player)

  protected def isPushable(sit: Situation, piece: Piece): Boolean = isEjectable(sit, piece)

  protected def isEjectable(sit: Situation, piece: Piece): Boolean = piece.isNot(sit.player) && Player.all.find(p => piece.is(p)).isDefined

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

  val byId = all map { v =>
    (v.id, v)
  } toMap
  val byKey = all map { v =>
    (v.key, v)
  } toMap

  val default = Abalone

  lazy val all: List[Variant] = List(Abalone, GrandAbalone)
}