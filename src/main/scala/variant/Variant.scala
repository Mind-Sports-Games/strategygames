package chess
package variant

import cats.data.Validated
import cats.syntax.option._
import scala.annotation.nowarn

import chess.format.FEN

abstract class Variant private[variant](){}

// Correctness depends on singletons for each variant ID
abstract class ChessVariant private[variant] (
    val id: Int,
    val key: String,
    val name: String,
    val shortName: String,
    val title: String,
    val standardInitialPosition: Boolean,
    val boardSize: ChessBoard.BoardSize
) extends Variant {

  def pieces: Map[Pos, ChessPiece]

  def standard      = this == Standard
  def chess960      = this == Chess960
  def fromPosition  = this == FromPosition
  def kingOfTheHill = this == KingOfTheHill
  def threeCheck    = this == ThreeCheck
  def antichess     = this == Antichess
  def atomic        = this == Atomic
  def horde         = this == Horde
  def racingKings   = this == RacingKings
  def crazyhouse    = this == Crazyhouse
  def linesOfAction = this == LinesOfAction

  def exotic = !standard

  def allowsCastling = !castles.isEmpty

  protected val backRank = Vector(Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook)

  def castles: Castles = Castles.all

  def initialFen: FEN = format.Forsyth.initial
  def startColor: Color = White

  def isValidPromotion(promotion: Option[PromotableChessRole]) =
    promotion match {
      case None                                 => true
      case Some(Queen | Rook | Knight | Bishop) => true
      case _                                    => false
    }

  def validMoves(situation: ChessSituation): Map[Pos, List[ChessMove]] =
    situation.actors
      .collect {
        case actor if actor.moves.nonEmpty => actor.pos -> actor.moves
      }
      .to(Map)

  // Optimised for performance
  def pieceThreatened(board: ChessBoard, color: Color, to: Pos, filter: ChessPiece => Boolean = _ => true): Boolean = {
    board.pieces exists {
      case (pos, piece) => 
        piece match {
          case p: ChessPiece if (p.color == color && filter(p) && p.eyes(pos, to)) =>
            (!p.role.projection) || p.role.dir(pos, to).exists {
              longRangeThreatens(board, pos, _, to)
            }
          case _ => false
        }
      case _ => false
    }
  }

  def kingThreatened(board: ChessBoard, color: Color, to: Pos, filter: ChessPiece => Boolean = _ => true) =
    pieceThreatened(board, color, to, filter)

  def kingSafety(m: ChessMove, filter: ChessPiece => Boolean, kingPos: Option[Pos]): Boolean =
    ! {
      kingPos exists { kingThreatened(m.after, !m.color, _, filter) }
    }

  def kingSafety(a: ChessActor, m: ChessMove): Boolean =
    kingSafety(
      m,
      if ((a.piece is King) || a.check) (_ => true)
      else piece match {
        case p: ChessPiece => (p.role.projection)
        case _ => false
      }, 
      if (a.piece.role == King)   None else a.board kingPosOf a.color
    )

  def longRangeThreatens(board: ChessBoard, p: Pos, dir: Direction, to: Pos): Boolean =
    dir(p) exists { next =>
      next == to || (!board.pieces.contains(next) && longRangeThreatens(board, next, dir, to))
    }

  def move(
      situation: ChessSituation,
      from: Pos,
      to: Pos,
      promotion: Option[PromotableChessRole]
  ): Validated[String, Move] = {

    // Find the move in the variant specific list of valid moves
    def findMove(from: Pos, to: Pos) = situation.moves get from flatMap (_.find(_.dest == to))

    for {
      actor <- situation.board.actors get from toValid "No piece on " + from
      _ <-
        if (actor is situation.color) Validated.valid(actor)
        else Validated.invalid("Not my piece on " + from)
      m1 <- findMove(from, to) toValid "Piece on " + from + " cannot move to " + to
      m2 <- m1 withPromotion promotion toValid "Piece on " + from + " cannot promote to " + promotion
      m3 <-
        if (isValidPromotion(promotion)) Validated.valid(m2)
        else Validated.invalid("Cannot promote to " + promotion + " in this game mode")
    } yield m3
  }

  def drop(situation: ChessSituation, role: ChessRole, pos: Pos): Validated[String, Drop] =
    Validated.invalid(s"$this variant cannot drop $situation $role $pos")

  def staleMate(situation: ChessSituation): Boolean =
    !situation.check && situation.moves.isEmpty

  def checkmate(situation: ChessSituation) = situation.check && situation.moves.isEmpty

  // In most variants, the winner is the last player to have played and there is a possibility of either a traditional
  // checkmate or a variant end condition
  def winner(situation: ChessSituation): Option[Color] =
    if (situation.checkMate || specialEnd(situation)) Option(!situation.color) else None

  @nowarn def specialEnd(situation: ChessSituation) = false

  @nowarn def specialDraw(situation: ChessSituation) = false

  /** Returns the material imbalance in pawns (overridden in Antichess)
    */
  def materialImbalance(board: ChessBoard): Int =
    board.pieces.values.foldLeft(0) { case (acc, Piece(color, role)) =>
      Role.valueOf(role).fold(acc) { value =>
        acc + value * color.fold(1, -1)
      }
    }

  /** Returns true if neither player can win. The game should end immediately.
    */
  def isInsufficientMaterial(board: ChessBoard) = InsufficientMatingMaterial(board)

  /** Returns true if the other player cannot win. This is relevant when the
    * side to move times out or disconnects. Instead of losing on time,
    * the game should be drawn.
    */
  def opponentHasInsufficientMaterial(situation: ChessSituation) =
    InsufficientMatingMaterial(situation.board, !situation.color)

  // Some variants have an extra effect on the board on a move. For example, in Atomic, some
  // pieces surrounding a capture explode
  def hasMoveEffects = false

  /** Applies a variant specific effect to the move. This helps decide whether a king is endangered by a move, for
    * example
    */
  def addVariantEffect(move: ChessMove): ChessMove = move

  def fiftyMoves(history: History): Boolean = history.halfMoveClock >= 100

  def isIrreversible(move: ChessMove): Boolean =
    (move.piece is Pawn) || move.captures || move.promotes || move.castles

  /** Once a move has been decided upon from the available legal moves, the board is finalized
    */
  @nowarn def finalizeBoard(board: ChessBoard, uci: format.Uci, captured: Option[ChessPiece]): ChessBoard = board

  protected def pawnsOnPromotionRank(board: ChessBoard, color: Color) = {
    board.pieces.exists {
      case (pos, ChessPiece(c, r)) if c == color && r == Pawn && pos.rank == color.promotablePawnRank => true
      case _                                                                                     => false
    }
  }

  protected def validSide(board: ChessBoard, strict: Boolean)(color: Color) = {
    val roles = board rolesOf color
    roles.count(_ == King) == 1 &&
    (!strict || { roles.count(_ == Pawn) <= 8 && roles.lengthCompare(16) <= 0 }) &&
    !pawnsOnPromotionRank(board, color)
  }

  def valid(board: ChessBoard, strict: Boolean) = Color.all forall validSide(board, strict)

  val roles: List[ChessRole] = List(Rook, Knight, King, Bishop, King, Queen, Pawn)

  val promotableRoles: List[PromotableChessRole] = List(Queen, Rook, Bishop, Knight)

  lazy val rolesByPgn: Map[Char, ChessRole] = roles
    .map { r =>
      (r.pgn, r)
    }
    .to(Map)

  lazy val rolesPromotableByPgn: Map[Char, PromotableChessRole] =
    promotableRoles
      .map { r =>
        (r.pgn, r)
      }
      .to(Map)

  def isUnmovedPawn(color: Color, pos: Pos) = pos.rank == color.fold(Rank.Second, Rank.Seventh)

  override def toString = s"Variant($name)"

  override def equals(that: Any): Boolean = this eq that.asInstanceOf[AnyRef]

  override def hashCode: Int = id
}

object ChessVariant {

  val all = List(
    Standard,
    Crazyhouse,
    Chess960,
    FromPosition,
    KingOfTheHill,
    ThreeCheck,
    Antichess,
    Atomic,
    Horde,
    RacingKings,
    LinesOfAction
  )
  val byId = all map { v =>
    (v.id, v)
  } toMap
  val byKey = all map { v =>
    (v.key, v)
  } toMap

  val default = Standard

  def apply(id: Int): Option[ChessVariant]     = byId get id
  def apply(key: String): Option[ChessVariant] = byKey get key
  def orDefault(id: Int): ChessVariant         = apply(id) | default
  def orDefault(key: String): ChessVariant     = apply(key) | default

  def byName(name: String): Option[ChessVariant] =
    all find (_.name.toLowerCase == name.toLowerCase)

  def exists(id: Int): Boolean = byId contains id

  val openingSensibleVariants: Set[ChessVariant] = Set(
    chess.variant.Standard,
    chess.variant.Crazyhouse,
    chess.variant.ThreeCheck,
    chess.variant.KingOfTheHill
  )

  val divisionSensibleVariants: Set[ChessVariant] = Set(
    chess.variant.Standard,
    chess.variant.Chess960,
    chess.variant.ThreeCheck,
    chess.variant.KingOfTheHill,
    chess.variant.FromPosition
  )

  private[variant] def symmetricRank(rank: IndexedSeq[ChessRole]): Map[Pos, ChessPiece] =
    (for (y <- Seq(Rank.First, Rank.Second, Rank.Seventh, Rank.Eighth); x <- File.allForBoard(8)) yield {
      Pos(x, y) -> (y match {
        case Rank.First   => White - rank(x.index)
        case Rank.Second  => White.pawn
        case Rank.Seventh => Black.pawn
        case Rank.Eighth  => Black - rank(x.index)
      })
    }).toMap
}
