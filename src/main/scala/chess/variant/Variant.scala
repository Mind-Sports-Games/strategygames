package strategygames.chess.variant

import cats.data.Validated
import cats.syntax.option._
import scala.annotation.nowarn

import strategygames.chess._
import strategygames.chess.format.{ FEN, Uci }
import strategygames.{ GameFamily, Player }

// Correctness depends on singletons for each variant ID
abstract class Variant private[variant] (
    val id: Int,
    val key: String,
    val name: String,
    val standardInitialPosition: Boolean
) {

  val fishnetKey = key match {
    case "fiveCheck"  => "5check"
    case "noCastling" => "nocastle"
    case _            => key
  }

  def pieces: Map[Pos, Piece]

  def exotic = this != Standard

  // used to define chess variants medley
  def exoticChessVariant: Boolean = false

  def baseVariant: Boolean      = false
  def fenVariant: Boolean       = false
  def hasAnalysisBoard: Boolean = true
  def hasFishnet: Boolean       = true

  def p1IsBetterVariant: Boolean = false
  def blindModeVariant: Boolean  = true

  def materialImbalanceVariant: Boolean = false

  def dropsVariant: Boolean      = false
  def hasDetachedPocket: Boolean = false

  def canOfferDraw: Boolean = true

  def perfId: Int
  def perfIcon: Char

  def allowsCastling = !castles.isEmpty

  protected val backRank =
    Vector(Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook)

  def castles: Castles = Castles.all

  def initialFen: FEN     = format.Forsyth.initial
  def startPlayer: Player = P1

  def isValidPromotion(promotion: Option[PromotableRole]) =
    promotion match {
      case None                                 => true
      case Some(Queen | Rook | Knight | Bishop) => true
      case _                                    => false
    }

  def validMoves(situation: Situation): Map[Pos, List[Move]] =
    situation.actors
      .collect {
        case actor if actor.moves.nonEmpty => actor.pos -> actor.moves
      }
      .to(Map)

  // Optimised for performance
  def pieceThreatened(
      board: Board,
      player: Player,
      to: Pos,
      filter: Piece => Boolean = _ => true
  ): Boolean = {
    board.pieces exists {
      case (pos, piece) if piece.player == player && filter(piece) && piece.eyes(pos, to) =>
        (!piece.role.projection) || piece.role.dir(pos, to).exists {
          longRangeThreatens(board, pos, _, to)
        }
      case _                                                                              => false
    }
  }

  def kingThreatened(
      board: Board,
      player: Player,
      to: Pos,
      filter: Piece => Boolean = _ => true,
      @nowarn validatingCheck: Boolean = false
  ) =
    pieceThreatened(board, player, to, filter)

  def kingSafety(
      m: Move,
      filter: Piece => Boolean,
      kingPos: Option[Pos]
  ): Boolean =
    ! {
      kingPos exists { kingThreatened(m.after, m.playerAfter, _, filter) }
    }

  def kingSafety(a: Actor, m: Move): Boolean =
    kingSafety(
      m,
      if ((a.piece is King) || a.check) (_ => true) else (_.role.projection),
      if (a.piece.role == King) None else a.board kingPosOf a.player
    )

  def longRangeThreatens(
      board: Board,
      p: Pos,
      dir: Direction,
      to: Pos
  ): Boolean =
    dir(p) exists { next =>
      next == to || (!board.pieces
        .contains(next) && longRangeThreatens(board, next, dir, to))
    }

  def move(
      situation: Situation,
      from: Pos,
      to: Pos,
      promotion: Option[PromotableRole]
  ): Validated[String, Move] = {

    // Find the move in the variant specific list of valid moves
    def findMove(from: Pos, to: Pos) =
      situation.moves get from flatMap (_.find(_.dest == to))

    for {
      actor <- situation.board.actors get from toValid "No piece on " + from
      _     <-
        if (actor is situation.player) Validated.valid(actor)
        else Validated.invalid("Not my piece on " + from)
      m1    <- findMove(
                 from,
                 to
               ) toValid "Piece on " + from + " cannot move to " + to
      m2    <-
        m1 withPromotion promotion toValid "Piece on " + from + " cannot promote to " + promotion
      m3    <-
        if (isValidPromotion(promotion)) Validated.valid(m2)
        else
          Validated.invalid(
            "Cannot promote to " + promotion + " in this game mode"
          )
    } yield m3
  }

  def drop(
      situation: Situation,
      role: Role,
      pos: Pos
  ): Validated[String, Drop] =
    Validated.invalid(s"$this variant cannot drop $situation $role $pos")

  def diceRoll(situation: Situation, dice: List[Int]): Validated[String, DiceRoll] =
    Validated.invalid(s"$this variant cannot roll dice $situation $dice")

  def possibleDropsByRole(@nowarn situation: Situation): Option[Map[Role, List[Pos]]] =
    None // override in crazyhouse

  @nowarn def validDiceRolls(situation: Situation): List[DiceRoll] = List.empty

  def staleMate(situation: Situation): Boolean = !situation.check && situation.moves.isEmpty

  def checkmate(situation: Situation) =
    situation.check && situation.moves.isEmpty

  def stalemateIsDraw = true

  // In most variants, the winner is the last player to have played and there is a possibility of either a traditional
  // checkmate or a variant end condition
  def winner(situation: Situation): Option[Player] =
    if (situation.checkMate || specialEnd(situation)) Option(!situation.player)
    else None

  @nowarn def specialEnd(situation: Situation) = false

  @nowarn def specialDraw(situation: Situation) = false

  /** Returns the material imbalance in pawns (overridden in Antichess)
    */
  def materialImbalance(board: Board): Int =
    board.pieces.values.foldLeft(0) { case (acc, Piece(player, role)) =>
      Role.valueOf(role).fold(acc) { value =>
        acc + value * player.fold(1, -1)
      }
    }

  /** Returns true if neither player can win. The game should end immediately.
    */
  def isInsufficientMaterial(board: Board) = InsufficientMatingMaterial(board)

  /** Returns true if the other player cannot win. This is relevant when the side to move times out or
    * disconnects. Instead of losing on time, the game should be drawn.
    */
  def opponentHasInsufficientMaterial(situation: Situation) =
    InsufficientMatingMaterial(situation.board, !situation.player)

  def enPassantSquares(situation: Situation): List[Pos] =
    // Before potentially expensive move generation, first ensure some basic
    // conditions are met.
    situation.history.lastTurn.flatMap {
      case move: Uci.Move =>
        if (
          move.dest.yDist(move.orig) == 2 &&
          situation.board(move.dest).exists(_.is(Pawn)) &&
          List(
            move.dest.file.offset(-1),
            move.dest.file.offset(1)
          ).flatten
            .flatMap(
              situation.board(_, Rank.passablePawnRank(situation.player))
            )
            .exists(_ == Piece(situation.player, Pawn))
        )
          situation.moves.values.flatten.find(_.enpassant).map(_.dest)
        else None
      case _              => None
    }

  // Some variants have an extra effect on the board on a move. For example, in Atomic, some
  // pieces surrounding a capture explode
  def hasMoveEffects = false

  /** Applies a variant specific effect to the move. This helps decide whether a king is endangered by a move,
    * for example
    */
  def addVariantEffect(move: Move): Move = move

  def fiftyMoves(history: History): Boolean = history.halfMoveClock >= 100

  def isIrreversible(move: Move): Boolean =
    (move.piece is Pawn) || move.captures || move.promotes || move.castles

  /** Once a move has been decided upon from the available legal moves, the board is finalized
    */
  @nowarn def finalizeBoard(
      board: Board,
      uci: format.Uci,
      captured: Option[Piece]
  ): Board = board

  protected def pawnsOnPromotionRank(board: Board, player: Player) = {
    board.pieces.exists {
      case (pos, Piece(c, r))
          if c == player && r == Pawn && pos.rank == Rank.promotablePawnRank(
            player
          ) =>
        true
      case _ => false
    }
  }

  protected def validSide(board: Board, strict: Boolean)(player: Player) = {
    val roles = board rolesOf player
    roles.count(_ == King) == 1 &&
    (!strict || roles.count(_ == Pawn) <= 8) &&
    !pawnsOnPromotionRank(board, player)
  }

  def valid(board: Board, strict: Boolean) =
    Player.all forall validSide(board, strict)

  val roles = List(Rook, Knight, King, Bishop, King, Queen, Pawn)

  val promotableRoles: List[PromotableRole] = List(Queen, Rook, Bishop, Knight)

  lazy val rolesByPgn: Map[Char, Role] = roles
    .map { r =>
      (r.pgn, r)
    }
    .to(Map)

  lazy val rolesPromotableByPgn: Map[Char, PromotableRole] =
    promotableRoles
      .map { r =>
        (r.pgn, r)
      }
      .to(Map)

  def isUnmovedPawn(player: Player, pos: Pos) =
    pos.rank == player.fold(Rank.Second, Rank.Seventh)

  // override on multiaction variants
  def lastActionOfTurn(@nowarn situation: Situation): Boolean = true

  override def toString = s"Variant($name)"

  override def equals(that: Any): Boolean = this eq that.asInstanceOf[AnyRef]

  override def hashCode: Int = id

  def gameFamily: GameFamily = GameFamily.Chess()

  def pliesFromFen(fenTurnCount: Int, player: Player, currentTurnPlies: Int = 0) =
    fenTurnCount * 2 - player.fold(2, 1) + currentTurnPlies
}

object Variant {

  val all   = List(
    Standard,
    Crazyhouse,
    Chess960,
    FromPosition,
    KingOfTheHill,
    ThreeCheck,
    FiveCheck,
    Antichess,
    Atomic,
    Horde,
    RacingKings,
    NoCastling,
    Monster,
    LinesOfAction,
    ScrambledEggs
  )
  val byId  = all map { v =>
    (v.id, v)
  } toMap
  val byKey = all map { v =>
    (v.key, v)
  } toMap

  val default = Standard

  def apply(id: Int): Option[Variant]     = byId get id
  def apply(key: String): Option[Variant] = byKey get key
  def orDefault(id: Int): Variant         = apply(id) | default
  def orDefault(key: String): Variant     = apply(key) | default

  def byName(name: String): Option[Variant] =
    all find (_.name.toLowerCase == name.toLowerCase)

  def exists(id: Int): Boolean = byId contains id

  val openingSensibleVariants: Set[Variant] = Set(
    strategygames.chess.variant.Standard,
    strategygames.chess.variant.Crazyhouse,
    strategygames.chess.variant.ThreeCheck,
    strategygames.chess.variant.FiveCheck,
    strategygames.chess.variant.KingOfTheHill,
    strategygames.chess.variant.NoCastling,
    strategygames.chess.variant.LinesOfAction,
    strategygames.chess.variant.ScrambledEggs
  )

  val divisionSensibleVariants: Set[Variant] = Set(
    strategygames.chess.variant.Standard,
    strategygames.chess.variant.Chess960,
    strategygames.chess.variant.ThreeCheck,
    strategygames.chess.variant.FiveCheck,
    strategygames.chess.variant.KingOfTheHill,
    strategygames.chess.variant.FromPosition
  )

  private[variant] def symmetricRank(rank: IndexedSeq[Role]): Map[Pos, Piece] =
    (for (
      y <- Seq(Rank.First, Rank.Second, Rank.Seventh, Rank.Eighth);
      x <- File.all
    ) yield {
      Pos(x, y) -> (y match {
        case Rank.First   => Piece(P1, rank(x.index))
        case Rank.Second  => Piece(P1, Pawn)
        case Rank.Seventh => Piece(P2, Pawn)
        case _            => Piece(P2, rank(x.index))
      })
    }).toMap

}
