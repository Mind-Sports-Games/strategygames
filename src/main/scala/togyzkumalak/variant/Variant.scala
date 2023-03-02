package strategygames.togyzkumalak.variant

import cats.data.Validated
import cats.syntax.option._
import scala.annotation.nowarn

import strategygames.togyzkumalak._
import strategygames.togyzkumalak.format.{ FEN, Forsyth, Uci }
import strategygames.{ GameFamily, Player }

case class TogyzkumalakName(val name: String)

// Correctness depends on singletons for each variant ID
abstract class Variant private[variant] (
    val id: Int,
    val key: String,
    val name: String,
    val standardInitialPosition: Boolean,
    val boardSize: Board.BoardSize
) {

  def togyzkumalak = this == Togyzkumalak

  def exotic = true

  def baseVariant: Boolean      = false
  def fenVariant: Boolean       = false
  def hasAnalysisBoard: Boolean = true
  def hasFishnet: Boolean       = false

  def p1IsBetterVariant: Boolean = false
  def blindModeVariant: Boolean  = true

  def materialImbalanceVariant: Boolean = false

  def dropsVariant: Boolean     = false
  def onlyDropsVariant: Boolean = false
  def hasGameScore: Boolean     = true

  def repetitionEnabled: Boolean = false

  def perfId: Int
  def perfIcon: Char

  def initialFen: FEN = format.FEN("9S,9S,9S,9S,9S,9S,9S,9S,9S/9S,9S,9S,9S,9S,9S,9S,9S,9S 0 0 S 1")

  def pieces: PieceMap = initialFen.pieces

  def startPlayer: Player = P1
  def plysPerTurn: Int    = 1

  val kingPiece: Option[Role] = None

  // looks like this is only to allow King to be a valid promotion piece
  // in just atomic, so can leave as true for now
  def isValidPromotion(promotion: Option[PromotableRole]): Boolean = true

  private def destFromOrig(pos: Pos, count: Int): Pos =
    (if (count == 1) Pos((pos.index + 1) % 18)
     else Pos((pos.index + count - 1)    % 18)) match {
      case Some(dest) => dest
      case None       => sys.error(s"Invalid dest from orig(${pos.index}) in togyz move(${count})")
    }

  private lazy val emptyPieceMap: PieceMap =
    Pos.all.map(pos => (pos, (Piece(pos.player, Stone), 0))).toMap

  private def pieceMapWithEmpties(pieces: PieceMap): PieceMap = emptyPieceMap.map {
    case (pos, posInfo) if pieces.get(pos).nonEmpty => (pos -> pieces(pos))
    case piece                                      => piece
  }

  private def stonesAfterMove(origStones: Int, thisStones: Int, origIndex: Int, thisIndex: Int): Int = {
    val thisDiff = if (thisIndex < origIndex) thisIndex + 18 else thisIndex;
    if (origStones == 1) {
      if (origIndex == thisIndex) 0
      else if (thisDiff - origIndex == 1) thisStones + 1
      else thisStones
    } else {
      val remainder = if ((thisDiff - origIndex) < origStones % 18) 1 else 0;
      val remaining = if (origIndex == thisIndex) 0 else thisStones
      remaining + (origStones / 18) + remainder
    }
  }

  def piecesAfterMove(pieces: PieceMap, orig: Pos, dest: Pos, oppTuzdik: Option[Pos]): PieceMap =
    pieceMapWithEmpties(pieces)
      .map {
        case (pos, posInfo) if posInfo._1.role != Tuzdik =>
          (pos, (posInfo._1, stonesAfterMove(pieces(orig)._2, posInfo._2, orig.index, pos.index)))
        case (pos, posInfo)                              => (pos, posInfo)
      }
      // now remove stones
      .map {
        case (pos, posInfo) if pos == dest && orig.player != dest.player && posInfo._2 % 2 == 0 =>
          (pos, (posInfo._1, 0))
        case (pos, posInfo) if pos == dest && orig.player != dest.player && posInfo._2 == 3 && pieces.filter {
              case (pos2, posInfo2) => posInfo2._1.role == Tuzdik && pos2.player == dest.player
            }.isEmpty && oppTuzdik != Pos.opposite(dest.index) && !dest.last =>
          (pos, (Piece(!pos.player, Tuzdik), 1))
        case (pos, posInfo) => (pos, posInfo)
      }
      .filterNot { case (pos, posInfo) => posInfo._2 == 0 }
      .toMap

  def boardAfter(situation: Situation, orig: Pos, dest: Pos): Board = {
    val boardAfter     = situation.board.copy(
      pieces = piecesAfterMove(situation.board.pieces, orig, dest, situation.oppTuzdik)
    )
    val oppCaptured    = situation.oppTuzdik
      .map(p => stonesAfterMove(situation.board.pieces(orig)._2, 0, orig.index, p.index))
      .getOrElse(0)
    val activeCaptured =
      if (boardAfter.playerStoneCount(!situation.player) == 0) situation.board.stoneCount - oppCaptured
      else (situation.board.stoneCount - boardAfter.stoneCount) - oppCaptured
    boardAfter.withHistory(
      situation.history.copy(
        lastMove = Some(Uci.Move(orig, dest)),
        score = Score(
          situation.history.score.p1 + situation.player.fold(activeCaptured, oppCaptured),
          situation.history.score.p2 + situation.player.fold(oppCaptured, activeCaptured)
        ),
        halfMoveClock = situation.board.history.halfMoveClock + situation.player.fold(0, 1)
      )
    )
  }

  def validMoves(situation: Situation): Map[Pos, List[Move]] =
    situation.board.pieces
      .filter { case (pos, posInfo) =>
        posInfo._1.player == situation.player && posInfo._1.role == Stone
      }
      .map {
        case (pos, posInfo) => {
          val dest = destFromOrig(pos, posInfo._2);
          (
            pos,
            List(
              Move(
                piece = posInfo._1,
                orig = pos,
                dest = dest,
                situationBefore = situation,
                after = boardAfter(situation, pos, dest),
                capture = None,
                promotion = None
              )
            )
          )
        }
      }
      .toMap

  def move(
      situation: Situation,
      from: Pos,
      to: Pos,
      promotion: Option[PromotableRole]
  ): Validated[String, Move] = {
    // Find the move in the variant specific list of valid moves
    situation.moves get from flatMap (_.find(m => m.dest == to && m.promotion == promotion)) toValid
      s"Not a valid move: ${from}${to} with prom: ${promotion}. Allowed moves: ${situation.moves}"
  }

  def stalemateIsDraw = false

  def winner(situation: Situation): Option[Player]

  @nowarn def specialEnd(situation: Situation) = false

  @nowarn def specialDraw(situation: Situation) = false

  def materialImbalance(board: Board): Int =
    board.pieces.values.foldLeft(0) { case (acc, (Piece(player, _), count)) =>
      acc + count * player.fold(1, -1)
    }

  // Some variants have an extra effect on the board on a move. For example, in Atomic, some
  // pieces surrounding a capture explode
  def hasMoveEffects = false

  def addVariantEffect(move: Move): Move = move

  /** Once a move has been decided upon from the available legal moves, the board is finalized
    */
  @nowarn def finalizeBoard(board: Board, uci: format.Uci, captured: Option[Piece]): Board =
    board

  // TODO: implement
  def valid(board: Board, strict: Boolean): Boolean = true

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

  lazy val all: List[Variant] = List(
    Togyzkumalak
  )
  val byId                    = all map { v =>
    (v.id, v)
  } toMap
  val byKey                   = all map { v =>
    (v.key, v)
  } toMap

  val default = Togyzkumalak

  def apply(id: Int): Option[Variant]     = byId get id
  def apply(key: String): Option[Variant] = byKey get key
  def orDefault(id: Int): Variant         = apply(id) | default
  def orDefault(key: String): Variant     = apply(key) | default

  def byName(name: String): Option[Variant] =
    all find (_.name.toLowerCase == name.toLowerCase)

  def exists(id: Int): Boolean = byId contains id

  val openingSensibleVariants: Set[Variant] = Set(strategygames.togyzkumalak.variant.Togyzkumalak)

  val divisionSensibleVariants: Set[Variant] = Set()

}
