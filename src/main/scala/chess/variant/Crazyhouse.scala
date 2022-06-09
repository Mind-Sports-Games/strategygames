package strategygames.chess.variant

import cats.syntax.option._
import cats.data.Validated
import strategygames.chess._
import strategygames.chess.format.Uci
import strategygames.chess.format.FEN
import strategygames.Player

case object Crazyhouse
    extends Variant(
      id = 10,
      key = "crazyhouse",
      name = "Crazyhouse",
      standardInitialPosition = true
    ) {

  def perfId: Int    = 18
  def perfIcon: Char = ''

  override def exoticChessVariant       = true
  override def blindModeVariant         = false
  override def materialImbalanceVariant = true

  override def dropsVariant = true

  def pieces = Standard.pieces

  override val initialFen = FEN("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR/ w KQkq - 0 1")

  override def valid(board: Board, strict: Boolean) = {
    val pieces = board.pieces.values
    (Player.all forall validSide(board, false)) &&
    (!strict || (pieces.count(_ is Pawn) <= 16 && pieces.sizeIs <= 32))
  }

  private def canDropPawnOn(pos: Pos) = pos.rank != Rank.First && pos.rank != Rank.Eighth

  override def drop(situation: Situation, role: Role, pos: Pos): Validated[String, Drop] =
    for {
      d1     <- situation.board.pocketData toValid "Board has no crazyhouse data"
      _      <-
        if (role != Pawn || canDropPawnOn(pos)) Validated.valid(d1)
        else Validated.invalid(s"Can't drop $role on $pos")
      piece   = Piece(situation.player, role)
      d2     <- d1.drop(piece) toValid s"No $piece to drop on $pos"
      board1 <- situation.board.place(piece, pos) toValid s"Can't drop $role on $pos, it's occupied"
      _      <-
        if (!board1.check(situation.player)) Validated.valid(board1)
        else Validated.invalid(s"Dropping $role on $pos doesn't uncheck the king")
    } yield Drop(
      piece = piece,
      pos = pos,
      situationBefore = situation,
      after = board1 withCrazyData d2
    )

  override def fiftyMoves(history: History): Boolean = false

  override def isIrreversible(move: Move): Boolean = move.castles

  override def finalizeBoard(board: Board, uci: Uci, capture: Option[Piece]): Board =
    uci match {
      case Uci.Move(orig, dest, promOption) =>
        board.pocketData.fold(board) { data =>
          val d1 = capture.fold(data) { data.store(_, dest) }
          val d2 = promOption.fold(d1.move(orig, dest)) { _ =>
            d1 promote dest
          }
          board withCrazyData d2
        }
      case _                                => board
    }

  private def canDropStuff(situation: Situation) =
    situation.board.pocketData.fold(false) { (data: PocketData) =>
      val roles = data.pockets(situation.player).roles
      roles.nonEmpty && possibleDrops(situation).fold(true) { squares =>
        squares.nonEmpty && {
          squares.exists(canDropPawnOn) || roles.exists(r => strategygames.chess.Pawn.forsyth != r.forsyth)
        }
      }
    }

  override def staleMate(situation: Situation) =
    super.staleMate(situation) && !canDropStuff(situation)

  override def checkmate(situation: Situation) =
    super.checkmate(situation) && !canDropStuff(situation)

  // there is always sufficient mating material in Crazyhouse
  override def opponentHasInsufficientMaterial(situation: Situation) = false
  override def isInsufficientMaterial(board: Board)                  = false

  def possibleDrops(situation: Situation): Option[List[Pos]] =
    if (!situation.check) None
    else situation.kingPos.map { blockades(situation, _) }

  private def blockades(situation: Situation, kingPos: Pos): List[Pos] = {
    def attacker(piece: Piece)                                         = piece.role.projection && piece.player != situation.player
    @scala.annotation.tailrec
    def forward(p: Pos, dir: Direction, squares: List[Pos]): List[Pos] =
      dir(p) match {
        case None                                                 => Nil
        case Some(next) if situation.board(next).exists(attacker) => next :: squares
        case Some(next) if situation.board(next).isDefined        => Nil
        case Some(next)                                           => forward(next, dir, next :: squares)
      }
    Queen.dirs flatMap { forward(kingPos, _, Nil) } filter { square =>
      situation.board.place(Piece(situation.player, Knight), square) exists { defended =>
        !defended.check(situation.player)
      }
    }
  }

}
