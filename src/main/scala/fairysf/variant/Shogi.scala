package strategygames.fairysf
package variant

import cats.implicits._
import cats.syntax.option._
import cats.data.Validated

import strategygames.fairysf._
import strategygames.fairysf.format.Uci
import strategygames.{ Color, GameFamily }

case object Shogi
    extends Variant(
      id = 1,
      key = "shogi",
      name = "Shogi",
      shortName = "Shogi",
      title = "Shogi (Japanese Chess)",
      standardInitialPosition = true
      //boardSize = Board.D100
    ) {
  import Variant._

  override def gameFamily: GameFamily = GameFamily.Shogi()

  override def dropsVariant = true

  def perfIcon: Char = 'K'
  def perfId: Int = 200

  override def baseVariant: Boolean = true

  val pieces: Map[Pos, Piece] = Variant.symmetricRank(backRank)
  //val initialFen       = format.Forsyth.initial

  private def canDropPawnOn(pos: Pos) = pos.rank != Rank.First && pos.rank != Rank.Eighth

//  override def drop(situation: Situation, role: Role, pos: Pos): Validated[String, Drop] =
//    for {
//      d1 <- situation.board.crazyData toValid "Board has no crazyhouse data"
//      _ <-
//        if (role != Pawn || canDropPawnOn(pos)) Validated.valid(d1)
//        else Validated.invalid(s"Can't drop $role on $pos")
//      piece = Piece(situation.color, role)
//      d2     <- d1.drop(piece) toValid s"No $piece to drop on $pos"
//      board1 <- situation.board.place(piece, pos) toValid s"Can't drop $role on $pos, it's occupied"
//      _ <-
//        if (!board1.check(situation.color)) Validated.valid(board1)
//        else Validated.invalid(s"Dropping $role on $pos doesn't uncheck the king")
//    } yield Drop(
//      piece = piece,
//      pos = pos,
//      situationBefore = situation,
//      after = board1 withCrazyData d2
//    )

  override def finalizeBoard(board: Board, uci: Uci, capture: Option[Piece]): Board =
    uci match {
      case Uci.Move(orig, dest, promOption) =>
        board.crazyData.fold(board) { data =>
          val d1 = capture.fold(data) { data.store(_, dest) }
          val d2 = promOption.fold(d1.move(orig, dest)) { _ =>
            d1 promote dest
          }
          board withCrazyData d2
        }
      case _ => board
    }

  private def canDropStuff(situation: Situation) =
    situation.board.crazyData.fold(false) { (data: PocketData) =>
      val roles = data.pockets(situation.color).roles
      roles.nonEmpty && possibleDrops(situation).fold(true) { squares =>
        squares.nonEmpty && {
          squares.exists(canDropPawnOn) || roles.exists(strategygames.chess.Pawn !=)
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
    def attacker(piece: Piece) = piece.role.projection && piece.color != situation.color
    @scala.annotation.tailrec
    def forward(p: Pos, dir: Direction, squares: List[Pos]): List[Pos] =
      dir(p) match {
        case None                                                 => Nil
        case Some(next) if situation.board(next).exists(attacker) => next :: squares
        case Some(next) if situation.board(next).isDefined        => Nil
        case Some(next)                                           => forward(next, dir, next :: squares)
      }
    Queen.dirs flatMap { forward(kingPos, _, Nil) } filter { square =>
      situation.board.place(Piece(situation.color, Knight), square) exists { defended =>
        !defended.check(situation.color)
      }
    }
  }

}
