package strategygames.chess
package format.pgn

import cats.data.Validated
import cats.syntax.option._

import strategygames.{ Move => StratMove, Drop => StratDrop }
import strategygames.format.pgn.{ Metas, ParsedPgn, San, Sans, Suffixes, Tags }

case class Std(
    dest: Pos,
    role: Role,
    capture: Boolean = false,
    file: Option[Int] = None,
    rank: Option[Int] = None,
    promotion: Option[PromotableRole] = None,
    metas: Metas = Metas.empty
) extends San {

  def apply(
    situation: strategygames.Situation,
    iteratedCapts: Boolean = false,
    forbiddenUci: Option[List[String]] = None
  ) = move(situation.toChess).map(StratMove.wrap).map(Left.apply)

  override def withSuffixes(s: Suffixes) =
    copy(
      metas = metas withSuffixes s,
      promotion = s.promotion.map(_.toChess)
    )

  def withMetas(m: Metas) = copy(metas = m)

  def move(situation: Situation): Validated[String, strategygames.chess.Move] =
    situation.board.pieces.foldLeft(none[strategygames.chess.Move]) {
      case (None, (pos, piece))
          if piece.player == situation.player && piece.role == role && compare(
            file,
            pos.file.index + 1
          ) && compare(
            rank,
            pos.rank.index + 1
          ) && piece.eyesMovable(pos, dest) =>
        val a = Actor(piece, pos, situation.board)
        a trustedMoves false find { m =>
          m.dest == dest && a.board.variant.kingSafety(a, m)
        }
      case (m, _) => m
    } match {
      case None       => Validated invalid s"No move found: $this\n$situation"
      case Some(move) => move withPromotion promotion toValid "Wrong promotion"
    }

  private def compare[A](a: Option[A], b: A) = a.fold(true)(b ==)
}

case class Drop(
    role: Role,
    pos: Pos,
    metas: Metas = Metas.empty
) extends San {

  def apply(
    situation: strategygames.Situation,
    iteratedCapts: Boolean = false,
    forbiddenUci: Option[List[String]] = None
  ) = drop(situation.toChess).map(StratDrop.wrap).map(Right.apply)

  def withMetas(m: Metas) = copy(metas = m)

  def drop(situation: Situation): Validated[String, strategygames.chess.Drop] =
    situation.drop(role, pos)
}

case class Castle(
    side: Side,
    metas: Metas = Metas.empty
) extends San {

  def apply(
    situation: strategygames.Situation,
    iteratedCapts: Boolean = false,
    forbiddenUci: Option[List[String]] = None
  ) = move(situation.toChess).map(StratMove.wrap).map(Left.apply)

  def withMetas(m: Metas) = copy(metas = m)

  def move(situation: Situation): Validated[String, strategygames.chess.Move] =
    for {
      kingPos <- situation.board kingPosOf situation.player toValid "No king found"
      actor   <- situation.board actorAt kingPos toValid "No actor found"
      move    <- actor.castleOn(side).headOption toValid "Cannot castle / variant is " + situation.board.variant
    } yield move
}
