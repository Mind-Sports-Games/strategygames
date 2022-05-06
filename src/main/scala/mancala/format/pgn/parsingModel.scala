package strategygames.mancala
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
  ) = move(situation.toMancala).map(StratMove.wrap).map(Left.apply)

  override def withSuffixes(s: Suffixes) =
    copy(
      metas = metas withSuffixes s,
      promotion = s.promotion.map(_.toMancala)
    )

  def withMetas(m: Metas) = copy(metas = m)

  def move(situation: Situation): Validated[String, strategygames.mancala.Move] = Validated.invalid("Not implemented move") //TODO: ???

  private def compare[A](a: Option[A], b: A) = a.fold(true)(b ==)
}