package strategygames.fairysf
package format.pgn

import scala.annotation.nowarn
import cats.data.Validated

import strategygames.{ Drop => StratDrop, Move => StratMove }
import strategygames.format.pgn.{ Metas, San, Suffixes }

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
  ) = move(situation.toFairySF).map(StratMove.wrap)

  override def withSuffixes(s: Suffixes) =
    copy(
      metas = metas withSuffixes s,
      promotion = s.promotion.map(_.toFairySF)
    )

  def withMetas(m: Metas) = copy(metas = m)

  def move(@nowarn situation: Situation): Validated[String, strategygames.fairysf.Move] =
    Validated.invalid("Not implemented move") // TODO: ???

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
  ) = drop(situation.toFairySF).map(StratDrop.wrap)

  def withMetas(m: Metas) = copy(metas = m)

  def drop(situation: Situation): Validated[String, strategygames.fairysf.Drop] =
    situation.drop(role, pos)
}
