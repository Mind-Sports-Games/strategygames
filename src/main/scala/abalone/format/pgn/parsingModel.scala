package strategygames.abalone
package format.pgn

import cats.data.Validated
import strategygames.format.pgn.{Metas, San, Suffixes}
import strategygames.{Move => StratMove}

import scala.annotation.nowarn

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
  ) = move(situation.toAbalone).map(StratMove.wrap)

  override def withSuffixes(s: Suffixes) =
    copy(
      metas = metas withSuffixes s,
      promotion = s.promotion.map(_.toAbalone)
    )

  def withMetas(m: Metas) = copy(metas = m)

  def move(@nowarn sit: Situation): Validated[String, strategygames.abalone.Move] =
    Validated.invalid("Not implemented move") // TODO: ???
}
