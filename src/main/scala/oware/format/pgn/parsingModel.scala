package strategygames.oware
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
  ) = move(situation.toOware).map(StratMove.wrap).map(Left.apply)

  override def withSuffixes(s: Suffixes) =
    copy(
      metas = metas withSuffixes s,
      promotion = s.promotion.map(_.toOware)
    )

  def withMetas(m: Metas) = copy(metas = m)

  def move(situation: Situation): Validated[String, strategygames.oware.Move] = Validated.invalid("Not implemented move") //TODO: ???

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
  ) = drop(situation.toOware).map(StratDrop.wrap).map(Right.apply)

  def withMetas(m: Metas) = copy(metas = m)

  def drop(situation: Situation): Validated[String, strategygames.oware.Drop] =
    situation.drop(role, pos)
}
