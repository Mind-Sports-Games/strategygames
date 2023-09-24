package strategygames.go
package format.pgn

import cats.data.Validated
import scala.annotation.nowarn

import strategygames.{ Drop => StratDrop, Pass => StratPass, SelectSquares => StratSelectSquares }
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
  ) = move(situation.toGo)

  override def withSuffixes(s: Suffixes) =
    copy(
      metas = metas withSuffixes s,
      promotion = s.promotion.map(_.toGo)
    )

  def withMetas(m: Metas) = copy(metas = m)

  def move(@nowarn situation: Situation) = Validated.invalid("Not implemented move") // TODO: ???
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
  ) = drop(situation.toGo).map(StratDrop.wrap)

  def withMetas(m: Metas) = copy(metas = m)

  def drop(situation: Situation): Validated[String, strategygames.go.Drop] =
    situation.drop(role, pos)
}

case class Pass(
    metas: Metas = Metas.empty
) extends San {

  def apply(
      situation: strategygames.Situation,
      iteratedCapts: Boolean = false,
      forbiddenUci: Option[List[String]] = None
  ) = pass(situation.toGo).map(StratPass.wrap)

  def withMetas(m: Metas) = copy(metas = m)

  def pass(situation: Situation): Validated[String, strategygames.go.Pass] =
    situation.pass()
}

case class SelectSquares(
    squares: List[Pos],
    metas: Metas = Metas.empty
) extends San {

  def apply(
      situation: strategygames.Situation,
      iteratedCapts: Boolean = false,
      forbiddenUci: Option[List[String]] = None
  ) = selectSquares(situation.toGo).map(StratSelectSquares.wrap)

  def withMetas(m: Metas) = copy(metas = m)

  def selectSquares(situation: Situation): Validated[String, strategygames.go.SelectSquares] =
    situation.selectSquares(squares)
}
