package strategygames.backgammon
package format.pgn

import cats.data.Validated
import scala.annotation.nowarn

import strategygames.{
  DiceRoll => StratDiceRoll,
  Drop => StratDrop,
  EndTurn => StratEndTurn,
  Lift => StratLift,
  Move => StratMove
}
import strategygames.format.pgn.{ Metas, San, Suffixes }

case class Std(
    dest: Pos,
    role: Role,
    capture: Boolean = false,
    file: Option[Int] = None,
    rank: Option[Int] = None,
    metas: Metas = Metas.empty
) extends San {

  def apply(
      situation: strategygames.Situation,
      iteratedCapts: Boolean = false,
      forbiddenUci: Option[List[String]] = None
  ) = move(situation.toBackgammon).map(StratMove.wrap)

  override def withSuffixes(s: Suffixes) =
    copy(
      metas = metas withSuffixes s
    )

  def withMetas(m: Metas) = copy(metas = m)

  @nowarn def move(situation: Situation): Validated[String, strategygames.backgammon.Move] =
    Validated.invalid("Not implemented move") // TODO: ???

  @nowarn private def compare[A](a: Option[A], b: A) = a.fold(true)(b ==)

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
  ) = drop(situation.toBackgammon).map(StratDrop.wrap)

  def withMetas(m: Metas) = copy(metas = m)

  def drop(situation: Situation): Validated[String, strategygames.backgammon.Drop] =
    situation.drop(role, pos)

}

case class Lift(
    pos: Pos,
    metas: Metas = Metas.empty
) extends San {

  def apply(
      situation: strategygames.Situation,
      iteratedCapts: Boolean = false,
      forbiddenUci: Option[List[String]] = None
  ) = lift(situation.toBackgammon).map(StratLift.wrap)

  def withMetas(m: Metas) = copy(metas = m)

  def lift(situation: Situation): Validated[String, strategygames.backgammon.Lift] =
    situation.lift(pos)

}

case class DiceRoll(
    dice: List[Int],
    metas: Metas = Metas.empty
) extends San {

  def apply(
      situation: strategygames.Situation,
      iteratedCapts: Boolean = false,
      forbiddenUci: Option[List[String]] = None
  ) = diceRoll(situation.toBackgammon).map(StratDiceRoll.wrap)

  def withMetas(m: Metas) = copy(metas = m)

  def diceRoll(situation: Situation): Validated[String, strategygames.backgammon.DiceRoll] =
    situation.diceRoll(dice)

}

case class EndTurn(
    metas: Metas = Metas.empty
) extends San {

  def apply(
      situation: strategygames.Situation,
      iteratedCapts: Boolean = false,
      forbiddenUci: Option[List[String]] = None
  ) = endTurn(situation.toBackgammon).map(StratEndTurn.wrap)

  def withMetas(m: Metas) = copy(metas = m)

  def endTurn(situation: Situation): Validated[String, strategygames.backgammon.EndTurn] =
    situation.endTurn

}
