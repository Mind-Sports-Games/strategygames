package strategygames.backgammon
package format.pgn

import scala.annotation.nowarn

object Dumper {

  // TODO consider rewriting. Can we just use one function with Action?
  // Would want to check how other gamelogics handle this
  // Also dont like determining playerAfter here in this code. Use the Action?

  def apply(
      @nowarn situation: Situation,
      data: strategygames.backgammon.Move,
      @nowarn next: Situation
  ): String =
    data.toUci.uci
  def apply(
      @nowarn situation: Situation,
      data: strategygames.backgammon.Drop,
      @nowarn next: Situation
  ): String =
    data.toUci.uci
  def apply(
      @nowarn situation: Situation,
      data: strategygames.backgammon.Lift,
      @nowarn next: Situation
  ): String =
    data.toUci.uci
  def apply(
      @nowarn situation: Situation,
      data: strategygames.backgammon.DiceRoll,
      @nowarn next: Situation
  ): String =
    data.toUci.uci
  def apply(
      @nowarn situation: Situation,
      data: strategygames.backgammon.EndTurn,
      @nowarn next: Situation
  ): String =
    data.toUci.uci

  def apply(data: strategygames.backgammon.Move): String =
    apply(
      data.situationBefore,
      data,
      data.finalizeAfter.situationOf(data.player)
    )

  def apply(data: strategygames.backgammon.Drop): String =
    apply(
      data.situationBefore,
      data,
      data.finalizeAfter.situationOf(data.player)
    )

  def apply(data: strategygames.backgammon.Lift): String =
    apply(
      data.situationBefore,
      data,
      data.finalizeAfter.situationOf(data.player)
    )

  def apply(data: strategygames.backgammon.DiceRoll): String =
    apply(
      data.situationBefore,
      data,
      data.finalizeAfter.situationOf(data.player)
    )

  def apply(data: strategygames.backgammon.EndTurn): String =
    apply(
      data.situationBefore,
      data,
      data.finalizeAfter.situationOf(!data.player)
    )

}
