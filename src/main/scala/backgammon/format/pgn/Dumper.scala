package strategygames.backgammon
package format.pgn

import scala.annotation.nowarn

object Dumper {

  // TODO consider rewriting. Can we just use one function with Action?
  // Would want to check how other gamelogics handle this
  // Also dont like determining playerAfter here in this code. Use the Action?

  def apply(@nowarn situation: Situation, data: Move, @nowarn next: Situation): String     =
    data.toUci.uci
  def apply(@nowarn situation: Situation, data: Drop, @nowarn next: Situation): String     =
    data.toUci.uci
  def apply(@nowarn situation: Situation, data: Lift, @nowarn next: Situation): String     =
    data.toUci.uci
  def apply(@nowarn situation: Situation, data: DiceRoll, @nowarn next: Situation): String =
    data.toUci.uci
  def apply(@nowarn situation: Situation, data: EndTurn, @nowarn next: Situation): String  =
    data.toUci.uci

  def apply(data: Move): String =
    apply(
      data.situationBefore,
      data,
      data.finalizeAfter.situationOf(data.player)
    )

  def apply(data: Drop): String =
    apply(
      data.situationBefore,
      data,
      data.finalizeAfter.situationOf(data.player)
    )

  def apply(data: Lift): String =
    apply(
      data.situationBefore,
      data,
      data.finalizeAfter.situationOf(data.player)
    )

  def apply(data: DiceRoll): String =
    apply(
      data.situationBefore,
      data,
      data.finalizeAfter.situationOf(data.player)
    )

  def apply(data: EndTurn): String =
    apply(
      data.situationBefore,
      data,
      data.finalizeAfter.situationOf(!data.player)
    )

}
