package strategygames.backgammon
package format.pgn

object Dumper {

  // TODO consider rewriting. Can we just use one function with Action?
  // Would want to check how other gamelogics handle this
  // Also dont like determining playerAfter here in this code. Use the Action?

  def apply(situation: Situation, data: Move, next: Situation): String     = data.toUci.uci
  def apply(situation: Situation, data: Drop, next: Situation): String     = data.toUci.uci
  def apply(situation: Situation, data: Lift, next: Situation): String     = data.toUci.uci
  def apply(situation: Situation, data: DiceRoll, next: Situation): String = data.toUci.uci
  def apply(situation: Situation, data: EndTurn, next: Situation): String  = data.toUci.uci

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
