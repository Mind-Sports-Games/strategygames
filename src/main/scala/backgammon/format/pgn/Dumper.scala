package strategygames.backgammon
package format.pgn

object Dumper {

  def apply(situation: Situation, data: strategygames.backgammon.Move, next: Situation): String     =
    data.toUci.uci
  def apply(situation: Situation, data: strategygames.backgammon.Drop, next: Situation): String     =
    data.toUci.uci
  def apply(situation: Situation, data: strategygames.backgammon.DiceRoll, next: Situation): String =
    data.toUci.uci

  def apply(data: strategygames.backgammon.Move): String =
    apply(
      data.situationBefore,
      data,
      data.finalizeAfter situationOf !data.player
    )

  def apply(data: strategygames.backgammon.Drop): String =
    apply(
      data.situationBefore,
      data,
      data.finalizeAfter situationOf !data.player
    )

  def apply(data: strategygames.backgammon.DiceRoll): String =
    apply(
      data.situationBefore,
      data,
      data.finalizeAfter situationOf !data.player
    )
}
