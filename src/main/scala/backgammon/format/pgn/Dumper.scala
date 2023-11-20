package strategygames.backgammon
package format.pgn

object Dumper {

  def apply(situation: Situation, data: strategygames.backgammon.Move, next: Situation): String =
    data.toUci.uci

  def apply(data: strategygames.backgammon.Move): String =
    apply(
      data.situationBefore,
      data,
      data.finalizeAfter situationOf !data.player
    )

}
