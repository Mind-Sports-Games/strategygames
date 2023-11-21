package strategygames.abalone
package format.pgn

object Dumper {

  def apply(situation: Situation, data: strategygames.abalone.Move, next: Situation): String =
    data.toUci.uci

  def apply(data: strategygames.abalone.Move): String =
    apply(
      data.situationBefore,
      data,
      data.finalizeAfter situationOf !data.player
    )

}
