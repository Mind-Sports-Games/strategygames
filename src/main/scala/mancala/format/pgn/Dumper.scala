package strategygames.mancala
package format.pgn

object Dumper {

  def apply(situation: Situation, data: strategygames.mancala.Move, next: Situation): String =  data.toUci.uci

  def apply(data: strategygames.mancala.Move): String =
    apply(
      data.situationBefore,
      data,
      data.finalizeAfter situationOf !data.player
    )

}
