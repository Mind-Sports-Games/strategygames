package strategygames.samurai
package format.pgn

object Dumper {

  def apply(situation: Situation, data: strategygames.samurai.Move, next: Situation): String = data.toUci.uci

  def apply(data: strategygames.samurai.Move): String =
    apply(
      data.situationBefore,
      data,
      data.finalizeAfter situationOf !data.player
    )

}
