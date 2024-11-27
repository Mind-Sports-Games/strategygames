package strategygames.abalone
package format.pgn

import scala.annotation.nowarn

object Dumper {

  def apply(
      @nowarn situation: Situation,
      data: strategygames.abalone.Move,
      @nowarn next: Situation
  ): String =
    data.toUci.uci

  def apply(data: strategygames.abalone.Move): String =
    apply(
      data.situationBefore,
      data,
      data.finalizeAfter situationOf !data.player
    )

}
