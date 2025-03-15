package strategygames.abalone
package format.pgn

import scala.annotation.nowarn

object Dumper {
  def apply(
             @nowarn situation: SSituation,
             data: MMove,
             @nowarn next: SSituation
           ): String =
    data.toUci.uci

  def apply(data: MMove): String =
    apply(
      data.situationBefore,
      data,
      data.finalizeAfter situationOf !data.player
    )
}