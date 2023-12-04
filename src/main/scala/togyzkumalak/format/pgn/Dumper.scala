package strategygames.togyzkumalak
package format.pgn

import scala.annotation.nowarn

object Dumper {

  def apply(
      @nowarn situation: Situation,
      data: strategygames.togyzkumalak.Move,
      @nowarn next: Situation
  ): String =
    data.toUci.uci

  def apply(data: strategygames.togyzkumalak.Move): String =
    apply(
      data.situationBefore,
      data,
      data.finalizeAfter situationOf !data.player
    )

}
