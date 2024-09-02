package strategygames.draughts
package format.pdn

import scala.annotation.nowarn

object Dumper {

  def apply(
      @nowarn _situation: Situation,
      data: strategygames.draughts.Move,
      @nowarn _next: Situation
  ): String =
    data.orig.shortKey + (if (data.captures) "x" else "-") + data.dest.shortKey

  def apply(data: strategygames.draughts.Move): String = apply(
    data.situationBefore,
    data,
    data.afterWithLastMove() situationOf !data.player
  )

}
