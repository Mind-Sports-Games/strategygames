package strategygames.dameo
package format.pdn

import scala.annotation.nowarn

object Dumper {

  def apply(
      @nowarn _situation: Situation,
      data: strategygames.dameo.Move,
      @nowarn _next: Situation
  ): String =
    data.orig.key + (if (data.captures) "x" else "-") + data.dest.key

  def apply(data: strategygames.dameo.Move): String = apply(
    data.situationBefore,
    data,
    data.after situationOf !data.player
  )

}
