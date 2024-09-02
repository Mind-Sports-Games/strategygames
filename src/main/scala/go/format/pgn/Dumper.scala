package strategygames.go
package format.pgn

import scala.annotation.nowarn

object Dumper {

  def apply(@nowarn situation: Situation, data: strategygames.go.Drop, @nowarn next: Situation): String =
    data.toUci.uci

  def apply(@nowarn situation: Situation, data: strategygames.go.Pass, @nowarn next: Situation): String =
    data.toUci.uci

  def apply(
      @nowarn situation: Situation,
      data: strategygames.go.SelectSquares,
      @nowarn next: Situation
  ): String =
    data.toUci.uci

  def apply(data: strategygames.go.Drop): String =
    apply(
      data.situationBefore,
      data,
      data.finalizeAfter situationOf !data.player
    )

  def apply(data: strategygames.go.Pass): String =
    apply(
      data.situationBefore,
      data,
      data.finalizeAfter situationOf !data.player
    )

  def apply(data: strategygames.go.SelectSquares): String =
    apply(
      data.situationBefore,
      data,
      data.finalizeAfter situationOf !data.player
    )

}
