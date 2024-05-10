package strategygames.samurai
package format.pgn
import scala.annotation.nowarn

object Dumper {

  def apply(@nowarn situation: Situation, data: strategygames.samurai.Move, @nowarn next: Situation): String =
    data.toUci.uci

  def apply(data: strategygames.samurai.Move): String =
    apply(
      data.situationBefore,
      data,
      data.finalizeAfter situationOf !data.player
    )

}
