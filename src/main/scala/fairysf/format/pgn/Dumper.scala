package strategygames.fairysf
package format.pgn
import scala.annotation.nowarn

object Dumper {

  def apply(@nowarn situation: Situation, data: strategygames.fairysf.Move, @nowarn next: Situation): String =
    data.toUci.lilaUci

  def apply(data: strategygames.fairysf.Drop, @nowarn next: Situation): String = data.toUci.lilaUci

  def apply(data: strategygames.fairysf.Move): String =
    apply(
      data.situationBefore,
      data,
      data.finalizeAfter situationOf !data.player
    )

  def apply(data: strategygames.fairysf.Drop): String =
    apply(
      data,
      data.finalizeAfter situationOf !data.player
    )
}
