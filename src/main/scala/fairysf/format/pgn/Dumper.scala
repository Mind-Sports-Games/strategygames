package strategygames.fairysf
package format.pgn

object Dumper {

  def apply(situation: Situation, data: strategygames.fairysf.Move, next: Situation): String = data.toUci.lilaUci

  def apply(data: strategygames.fairysf.Drop, next: Situation): String = data.toUci.lilaUci

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
