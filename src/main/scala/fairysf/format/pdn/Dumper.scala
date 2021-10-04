package strategygames.fairysf
package format.pdn

object Dumper {

  def apply(_situation: Situation, data: strategygames.fairysf.Move, _next: Situation): String =
    data.orig.shortKey + (if (data.captures) "x" else "-") + data.dest.shortKey

  def apply(data: strategygames.fairysf.Move): String = apply(
    data.situationBefore,
    data,
    data.afterWithLastMove() situationOf !data.color
  )

}
