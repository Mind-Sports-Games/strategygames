package strategygames.go
package format.pgn

object Dumper {

  def apply(situation: Situation, data: strategygames.go.Drop, next: Situation): String = data.toUci.uci

  def apply(data: strategygames.go.Drop): String =
    apply(
      data.situationBefore,
      data,
      data.finalizeAfter situationOf !data.player
    )

}
