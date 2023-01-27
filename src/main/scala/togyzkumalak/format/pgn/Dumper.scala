package strategygames.togyzkumalak
package format.pgn

object Dumper {

  def apply(situation: Situation, data: strategygames.togyzkumalak.Move, next: Situation): String =
    data.toUci.uci

  def apply(data: strategygames.togyzkumalak.Move): String =
    apply(
      data.situationBefore,
      data,
      data.finalizeAfter situationOf !data.player
    )

}
