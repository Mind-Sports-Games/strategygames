package strategygames.entropy
package format.pgn

object Dumper {

  def apply(data: strategygames.entropy.Move): String = data.toUci.uci

  def apply(data: strategygames.entropy.Drop): String = data.toUci.uci

  def apply(data: strategygames.entropy.Pass): String = data.toUci.uci

  def apply(data: strategygames.entropy.DrawCounter): String = data.toUci.uci

}
