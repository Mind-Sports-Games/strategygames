package strategygames.entropy
package format.pgn

object Dumper {

  // entropy has no algebraic notation of its own: a game is recorded as the
  // uci action strings themselves, so dumping an action is just reading its uci.
  def apply(data: strategygames.entropy.Move): String = data.toUci.uci

  def apply(data: strategygames.entropy.Drop): String = data.toUci.uci

  def apply(data: strategygames.entropy.Pass): String = data.toUci.uci

  def apply(data: strategygames.entropy.DrawCounter): String = data.toUci.uci

}
