package strategygames.fiveinarow
package format.pgn

object Dumper {

  def apply(data: strategygames.fiveinarow.Drop): String = data.toUci.uci

  def apply(data: strategygames.fiveinarow.Swap): String = data.toUci.uci

  def apply(data: strategygames.fiveinarow.Swap2): String = data.toUci.uci

}
