package strategygames.yonkoma
package format.pgn

object Dumper {

  def apply(data: strategygames.yonkoma.Move): String = data.toUci.uci

  def apply(data: strategygames.yonkoma.Pass): String = data.toUci.uci

}
