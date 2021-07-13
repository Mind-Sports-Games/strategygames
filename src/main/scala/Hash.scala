package strategygames

final class Hash(size: Int) {

  def apply(lib: GameLib, situation: Situation): PositionHash = {
    val l = Hash.get(situation, Hash.polyglotTable(lib))
    if (size <= 8) {
      Array.tabulate(size)(i => (l >>> ((7 - i) * 8)).toByte)
    } else {
      val m = Hash.get(situation, Hash.randomTable(lib))
      Array.tabulate(size)(i =>
        if (i < 8) (l >>> ((7 - i) * 8)).toByte
        else (m >>> ((15 - i) * 8)).toByte
      )
    }
  }
}

object Hash {

  val size = 3

  sealed class ZobristConstants {
    def hexToLong(s: String): Long
    val whiteTurnMask: Long
    val actorMasks: Array[Long]
  }

  final class ChessZobristConstants(zc: chess.Hash.ZobristContants) extends ZobristContants {
    def hexToLong(s: String): Long = zc.hexToLong(s)
    val whiteTurnMask: Long        = zc.whiteTurnMask
    val actorMasks: Array[Long]    = zc.actorMasks 
  }

  final class DraughtsZobristConstants(zc: draughts.Hash.ZobristContants) extends ZobristContants {
    def hexToLong(s: String): Long = zc.hexToLong(s)
    val whiteTurnMask: Long        = zc.whiteTurnMask
    val actorMasks: Array[Long]    = zc.actorMasks 
  }

  // The following masks are compatible with the Polyglot
  // opening book format.
  private def polyglotTable(lib: GameLib): ZobristConstants = lib match {
    case GameLib.Draughts() => new DraughtsZobristConstants(new draughts.ZobristConstants(0))
    case GameLib.Chess()    => new ChessZobristConstants(new chess.ZobristConstants(0))
  }

  private def randomTable(lib: GameLib): ZobristConstants = lib match {
    case GameLib.Draughts() => new DraughtsZobristConstants(new ZobristConstants(16))
    case GameLib.Chess()    => new ChessZobristConstants(new ZobristConstants(16))
  }

  private def get(lib: GameLib, situation: Situation, table: ZobristConstants): Long = (lib, situation, table) match {
    case (GameLib.Draughts(), Situation.Draughts(situation), DraughtsZobristConstants(table)) => draughts.Hash.get(situation, table)
    case (GameLib.Chess(), Situation.Chess(situation), ChessZobristConstants(table))          => chess.Hash.get(situation, table)
  }

  private val h = new Hash(size)

  def apply(situation: Situation): PositionHash = h.apply(situation)

  def debug(hashes: PositionHash) = hashes.map(_.toInt).sum.toString

}

