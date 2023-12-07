package strategygames

final class Hash(size: Int) {

  def apply(lib: GameLogic, situation: Situation): PositionHash = {
    val l = Hash.get(lib, situation, Hash.polyglotTable(lib))
    if (size <= 8) {
      Array.tabulate(size)(i => (l >>> ((7 - i) * 8)).toByte)
    } else {
      val m = Hash.get(lib, situation, Hash.randomTable(lib))
      Array.tabulate(size)(i =>
        if (i < 8) (l >>> ((7 - i) * 8)).toByte
        else (m >>> ((15 - i) * 8)).toByte
      )
    }
  }
}

object Hash {

  val size = 3

  sealed abstract class ZobristConstants {
    def hexToLong(s: String): Long
    val p1TurnMask: Long
    val actorMasks: Array[Long]
  }

  final case class ChessZobristConstants(zc: chess.Hash.ZobristConstants) extends ZobristConstants {
    def hexToLong(s: String): Long = zc.hexToLong(s)
    val p1TurnMask: Long           = zc.p1TurnMask
    val actorMasks: Array[Long]    = zc.actorMasks
  }

  final case class DraughtsZobristConstants(zc: draughts.Hash.ZobristConstants) extends ZobristConstants {
    def hexToLong(s: String): Long = zc.hexToLong(s)
    val p1TurnMask: Long           = zc.p1TurnMask
    val actorMasks: Array[Long]    = zc.actorMasks
  }

  final case class FairySFZobristConstants(zc: fairysf.Hash.ZobristConstants) extends ZobristConstants {
    def hexToLong(s: String): Long = zc.hexToLong(s)
    val p1TurnMask: Long           = zc.p1TurnMask
    val actorMasks: Array[Long]    = zc.actorMasks
  }

  final case class SamuraiZobristConstants(zc: samurai.Hash.ZobristConstants) extends ZobristConstants {
    def hexToLong(s: String): Long = zc.hexToLong(s)
    val p1TurnMask: Long           = zc.p1TurnMask
    val actorMasks: Array[Long]    = zc.actorMasks
  }

  final case class TogyzkumalakZobristConstants(zc: togyzkumalak.Hash.ZobristConstants)
      extends ZobristConstants {
    def hexToLong(s: String): Long = zc.hexToLong(s)
    val p1TurnMask: Long           = zc.p1TurnMask
    val actorMasks: Array[Long]    = zc.actorMasks
  }

  final case class GoZobristConstants(zc: go.Hash.ZobristConstants) extends ZobristConstants {
    def hexToLong(s: String): Long = zc.hexToLong(s)
    val p1TurnMask: Long           = zc.p1TurnMask
    val actorMasks: Array[Long]    = zc.actorMasks
  }

  // The following masks are compatible with the Polyglot
  // opening book format.
  private def polyglotTable(lib: GameLogic): ZobristConstants = lib match {
    case GameLogic.Draughts()     => DraughtsZobristConstants(new draughts.Hash.ZobristConstants(0))
    case GameLogic.Chess()        => ChessZobristConstants(new chess.Hash.ZobristConstants(0))
    case GameLogic.FairySF()      => FairySFZobristConstants(new fairysf.Hash.ZobristConstants(0))
    case GameLogic.Samurai()      => SamuraiZobristConstants(new samurai.Hash.ZobristConstants(0))
    case GameLogic.Togyzkumalak() => TogyzkumalakZobristConstants(new togyzkumalak.Hash.ZobristConstants(0))
    case GameLogic.Go()           => GoZobristConstants(new go.Hash.ZobristConstants(0))
  }

  private def randomTable(lib: GameLogic): ZobristConstants = lib match {
    case GameLogic.Draughts()     => DraughtsZobristConstants(new draughts.Hash.ZobristConstants(16))
    case GameLogic.Chess()        => ChessZobristConstants(new chess.Hash.ZobristConstants(16))
    case GameLogic.FairySF()      => FairySFZobristConstants(new fairysf.Hash.ZobristConstants(16))
    case GameLogic.Samurai()      => SamuraiZobristConstants(new samurai.Hash.ZobristConstants(16))
    case GameLogic.Togyzkumalak() => TogyzkumalakZobristConstants(new togyzkumalak.Hash.ZobristConstants(16))
    case GameLogic.Go()           => GoZobristConstants(new go.Hash.ZobristConstants(16))
  }

  private def get(lib: GameLogic, situation: Situation, table: ZobristConstants): Long =
    (lib, situation, table) match {
      case (GameLogic.Draughts(), Situation.Draughts(situation), DraughtsZobristConstants(table)) =>
        draughts.Hash.get(situation, table)
      case (GameLogic.Chess(), Situation.Chess(situation), ChessZobristConstants(table))          =>
        chess.Hash.get(situation, table)
      case (GameLogic.FairySF(), Situation.FairySF(situation), FairySFZobristConstants(table))    =>
        fairysf.Hash.get(situation, table)
      case (GameLogic.Samurai(), Situation.Samurai(situation), SamuraiZobristConstants(table))    =>
        samurai.Hash.get(situation, table)
      case (
            GameLogic.Togyzkumalak(),
            Situation.Togyzkumalak(situation),
            TogyzkumalakZobristConstants(table)
          ) =>
        togyzkumalak.Hash.get(situation, table)
      case (GameLogic.Go(), Situation.Go(situation), GoZobristConstants(table))                   =>
        go.Hash.get(situation, table)
      case _                                                                                      => sys.error("Invalid lib, situation and table combination")
    }

  private val h = new Hash(size)

  def long(lib: GameLogic, situation: Situation) = Hash.get(lib, situation, Hash.polyglotTable(lib))

  def apply(lib: GameLogic, situation: Situation): PositionHash = h.apply(lib, situation)

  def debug(hashes: PositionHash) = hashes.map(_.toInt).sum.toString

}
