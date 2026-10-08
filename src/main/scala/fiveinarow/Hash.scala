package strategygames.fiveinarow

final class Hash(size: Int) {

  def apply(situation: Situation): PositionHash = {
    val l = Hash.get(situation, Hash.polyglotTable)
    if (size <= 8) {
      Array.tabulate(size)(i => (l >>> ((7 - i) * 8)).toByte)
    } else {
      val m = Hash.get(situation, Hash.randomTable)
      Array.tabulate(size)(i =>
        if (i < 8) (l >>> ((7 - i) * 8)).toByte
        else (m >>> ((15 - i) * 8)).toByte
      )
    }
  }
}

object Hash {

  val size = 3

  class ZobristConstants(start: Int) {
    private val random = new scala.util.Random(0x9e3779b9L + start.toLong)

    val p1TurnMask: Long = random.nextLong()

    val p1BlackMask: Long = random.nextLong()

    val actorMasks: Array[Long] = Array.fill(Pos.allSize * Role.all.size)(random.nextLong())

    val stepMasks: Array[Long] = Array.fill(OpeningStep.all.size)(random.nextLong())

    def hexToLong(s: String): Long =
      (java.lang.Long.parseLong(s.substring(start, start + 8), 16) << 32) |
        java.lang.Long.parseLong(s.substring(start + 8, start + 16), 16)
  }

  private val polyglotTable    = new ZobristConstants(0)
  private lazy val randomTable = new ZobristConstants(16)

  def get(situation: Situation, table: ZobristConstants): Long = {
    val board  = situation.board
    val hturn  = situation.player.fold(table.p1TurnMask, 0L)
    val hseat  = board.blackSeat.fold(table.p1BlackMask, 0L)
    val hstep  = table.stepMasks(OpeningStep.all.indexOf(board.openingStep))
    val hstate = hturn ^ hseat ^ hstep
    board.pieces.view
      .map { case (pos, piece) => table.actorMasks(Pos.allSize * piece.role.hashInt + pos.index) }
      .fold(hstate)(_ ^ _)
  }

  private val h = new Hash(size)

  def apply(situation: Situation): PositionHash = h.apply(situation)
}
