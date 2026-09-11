package strategygames.entropy

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

  // Entropy has no opening book, so unlike the older game logics these masks need only be
  // stable between runs, not compatible with the Polyglot format. Generating them from a
  // fixed seed keeps that stability without a thousand lines of hex literals.
  class ZobristConstants(start: Int) {
    private val random = new scala.util.Random(0x9e3779b9L + start.toLong)

    val p1TurnMask: Long = random.nextLong()

    val actorMasks: Array[Long] = Array.fill(Pos.allSize * Role.all.size * 2)(random.nextLong())

    def hexToLong(s: String): Long =
      (java.lang.Long.parseLong(s.substring(start, start + 8), 16) << 32) |
        java.lang.Long.parseLong(s.substring(start + 8, start + 16), 16)
  }

  private val polyglotTable    = new ZobristConstants(0)
  private lazy val randomTable = new ZobristConstants(16)

  private def pieceIndex(piece: Piece): Int =
    piece.role.hashInt * 2 + piece.player.fold(1, 0)

  private def actorIndex(pos: Pos, piece: Piece): Int =
    Pos.allSize * pieceIndex(piece) + pos.index

  def get(situation: Situation, table: ZobristConstants): Long = {
    val hturn = situation.player.fold(table.p1TurnMask, 0L)
    situation.board.pieces.view
      .map { case (pos, piece) => table.actorMasks(actorIndex(pos, piece)) }
      .fold(hturn)(_ ^ _)
  }

  private val h = new Hash(size)

  def apply(situation: Situation): PositionHash = h.apply(situation)
}
