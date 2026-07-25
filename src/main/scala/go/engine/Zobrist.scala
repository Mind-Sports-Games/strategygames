package strategygames.go.engine

/** Per board size, the random `Long` a stone contributes to a position hash, indexed by padded point and
  * colour as `(point << 1) | colourBit` (0 = black, 1 = white).
  *
  * The values come from SplitMix64 seeded by the board size rather than from an RNG, so a table is identical
  * in every JVM and every run — position hashes are comparable across processes, and a fixture that pins one
  * stays pinned.
  */
object Zobrist {

  private val GoldenGamma = 0x9e3779b97f4a7c15L

  def tableForSize(size: Int): Array[Long] = {
    require(size == 9 || size == 13 || size == 19, s"unsupported go board size $size")
    size match {
      case 9  => table9
      case 13 => table13
      case _  => table19
    }
  }

  private lazy val table9  = generateTable(9)
  private lazy val table13 = generateTable(13)
  private lazy val table19 = generateTable(19)

  private def generateTable(size: Int): Array[Long] = {
    val stride = size + 2
    val table  = new Array[Long]((stride * stride) << 1)
    var seed   = mix(size.toLong)
    var i      = 0
    while (i < table.length) {
      seed += GoldenGamma
      table(i) = mix(seed)
      i += 1
    }
    table
  }

  private def mix(value: Long): Long = {
    val spread   = (value ^ (value >>> 30)) * 0xbf58476d1ce4e5b9L
    val respread = (spread ^ (spread >>> 27)) * 0x94d049bb133111ebL
    respread ^ (respread >>> 31)
  }
}
