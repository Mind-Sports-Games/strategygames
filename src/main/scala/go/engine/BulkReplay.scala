package strategygames.go.engine

/** Replays a whole move sequence on one mutable scratch board and publishes a single [[GoState]] at the end.
  * The scratch never escapes, so [[GoState]]'s copy-per-move contract still holds for every state a caller
  * can reach (ADR 0001).
  *
  * `ScratchGoState` below re-implements [[GoState]]'s rules — legality, capture, chain merge, simple ko and
  * the superko probe — in place. The duplication is deliberate and the two must move in lockstep: a rules fix
  * lands on both sides, or `BulkReplayTest` and `GoBulkReplayDifferentialSpec`, which fold the same moves
  * through each and compare every field of the result, go red.
  */
object BulkReplay {

  final class IllegalMoveAt(
      val index: Int,
      val move: Int,
      val legalMoves: Array[Int],
      message: String
  ) extends IllegalArgumentException(message)

  def replay(start: GoState, moves: Array[Int]): GoState = {
    val scratch = ScratchGoState.of(start, moves.length)
    var index   = 0
    while (index < moves.length) {
      val move = moves(index)
      if (!scratch.isLegal(move))
        throw new IllegalMoveAt(
          index,
          move,
          scratch.computeLegalMoves(),
          s"Illegal go move $move at index $index for player ${scratch.turn} " +
            s"on ${scratch.size}x${scratch.size}"
        )
      scratch.play(move)
      index += 1
    }
    // NOTE: the frozen state has to carry the whole hash history the fold walked — `fromStoneOwners` seeds
    // only the final position's own hash, and a truncated history readmits superko recreations the per-ply
    // path forbids. `ScratchGoState.of` seeds the other end from the caller's history, which is also what
    // makes `replay(replay(s, a), b)` equal `replay(s, a ++ b)`.
    GoState
      .fromStoneOwners(
        scratch.size,
        scratch.stoneOwnerAt,
        scratch.playerTurn,
        scratch.capturesByBlack,
        scratch.capturesByWhite,
        scratch.simpleKoMove,
        scratch.consecutivePasses
      )
      .withReplayHistory(
        scratch.occurredPositionHashes.toImmutableSet,
        scratch.capturedMovesOnLastPlacement
      )
  }
}

final private[engine] class ScratchGoState private (
    val size: Int,
    zobristTable: Array[Long],
    board: Array[Byte],
    chainIds: Array[Int],
    nextStoneInChain: Array[Int],
    chainStoneCounts: Array[Int],
    chainPseudoLiberties: Array[Int],
    var playerTurn: Int,
    var capturesByBlack: Int,
    var capturesByWhite: Int,
    var simpleKoPoint: Int,
    var positionHash: Long,
    val occurredPositionHashes: LongPositionHashSet,
    var consecutivePasses: Int,
    var capturedMovesOnLastPlacement: List[Int]
) {

  import ScratchGoState._

  val stride: Int   = size + 2
  val passMove: Int = size * size

  def turn: String = if (playerTurn == BlackPlayer) "b" else "w"

  def simpleKoMove: Option[Int] = if (simpleKoPoint == NoPoint) None else Some(simpleKoPoint)

  def stoneOwnerAt(move: Int): Int =
    board(paddedIndexOfMove(move)) match {
      case Black => BlackPlayer
      case White => WhitePlayer
      case _     => NoOwner
    }

  def isLegal(move: Int): Boolean =
    move == passMove || (move >= 0 && move < passMove && move != simpleKoPoint && {
      val padded = paddedIndexOfMove(move)
      board(padded) == Empty && isLegalEmptyPoint(padded, stoneToPlace, enemyStone)
    })

  def play(move: Int): Unit =
    if (move == passMove) playPass()
    else playPlacement(move)

  def computeLegalMoves(): Array[Int] = {
    val friendly  = stoneToPlace
    val enemy     = enemyStone
    val koPoint   = simpleKoPoint
    val collected = new Array[Int](passMove + 1)
    var count     = 0
    var move      = 0
    var padded    = stride + 1
    var row       = 0
    while (row < size) {
      var column = 0
      while (column < size) {
        if (move != koPoint && board(padded) == Empty && isLegalEmptyPoint(padded, friendly, enemy)) {
          collected(count) = move
          count += 1
        }
        move += 1
        padded += 1
        column += 1
      }
      padded += 2
      row += 1
    }
    collected(count) = passMove
    count += 1
    java.util.Arrays.copyOf(collected, count)
  }

  private def paddedIndexOfMove(move: Int): Int   = (move / size + 1) * stride + move     % size + 1
  private def moveOfPaddedIndex(padded: Int): Int = (padded / stride - 1) * size + padded % stride - 1

  private def stoneToPlace: Byte = if (playerTurn == BlackPlayer) Black else White
  private def enemyStone: Byte   = if (playerTurn == BlackPlayer) White else Black

  private def stoneHash(padded: Int, stone: Byte): Long = zobristTable((padded << 1) | (stone - 1))

  private def chainRemovalHash(root: Int, stone: Byte): Long = {
    var removed = 0L
    var current = root
    while ({
      removed ^= stoneHash(current, stone)
      current = nextStoneInChain(current)
      current != root
    }) ()
    removed
  }

  private def isLegalEmptyPoint(placed: Int, friendly: Byte, enemy: Byte): Boolean = {
    val west  = placed - 1
    val east  = placed + 1
    val south = placed - stride
    val north = placed + stride

    val westStone  = board(west)
    val eastStone  = board(east)
    val southStone = board(south)
    val northStone = board(north)

    val neighborStoneMask =
      (1 << westStone) | (1 << eastStone) | (1 << southStone) | (1 << northStone)

    val hasEmptyNeighbor = (neighborStoneMask & (1 << Empty)) != 0
    val touchesEnemy     = (neighborStoneMask & (1 << enemy)) != 0

    def anyFriendlyChainSurvives: Boolean = {
      val friendRootWest  = if (westStone == friendly) chainIds(west) else NoChain
      val friendRootEast  = if (eastStone == friendly) chainIds(east) else NoChain
      val friendRootSouth = if (southStone == friendly) chainIds(south) else NoChain
      val friendRootNorth = if (northStone == friendly) chainIds(north) else NoChain

      def friendAdjacencies(root: Int): Int =
        (if (friendRootWest == root) 1 else 0) +
          (if (friendRootEast == root) 1 else 0) +
          (if (friendRootSouth == root) 1 else 0) +
          (if (friendRootNorth == root) 1 else 0)

      def chainSurvives(root: Int): Boolean =
        root != NoChain && chainPseudoLiberties(root) > friendAdjacencies(root)

      chainSurvives(friendRootWest) || chainSurvives(friendRootEast) ||
      chainSurvives(friendRootSouth) || chainSurvives(friendRootNorth)
    }

    if (!touchesEnemy) hasEmptyNeighbor || anyFriendlyChainSurvives
    else {
      val enemyRootWest  = if (westStone == enemy) chainIds(west) else NoChain
      val enemyRootEast  = if (eastStone == enemy) chainIds(east) else NoChain
      val enemyRootSouth = if (southStone == enemy) chainIds(south) else NoChain
      val enemyRootNorth = if (northStone == enemy) chainIds(north) else NoChain

      def enemyAdjacencies(root: Int): Int =
        (if (enemyRootWest == root) 1 else 0) +
          (if (enemyRootEast == root) 1 else 0) +
          (if (enemyRootSouth == root) 1 else 0) +
          (if (enemyRootNorth == root) 1 else 0)

      def capturesChain(root: Int): Boolean =
        root != NoChain && chainPseudoLiberties(root) <= 4 &&
          chainPseudoLiberties(root) == enemyAdjacencies(root)

      val capturesWest  = capturesChain(enemyRootWest)
      val capturesEast  = enemyRootEast != enemyRootWest && capturesChain(enemyRootEast)
      val capturesSouth =
        enemyRootSouth != enemyRootWest && enemyRootSouth != enemyRootEast && capturesChain(enemyRootSouth)
      val capturesNorth =
        enemyRootNorth != enemyRootWest && enemyRootNorth != enemyRootEast &&
          enemyRootNorth != enemyRootSouth && capturesChain(enemyRootNorth)

      if (capturesWest || capturesEast || capturesSouth || capturesNorth) {
        var predictedHash = positionHash ^ stoneHash(placed, friendly)
        if (capturesWest) predictedHash ^= chainRemovalHash(enemyRootWest, enemy)
        if (capturesEast) predictedHash ^= chainRemovalHash(enemyRootEast, enemy)
        if (capturesSouth) predictedHash ^= chainRemovalHash(enemyRootSouth, enemy)
        if (capturesNorth) predictedHash ^= chainRemovalHash(enemyRootNorth, enemy)
        !occurredPositionHashes.contains(predictedHash)
      } else hasEmptyNeighbor || anyFriendlyChainSurvives
    }
  }

  private def playPass(): Unit = {
    playerTurn = -playerTurn
    simpleKoPoint = NoPoint
    consecutivePasses += 1
    capturedMovesOnLastPlacement = Nil
  }

  private def playPlacement(move: Int): Unit = {
    val placed   = paddedIndexOfMove(move)
    val friendly = stoneToPlace
    val enemy    = enemyStone

    var newHash = positionHash ^ stoneHash(placed, friendly)

    val west  = placed - 1
    val east  = placed + 1
    val south = placed - stride
    val north = placed + stride

    board(placed) = friendly
    chainIds(placed) = placed
    nextStoneInChain(placed) = placed
    chainStoneCounts(placed) = 1
    chainPseudoLiberties(placed) = (if (board(west) == Empty) 1 else 0) +
      (if (board(east) == Empty) 1 else 0) +
      (if (board(south) == Empty) 1 else 0) +
      (if (board(north) == Empty) 1 else 0)

    def removeLibertyOfNeighborChain(neighbor: Int): Unit =
      if (board(neighbor) == Black || board(neighbor) == White)
        chainPseudoLiberties(chainIds(neighbor)) -= 1

    removeLibertyOfNeighborChain(west)
    removeLibertyOfNeighborChain(east)
    removeLibertyOfNeighborChain(south)
    removeLibertyOfNeighborChain(north)

    var capturedStones           = 0
    var capturedMoves: List[Int] = Nil

    def creditLibertiesAround(cleared: Int): Unit = {
      def creditNeighbor(neighbor: Int): Unit =
        if (board(neighbor) == Black || board(neighbor) == White)
          chainPseudoLiberties(chainIds(neighbor)) += 1
      creditNeighbor(cleared - 1)
      creditNeighbor(cleared + 1)
      creditNeighbor(cleared - stride)
      creditNeighbor(cleared + stride)
    }

    def removeCapturedChain(root: Int): Unit = {
      var clearing  = root
      while ({
        board(clearing) = Empty
        newHash ^= stoneHash(clearing, enemy)
        capturedStones += 1
        capturedMoves = moveOfPaddedIndex(clearing) :: capturedMoves
        clearing = nextStoneInChain(clearing)
        clearing != root
      }) ()
      var crediting = root
      while ({
        creditLibertiesAround(crediting)
        crediting = nextStoneInChain(crediting)
        crediting != root
      }) ()
    }

    def captureIfDead(neighbor: Int): Unit =
      if (board(neighbor) == enemy && chainPseudoLiberties(chainIds(neighbor)) == 0)
        removeCapturedChain(chainIds(neighbor))

    captureIfDead(west)
    captureIfDead(east)
    captureIfDead(south)
    captureIfDead(north)

    var root = placed

    def absorbFriendlyNeighbor(neighbor: Int): Unit =
      if (board(neighbor) == friendly) {
        val neighborRoot = chainIds(neighbor)
        if (neighborRoot != root)
          root = mergeChainRoots(
            chainIds,
            nextStoneInChain,
            chainStoneCounts,
            chainPseudoLiberties,
            root,
            neighborRoot
          )
      }

    absorbFriendlyNeighbor(west)
    absorbFriendlyNeighbor(east)
    absorbFriendlyNeighbor(south)
    absorbFriendlyNeighbor(north)

    simpleKoPoint =
      if (capturedStones == 1 && chainStoneCounts(root) == 1 && chainPseudoLiberties(root) == 1)
        capturedMoves.head
      else NoPoint

    if (friendly == Black) capturesByBlack += capturedStones
    else capturesByWhite += capturedStones
    playerTurn = -playerTurn
    positionHash = newHash
    occurredPositionHashes.add(newHash)
    consecutivePasses = 0
    capturedMovesOnLastPlacement = capturedMoves
  }
}

private[engine] object ScratchGoState {

  val BlackPlayer = 1
  val WhitePlayer = -1
  val NoOwner     = 0
  val NoPoint     = -1

  private val Empty: Byte  = 0
  private val Black: Byte  = 1
  private val White: Byte  = 2
  private val Border: Byte = 3
  private val NoChain      = -1

  def of(start: GoState, plannedMoves: Int): ScratchGoState = {
    val size         = start.size
    val stride       = size + 2
    val paddedLength = stride * stride
    val zobristTable = Zobrist.tableForSize(size)

    val board           = new Array[Byte](paddedLength)
    java.util.Arrays.fill(board, Border)
    val chainIds        = new Array[Int](paddedLength)
    java.util.Arrays.fill(chainIds, NoChain)
    val nextStone       = new Array[Int](paddedLength)
    val stoneCounts     = new Array[Int](paddedLength)
    val pseudoLiberties = new Array[Int](paddedLength)

    var hash     = 0L
    val passMove = size * size
    var move     = 0
    while (move < passMove) {
      val padded = (move / size + 1) * stride + move % size + 1
      start.stoneOwnerAt(move) match {
        case BlackPlayer =>
          board(padded) = Black
          hash ^= zobristTable(padded << 1)
        case WhitePlayer =>
          board(padded) = White
          hash ^= zobristTable((padded << 1) | 1)
        case _           =>
          board(padded) = Empty
      }
      move += 1
    }

    var padded = 0
    while (padded < paddedLength) {
      if (board(padded) == Black || board(padded) == White) {
        chainIds(padded) = padded
        nextStone(padded) = padded
        stoneCounts(padded) = 1
        pseudoLiberties(padded) = (if (board(padded - 1) == Empty) 1 else 0) +
          (if (board(padded + 1) == Empty) 1 else 0) +
          (if (board(padded - stride) == Empty) 1 else 0) +
          (if (board(padded + stride) == Empty) 1 else 0)
      }
      padded += 1
    }

    padded = 0
    while (padded < paddedLength) {
      if (board(padded) == Black || board(padded) == White) {
        connectSameColorNeighbor(board, chainIds, nextStone, stoneCounts, pseudoLiberties, padded, padded - 1)
        connectSameColorNeighbor(
          board,
          chainIds,
          nextStone,
          stoneCounts,
          pseudoLiberties,
          padded,
          padded - stride
        )
      }
      padded += 1
    }

    val occurredPositionHashes =
      new LongPositionHashSet(start.occurredPositionHashes.size + plannedMoves + 1)
    start.occurredPositionHashes.foreach(occurredPositionHashes.add)
    occurredPositionHashes.add(hash)

    new ScratchGoState(
      size,
      zobristTable,
      board,
      chainIds,
      nextStone,
      stoneCounts,
      pseudoLiberties,
      start.playerTurn,
      start.capturesByBlack,
      start.capturesByWhite,
      start.simpleKoMove.getOrElse(NoPoint),
      hash,
      occurredPositionHashes,
      start.consecutivePasses,
      start.capturedMovesOnLastPlacement
    )
  }

  private def connectSameColorNeighbor(
      board: Array[Byte],
      chainIds: Array[Int],
      nextStone: Array[Int],
      stoneCounts: Array[Int],
      pseudoLiberties: Array[Int],
      stone: Int,
      neighbor: Int
  ): Unit =
    if (board(neighbor) == board(stone) && chainIds(neighbor) != chainIds(stone)) {
      mergeChainRoots(chainIds, nextStone, stoneCounts, pseudoLiberties, chainIds(stone), chainIds(neighbor))
      ()
    }

  private def mergeChainRoots(
      chainIds: Array[Int],
      nextStone: Array[Int],
      stoneCounts: Array[Int],
      pseudoLiberties: Array[Int],
      first: Int,
      second: Int
  ): Int = {
    var bigger      = first
    var smaller     = second
    if (stoneCounts(second) > stoneCounts(first)) {
      bigger = second
      smaller = first
    }
    var relabeling  = smaller
    while ({
      chainIds(relabeling) = bigger
      relabeling = nextStone(relabeling)
      relabeling != smaller
    }) ()
    val afterBigger = nextStone(bigger)
    nextStone(bigger) = nextStone(smaller)
    nextStone(smaller) = afterBigger
    stoneCounts(bigger) += stoneCounts(smaller)
    pseudoLiberties(bigger) += pseudoLiberties(smaller)
    bigger
  }
}

/** Open-addressed `Long` set for the fold's superko history — no boxing and no node per ply — sized from the
  * planned move count so that a fold never rehashes.
  *
  * `0L` marks a free slot, and the empty board hashes to `0L` (no stones, nothing XORed in), so that one
  * value lives in `hasZero` rather than in the table.
  */
final private[engine] class LongPositionHashSet(initialCapacity: Int) {

  private var table   =
    new Array[Long](java.lang.Integer.highestOneBit(math.max(initialCapacity - 1, 15)) << 2)
  private var mask    = table.length - 1
  private var used    = 0
  private var hasZero = false

  def contains(value: Long): Boolean =
    if (value == 0L) hasZero
    else {
      var index   = slotOf(value)
      var current = table(index)
      while (current != 0L && current != value) {
        index = (index + 1) & mask
        current = table(index)
      }
      current == value
    }

  def add(value: Long): Unit =
    if (value == 0L) hasZero = true
    else {
      var index   = slotOf(value)
      var current = table(index)
      while (current != 0L && current != value) {
        index = (index + 1) & mask
        current = table(index)
      }
      if (current == 0L) {
        table(index) = value
        used += 1
        if ((used << 1) >= table.length) grow()
      }
    }

  def toImmutableSet: Set[Long] = {
    val values = Set.newBuilder[Long]
    if (hasZero) values += 0L
    var index  = 0
    while (index < table.length) {
      if (table(index) != 0L) values += table(index)
      index += 1
    }
    values.result()
  }

  private def slotOf(value: Long): Int = {
    val spread = value * 0x9e3779b97f4a7c15L
    (spread ^ (spread >>> 32)).toInt & mask
  }

  private def grow(): Unit = {
    val previous = table
    table = new Array[Long](previous.length << 1)
    mask = table.length - 1
    used = 0
    var index    = 0
    while (index < previous.length) {
      if (previous(index) != 0L) add(previous(index))
      index += 1
    }
  }
}
