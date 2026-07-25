package strategygames.go.engine

final class GoState private (
    val size: Int,
    private val zobristTable: Array[Long],
    private val board: Array[Byte],
    private val chainIds: Array[Int],
    private val nextStoneInChain: Array[Int],
    private val chainStoneCounts: Array[Int],
    private val chainPseudoLiberties: Array[Int],
    val playerTurn: Int,
    val capturesByBlack: Int,
    val capturesByWhite: Int,
    val simpleKoMove: Option[Int],
    val positionHash: Long,
    val positionHashHistory: Vector[Long],
    private val occurredPositionHashes: Set[Long],
    val consecutivePasses: Int
) {

  import GoState._

  val stride: Int   = size + 2
  val passMove: Int = size * size

  def turn: String = if (playerTurn == BlackPlayer) "b" else "w"

  def inDeadStoneSelectionPhase: Boolean = consecutivePasses >= 2

  def stoneOwnerAt(move: Int): Int = {
    require(move >= 0 && move < passMove, s"point move out of range: $move")
    board(paddedIndexOfMove(move)) match {
      case Black => BlackPlayer
      case White => WhitePlayer
      case _     => NoOwner
    }
  }

  def isLegal(move: Int): Boolean =
    move == passMove || (move >= 0 && move < passMove && isLegalPlacement(paddedIndexOfMove(move)))

  def withoutStones(removedMoves: Set[Int]): GoState =
    GoState.fromStoneOwners(
      size,
      move => if (removedMoves.contains(move)) NoOwner else stoneOwnerAt(move),
      playerTurn,
      capturesByBlack,
      capturesByWhite,
      None,
      consecutivePasses
    )

  lazy val areaScore: AreaScore = {
    val paddedLength = stride * stride
    val visited      = new Array[Boolean](paddedLength)
    val frontier     = new Array[Int](passMove)
    var blackArea    = 0
    var whiteArea    = 0
    var origin       = 0
    while (origin < paddedLength) {
      board(origin) match {
        case Black                     => blackArea += 1
        case White                     => whiteArea += 1
        case Empty if !visited(origin) =>
          visited(origin) = true
          frontier(0) = origin
          var pending      = 1
          var regionSize   = 0
          var touchesBlack = false
          var touchesWhite = false
          while (pending > 0) {
            pending -= 1
            val point     = frontier(pending)
            regionSize += 1
            var direction = 0
            while (direction < 4) {
              val neighbor =
                if (direction == 0) point - 1
                else if (direction == 1) point + 1
                else if (direction == 2) point - stride
                else point + stride
              board(neighbor) match {
                case Black                       => touchesBlack = true
                case White                       => touchesWhite = true
                case Empty if !visited(neighbor) =>
                  visited(neighbor) = true
                  frontier(pending) = neighbor
                  pending += 1
                case _                           => ()
              }
              direction += 1
            }
          }
          if (touchesBlack && !touchesWhite) blackArea += regionSize
          else if (touchesWhite && !touchesBlack) whiteArea += regionSize
        case _                         => ()
      }
      origin += 1
    }
    AreaScore(blackArea, whiteArea)
  }

  def legalMoves: Array[Int] = computedLegalMoves.clone()

  def apply(move: Int): GoState =
    if (!isLegal(move))
      throw new IllegalArgumentException(s"Illegal go move $move for player $turn on ${size}x${size}")
    else if (move == passMove) afterPass
    else afterPlacement(move)

  private lazy val computedLegalMoves: Array[Int] = {
    val collected = new Array[Int](passMove + 1)
    var count     = 0
    var move      = 0
    while (move < passMove) {
      if (isLegalPlacement(paddedIndexOfMove(move))) {
        collected(count) = move
        count += 1
      }
      move += 1
    }
    collected(count) = passMove
    java.util.Arrays.copyOf(collected, count + 1)
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

  private def isLegalPlacement(placed: Int): Boolean =
    board(placed) == Empty && {
      val friendly = stoneToPlace
      val enemy    = enemyStone
      val west     = placed - 1
      val east     = placed + 1
      val south    = placed - stride
      val north    = placed + stride

      val enemyRootWest  = if (board(west) == enemy) chainIds(west) else NoChain
      val enemyRootEast  = if (board(east) == enemy) chainIds(east) else NoChain
      val enemyRootSouth = if (board(south) == enemy) chainIds(south) else NoChain
      val enemyRootNorth = if (board(north) == enemy) chainIds(north) else NoChain

      def enemyAdjacencies(root: Int): Int =
        (if (enemyRootWest == root) 1 else 0) +
          (if (enemyRootEast == root) 1 else 0) +
          (if (enemyRootSouth == root) 1 else 0) +
          (if (enemyRootNorth == root) 1 else 0)

      def capturesChain(root: Int): Boolean =
        root != NoChain && chainPseudoLiberties(root) == enemyAdjacencies(root)

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
      } else {
        val hasEmptyNeighbor =
          board(west) == Empty || board(east) == Empty || board(south) == Empty || board(north) == Empty
        hasEmptyNeighbor || {
          val friendRootWest  = if (board(west) == friendly) chainIds(west) else NoChain
          val friendRootEast  = if (board(east) == friendly) chainIds(east) else NoChain
          val friendRootSouth = if (board(south) == friendly) chainIds(south) else NoChain
          val friendRootNorth = if (board(north) == friendly) chainIds(north) else NoChain

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
      }
    }

  private def afterPass: GoState =
    new GoState(
      size,
      zobristTable,
      board,
      chainIds,
      nextStoneInChain,
      chainStoneCounts,
      chainPseudoLiberties,
      -playerTurn,
      capturesByBlack,
      capturesByWhite,
      None,
      positionHash,
      positionHashHistory,
      occurredPositionHashes,
      consecutivePasses + 1
    )

  private def afterPlacement(move: Int): GoState = {
    val placed   = paddedIndexOfMove(move)
    val friendly = stoneToPlace
    val enemy    = enemyStone

    val newBoard  = board.clone()
    val newChains = chainIds.clone()
    val newNext   = nextStoneInChain.clone()
    val newCounts = chainStoneCounts.clone()
    val newLibs   = chainPseudoLiberties.clone()

    var newHash = positionHash ^ stoneHash(placed, friendly)

    val west  = placed - 1
    val east  = placed + 1
    val south = placed - stride
    val north = placed + stride

    newBoard(placed) = friendly
    newChains(placed) = placed
    newNext(placed) = placed
    newCounts(placed) = 1
    newLibs(placed) = (if (newBoard(west) == Empty) 1 else 0) +
      (if (newBoard(east) == Empty) 1 else 0) +
      (if (newBoard(south) == Empty) 1 else 0) +
      (if (newBoard(north) == Empty) 1 else 0)

    def removeLibertyOfNeighborChain(neighbor: Int): Unit =
      if (newBoard(neighbor) == Black || newBoard(neighbor) == White)
        newLibs(newChains(neighbor)) -= 1

    removeLibertyOfNeighborChain(west)
    removeLibertyOfNeighborChain(east)
    removeLibertyOfNeighborChain(south)
    removeLibertyOfNeighborChain(north)

    var capturedStones = 0
    var lastCaptured   = NoChain

    def creditLibertiesAround(cleared: Int): Unit = {
      def creditNeighbor(neighbor: Int): Unit =
        if (newBoard(neighbor) == Black || newBoard(neighbor) == White)
          newLibs(newChains(neighbor)) += 1
      creditNeighbor(cleared - 1)
      creditNeighbor(cleared + 1)
      creditNeighbor(cleared - stride)
      creditNeighbor(cleared + stride)
    }

    def removeCapturedChain(root: Int): Unit = {
      var clearing  = root
      while ({
        newBoard(clearing) = Empty
        newHash ^= stoneHash(clearing, enemy)
        capturedStones += 1
        lastCaptured = clearing
        clearing = newNext(clearing)
        clearing != root
      }) ()
      var crediting = root
      while ({
        creditLibertiesAround(crediting)
        crediting = newNext(crediting)
        crediting != root
      }) ()
    }

    def captureIfDead(neighbor: Int): Unit =
      if (newBoard(neighbor) == enemy && newLibs(newChains(neighbor)) == 0)
        removeCapturedChain(newChains(neighbor))

    captureIfDead(west)
    captureIfDead(east)
    captureIfDead(south)
    captureIfDead(north)

    var root = placed

    def absorbFriendlyNeighbor(neighbor: Int): Unit =
      if (newBoard(neighbor) == friendly) {
        val neighborRoot = newChains(neighbor)
        if (neighborRoot != root)
          root = mergeChainRoots(newChains, newNext, newCounts, newLibs, root, neighborRoot)
      }

    absorbFriendlyNeighbor(west)
    absorbFriendlyNeighbor(east)
    absorbFriendlyNeighbor(south)
    absorbFriendlyNeighbor(north)

    val koMove =
      if (capturedStones == 1 && newCounts(root) == 1 && newLibs(root) == 1)
        Some(moveOfPaddedIndex(lastCaptured))
      else None

    new GoState(
      size,
      zobristTable,
      newBoard,
      newChains,
      newNext,
      newCounts,
      newLibs,
      -playerTurn,
      if (friendly == Black) capturesByBlack + capturedStones else capturesByBlack,
      if (friendly == White) capturesByWhite + capturedStones else capturesByWhite,
      koMove,
      newHash,
      positionHashHistory :+ newHash,
      occurredPositionHashes + newHash,
      0
    )
  }
}

object GoState {

  val BlackPlayer = 1
  val WhitePlayer = -1
  val NoOwner     = 0

  val supportedSizes: Set[Int] = Set(9, 13, 19)

  private val Empty: Byte  = 0
  private val Black: Byte  = 1
  private val White: Byte  = 2
  private val Border: Byte = 3
  private val NoChain      = -1

  def initial(size: Int): GoState =
    fromStoneOwners(size, _ => NoOwner, BlackPlayer, 0, 0, None, 0)

  def fromStoneOwners(
      size: Int,
      stoneOwnerAtMove: Int => Int,
      playerTurn: Int,
      capturesByBlack: Int,
      capturesByWhite: Int,
      simpleKoMove: Option[Int],
      consecutivePasses: Int
  ): GoState = {
    require(supportedSizes.contains(size), s"unsupported go board size $size")
    require(playerTurn == BlackPlayer || playerTurn == WhitePlayer, s"invalid playerTurn $playerTurn")
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
      stoneOwnerAtMove(move) match {
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

    new GoState(
      size,
      zobristTable,
      board,
      chainIds,
      nextStone,
      stoneCounts,
      pseudoLiberties,
      playerTurn,
      capturesByBlack,
      capturesByWhite,
      simpleKoMove,
      hash,
      Vector(hash),
      Set(hash),
      consecutivePasses
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
