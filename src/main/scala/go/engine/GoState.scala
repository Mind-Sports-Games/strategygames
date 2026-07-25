package strategygames.go.engine

/** An immutable Go position: stones, chains, side to move, and the position history superko is judged
  * against.
  *
  * Points use the engine move encoding shared with `Api.uciToMove`: file `f` (0-based, a = 0) on rank `r`
  * (1-based) is `size * (r - 1) + f`, and `passMove` (`size * size`) is the pass. `playerTurn` is
  * [[GoState.BlackPlayer]] (1) or [[GoState.WhitePlayer]] (-1).
  *
  * A pass records no position hash, so a repeat separated by an odd number of passes still collides: this is
  * positional superko, not situational. Architecture and the decisions behind it: `docs/go-engine.md`.
  *
  * @param chainPseudoLiberties
  *   per chain root, each empty neighbour counted once per adjacent stone of the chain, so a liberty shared
  *   by k stones counts k times. The count is therefore not the liberty count, but it is zero exactly when
  *   the liberty count is zero — the only question capture asks.
  */
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
    private[engine] val occurredPositionHashes: Set[Long],
    val consecutivePasses: Int,
    val capturedMovesOnLastPlacement: List[Int]
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

  /** A pass is always legal. A placement is legal on an empty point that is not the simple ko point and that
    * neither leaves its own chain without liberties nor recreates a position already in this state's history.
    *
    * The simple ko point is redundant while the history is complete — superko forbids the recapture anyway —
    * but a state rebuilt from a FEN starts with an empty history, so the ko point read from the FEN is the
    * only thing left protecting it.
    */
  def isLegal(move: Int): Boolean =
    move == passMove || (move >= 0 && move < passMove && move != forbiddenKoPoint && {
      val padded = paddedIndexOfMove(move)
      board(padded) == Empty && isLegalEmptyPoint(padded, stoneToPlace, enemyStone)
    })

  private def forbiddenKoPoint: Int = simpleKoMove.getOrElse(NoPoint)

  private[engine] def withReplayHistory(
      hashes: Set[Long],
      lastPlacementCaptures: List[Int]
  ): GoState = {
    require(
      hashes.contains(positionHash),
      s"replay history must contain this position's hash $positionHash"
    )
    new GoState(
      size,
      zobristTable,
      board,
      chainIds,
      nextStoneInChain,
      chainStoneCounts,
      chainPseudoLiberties,
      playerTurn,
      capturesByBlack,
      capturesByWhite,
      simpleKoMove,
      positionHash,
      hashes,
      consecutivePasses,
      lastPlacementCaptures
    )
  }

  /** The position with the given stones lifted, as agreed dead at the end of the game. Rebuilt from stone
    * owners, so the superko history restarts from the resulting position — acceptable because no further
    * placements follow dead stone selection.
    */
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

  /** Chinese area score: stones plus the empty regions bordered by exactly one colour. A region bordered by
    * no colour at all — only the empty board — counts for nobody. Komi is not included.
    */
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

  def legalMoves: Array[Int] = {
    val drops    = computedLegalDrops
    val withPass = java.util.Arrays.copyOf(drops, drops.length + 1)
    withPass(drops.length) = passMove
    withPass
  }

  def legalDrops: Array[Int] = computedLegalDrops.clone()

  /** The position after playing `move`.
    *
    * @throws IllegalArgumentException
    *   if the move is not [[isLegal]]. Nothing in the engine applies a move unchecked.
    */
  def apply(move: Int): GoState =
    if (!isLegal(move))
      throw new IllegalArgumentException(s"Illegal go move $move for player $turn on ${size}x${size}")
    else if (move == passMove) afterPass
    else afterPlacement(move)

  private lazy val computedLegalDrops: Array[Int] = {
    val friendly  = stoneToPlace
    val enemy     = enemyStone
    val boardArr  = board
    val koPoint   = forbiddenKoPoint
    val collected = new Array[Int](passMove)
    var count     = 0
    var move      = 0
    var padded    = stride + 1
    var row       = 0
    while (row < size) {
      var column = 0
      while (column < size) {
        if (move != koPoint && boardArr(padded) == Empty && isLegalEmptyPoint(padded, friendly, enemy)) {
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
    java.util.Arrays.copyOf(collected, count)
  }

  private def paddedIndexOfMove(move: Int): Int   = (move / size + 1) * stride + move     % size + 1
  private def moveOfPaddedIndex(padded: Int): Int = (padded / stride - 1) * size + padded % stride - 1

  private def stoneToPlace: Byte = if (playerTurn == BlackPlayer) Black else White
  private def enemyStone: Byte   = if (playerTurn == BlackPlayer) White else Black

  // NOTE: the low table bit is the colour, which relies on Black and White being 1 and 2.
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

      // NOTE: placing on `placed` costs the chain one pseudo-liberty per adjacency to it, so the
      // chain dies exactly when every pseudo-liberty it has is one of those adjacencies. The `<= 4`
      // test is only a cheap reject for chains that plainly have liberties elsewhere.
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

      // NOTE: only a capture can shrink the board back to an earlier arrangement, so superko is
      // tested here and nowhere else. The resulting hash is predicted by XORing the placed stone
      // and the doomed chains out of the current one — no board copy, no mutation.
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
      occurredPositionHashes,
      consecutivePasses + 1,
      Nil
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

    var capturedStones           = 0
    var capturedMoves: List[Int] = Nil

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
        capturedMoves = moveOfPaddedIndex(clearing) :: capturedMoves
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

    // NOTE: only load-bearing after a FEN rebuild — see the parseKoPoint NOTE in GoFen.
    val koMove =
      if (capturedStones == 1 && newCounts(root) == 1 && newLibs(root) == 1)
        Some(capturedMoves.head)
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
      occurredPositionHashes + newHash,
      0,
      capturedMoves
    )
  }
}

object GoState {

  val BlackPlayer = 1
  val WhitePlayer = -1
  val NoOwner     = 0

  // NOTE: a new size must also touch Api's fen regexes (row-count range {8,18}) and FEN.variant's size
  // inference — the other two places outside variant/ that know the board-size catalog.
  val supportedSizes: Set[Int] = Set(9, 13, 19)

  private val Empty: Byte  = 0
  private val Black: Byte  = 1
  private val White: Byte  = 2
  private val Border: Byte = 3
  private val NoChain      = -1
  private val NoPoint      = -1

  def initial(size: Int): GoState =
    fromStoneOwners(size, _ => NoOwner, BlackPlayer, 0, 0, None, 0)

  /** Rebuilds chains and the position hash from a bare arrangement of stones, as read from a FEN. A board
    * carries no record of the positions that preceded it, so the superko history begins empty apart from this
    * position; replays that need it feed the whole action sequence instead.
    */
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

    // Only the west and south neighbours are joined: scanning every point in order visits each
    // adjacent pair from exactly one of its two ends.
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
      Set(hash),
      consecutivePasses,
      Nil
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

  // NOTE: relabels the smaller chain onto the larger and splices the two circular stone lists, so
  // `chainIds` always holds the exact root of a stone's chain and no find-with-compression is
  // needed anywhere. Returns the surviving root.
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
