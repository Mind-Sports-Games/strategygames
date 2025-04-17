package strategygames.dameo

import strategygames.Player

import scala.annotation.nowarn

final case class Actor(
    piece: Piece,
    pos: Pos,
    board: Board
) {

  lazy val noncaptures: List[Move]                = noncaptureMoves()
  lazy val captures: List[Move]                   = capturesWithLineval._1
  lazy val capturesWithLineval: (List[Move], Int) = captureMoves()
  def getCaptures()                               = captures

  private def noncaptureMoves(): List[Move] = {
    def dy: Int        = if (player == P1) 1 else -1
    def dxs: List[Int] = List(-1, 0, 1)

    def posits: List[Pos] = piece.role match {
      case Man  =>
        dxs
          .flatMap(dx => linearStep(Some(pos), dx, dy))
          .filter(board.withinBounds)
          .filter(board.empty)
      case King =>
        dxs
          .flatMap(dx =>
            dxs.flatMap(dy =>
              if (dx == 0 && dy == 0) None
              else
                LazyList
                  .iterate(Option(pos))(_.flatMap(_.step(dx, dy)))
                  .tail
                  .takeWhile(p => !p.isEmpty && board.withinBounds(p.get) && board.empty(p.get))
            )
          )
          .flatten
      case _    => List()
    }

    posits.flatMap(dest =>
      Some(
        Move(
          piece = piece,
          orig = pos,
          dest = dest,
          situationBefore = Situation(board, piece.player),
          after = board.move(pos, dest).get,
          autoEndTurn = true,
          promotion = Option.when(piece.role == Man && board.backrow(dest, piece.player))(King)
        )
      )
    )
  }

  private def captureMoves(): (List[Move], Int) = {

    /* List of (capture pos, destination pos) pairs together with the length
    of the full capture chain */
    def capInfo: (List[(Pos, Pos)], Int) = piece.role match {
      case role @ (Man | King | ActiveMan | ActiveKing) => {
        def chains: List[List[(Pos, Pos)]] = role match {
          case Man | ActiveMan   =>
            allCaptureChains()
          case King | ActiveKing =>
            allCaptureChains(true)
          case _                 => List()
        }
        chains match {
          case List() => (List(), 0)
          case chains => {
            def byLength: Map[Int, List[List[(Pos, Pos)]]] = chains.groupBy(_.length)
            def maxLen: Int                                = byLength.keys.max
            (byLength(maxLen).map(_(0)).distinct, maxLen)
          }
        }
      }
      case _                                            => (List(), 0)
    }

    val (capAndDest, capLen) = capInfo

    (
      capAndDest.map { case (cap, dest) =>
        Move(
          piece = piece,
          orig = pos,
          dest = dest,
          situationBefore = Situation(board, piece.player),
          after = board.move(pos, dest).get,
          autoEndTurn = capLen == 1,
          capture = Some(cap),
          promotion = Option.when(
            capLen == 1 && (piece.role == Man || piece.role == ActiveMan) && board.backrow(dest, piece.player)
          )(King)
        )
      },
      capLen
    )
  }

  def linearStep(stepPos: Option[Pos], dx: Int, dy: Int): Option[Pos] = {
    /* Step through a line of own pieces in this direction */
    def nextPos: Option[Pos] = stepPos.flatMap(_.step(dx, dy))
    if (nextPos.flatMap(board.pieces.get(_)) == Some(piece)) {
      linearStep(nextPos, dx, dy)
    } else {
      nextPos
    }
  }

  def allCaptureChains(
      king: Boolean = false,
      curPos: Pos = pos,
      curBoard: Board = board,
      thisChain: List[(Pos, Pos)] = List()
  ): List[List[(Pos, Pos)]] = {
    def nextSteps: List[(Pos, Pos)] = if (king) {
      allKingCaptureSteps(curBoard, curPos)
    } else {
      allCaptureSteps(curBoard, curPos)
    }
    if (nextSteps.isEmpty) {
      return if (thisChain.isEmpty) List() else List(thisChain)
    } else {
      return nextSteps.map { case (cap, dest) =>
        allCaptureChains(
          king,
          dest,
          Board(
            curBoard.pieces - curPos - cap + (cap -> Piece(!player, GhostMan)),
            board.history,
            board.variant
          ),
          thisChain :+ (cap, dest)
        )
      }.flatten
    }
  }

  def allCaptureSteps(curBoard: Board, curPos: Pos): List[(Pos, Pos)] = {
    def dxys: List[(Int, Int)] = List((-1, 0), (1, 0), (0, -1), (0, 1))
    dxys.flatMap { case (dx, dy) => captureStep(curBoard, Some(curPos), dx, dy) }
  }

  def captureStep(curBoard: Board, stepPos: Option[Pos], dx: Int, dy: Int): Option[(Pos, Pos)] = {
    def capPos: Option[Pos]  = stepPos
      .flatMap(_.step(dx, dy))
      .filter(curBoard.pieces.get(_).map(_.player) == Some(!player))
      .filter(curBoard.pieces.get(_).map(_.isGhost) != Some(true))
    def destPos: Option[Pos] = capPos
      .flatMap(_.step(dx, dy))
      .filter(curBoard.empty(_))
      .filter(curBoard.withinBounds(_))
    for (a <- capPos; b <- destPos) yield (a, b)
  }

  def allKingCaptureSteps(curBoard: Board, curPos: Pos): List[(Pos, Pos)] = {
    def dxys: List[(Int, Int)] = List((-1, 0), (1, 0), (0, -1), (0, 1))
    dxys.flatMap { case (dx, dy) => kingCaptureSteps(curBoard, curPos, dx, dy) }
  }

  def kingCaptureSteps(curBoard: Board, stepPos: Pos, dx: Int, dy: Int): List[(Pos, Pos)] = {
    def findCapPos(curPos: Option[Pos]): Option[Pos] = {
      if (
        curPos.isEmpty || !curBoard.withinBounds(curPos.get)
        || curBoard.pieces.get(curPos.get).map(_.player) == Some(player)
        || curBoard.pieces.get(curPos.get).map(_.isGhost) == Some(true)
      ) {
        None
      } else if (curBoard.empty(curPos.get)) {
        findCapPos(curPos.flatMap(_.step(dx, dy)))
      } else {
        curPos
      }
    }
    def capPos: Option[Pos]                          = findCapPos(stepPos.step(dx, dy))
    def destPos: List[Pos]                           = LazyList
      .iterate(capPos)(_.flatMap(_.step(dx, dy)))
      .tail
      .takeWhile(p => !p.isEmpty && curBoard.withinBounds(p.get) && curBoard.empty(p.get))
      .toList
      .flatten
    destPos.map((capPos.get, _))
  }

  def player: Player         = piece.player
  def is(c: Player): Boolean = c == piece.player
  def is(p: Piece): Boolean  = p == piece

  def onLongDiagonal: Boolean = false

  @nowarn private def shortRangeMoves(dirs: Directions, checkPromotion: Boolean): List[Move] = List()

  @nowarn private def longRangeMoves(dirs: Directions): List[Move] = List()

}
