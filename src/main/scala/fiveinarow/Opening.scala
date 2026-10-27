package strategygames.fiveinarow

sealed trait Opening {

  def initialStep: OpeningStep

  def steps: Set[OpeningStep]

  def stepAfterDrop(board: Board): OpeningStep

  def swapsOnDrop(step: OpeningStep): Boolean = false

  def allowsDrop(board: Board, at: Pos): Boolean

  def canSwap(step: OpeningStep, board: Board): Boolean

  def stepAfterSwap: OpeningStep = OpeningStep.Play

  def canSwap2(step: OpeningStep): Boolean = false

  def canOffer10(board: Board): Boolean = false

  def offeringStep(board: Board): Boolean = false

}

object Opening {

  case object Swap2 extends Opening {

    def initialStep = OpeningStep.Opening

    val steps: Set[OpeningStep] =
      Set(OpeningStep.Opening, OpeningStep.Choice, OpeningStep.Swap2Drops, OpeningStep.FinalChoice)

    def stepAfterDrop(board: Board): OpeningStep = board.openingStep match {
      case OpeningStep.Opening if board.droppedCount == 3    => OpeningStep.Choice
      case OpeningStep.Swap2Drops if board.droppedCount == 5 => OpeningStep.FinalChoice
      case OpeningStep.Opening | OpeningStep.Swap2Drops      => board.openingStep
      case _                                                 => OpeningStep.Play
    }

    override def swapsOnDrop(step: OpeningStep): Boolean = step == OpeningStep.FinalChoice

    def allowsDrop(board: Board, at: Pos): Boolean = true

    def canSwap(step: OpeningStep, board: Board): Boolean =
      step == OpeningStep.Choice || step == OpeningStep.FinalChoice

    override def canSwap2(step: OpeningStep): Boolean = step == OpeningStep.Choice

  }

  case object Taraguchi10 extends Opening {

    private val restrictedStones = 5
    private val swappedStones    = 5
    private val offeredStones    = 10

    def initialStep = OpeningStep.Placing

    val steps: Set[OpeningStep] =
      Set(OpeningStep.Placing, OpeningStep.SwapOffered, OpeningStep.Offering, OpeningStep.Offered)

    def stepAfterDrop(board: Board): OpeningStep = board.openingStep match {
      case OpeningStep.Offering                          =>
        if (board.offered.size == offeredStones) OpeningStep.Offered else OpeningStep.Offering
      case OpeningStep.Placing | OpeningStep.SwapOffered =>
        if (board.droppedCount <= swappedStones) OpeningStep.SwapOffered else OpeningStep.Play
      case _                                             => OpeningStep.Play
    }

    private def inOpening(step: OpeningStep) =
      step == OpeningStep.Placing || step == OpeningStep.SwapOffered

    // the nth stone goes within the (2n - 1) square about the centre, until the fifth
    def allowsDrop(board: Board, at: Pos): Boolean =
      if (board.openingStep == OpeningStep.Offering) offerAllowed(board, at)
      else if (!inOpening(board.openingStep) || board.droppedCount >= restrictedStones) true
      else distance(board.centre, at) <= board.droppedCount

    def canSwap(step: OpeningStep, board: Board): Boolean = step == OpeningStep.SwapOffered

    override def stepAfterSwap: OpeningStep = OpeningStep.Placing

    override def canOffer10(board: Board): Boolean =
      inOpening(board.openingStep) && board.droppedCount == 4

    override def offeringStep(board: Board): Boolean = board.openingStep == OpeningStep.Offering

    private def distance(a: Pos, b: Pos): Int =
      (a.file.index - b.file.index).abs max (a.rank.index - b.rank.index).abs

    private def offerAllowed(board: Board, at: Pos): Boolean =
      !board.offered.contains(at) && {
        val mirrors = symmetriesOf(board).map(_(at))
        !board.offered.exists(o => mirrors.contains(Some(o)))
      }

    private type Symmetry = Pos => Option[Pos]

    // the board's rotations and reflections that leave every stone where it was (renju offer 10)
    private def symmetriesOf(board: Board): List[Symmetry] = {
      val last                                       = board.boardSize.width - 1
      val transforms: List[(Int, Int) => (Int, Int)] = List(
        (x, y) => (x, y),
        (x, y) => (last - x, y),
        (x, y) => (x, last - y),
        (x, y) => (last - x, last - y),
        (x, y) => (y, x),
        (x, y) => (last - y, x),
        (x, y) => (y, last - x),
        (x, y) => (last - y, last - x)
      )
      val symmetries: List[Symmetry]                 = transforms.map { t => (p: Pos) =>
        val (x, y) = t(p.file.index, p.rank.index)
        Pos.at(x, y)
      }
      symmetries.filter { s =>
        board.pieces.forall { case (pos, piece) => s(pos).flatMap(board.apply).exists(_.role == piece.role) }
      }
    }

  }

}
