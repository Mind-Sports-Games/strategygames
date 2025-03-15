package strategygames

import format.Uci
import variant.Variant

//same template as chess.CheckCount
case class Score(p1: Int = 0, p2: Int = 0) {

  def add(player: Player, increment: Int = 1) =
    copy(
      p1 = p1 + (increment * player.fold(1, 0)),
      p2 = p2 + (increment * player.fold(0, 1))
    )

  def nonEmpty = p1 > 0 || p2 > 0

  def apply(player: Player) = player.fold(p1, p2)

  def fenStr = s"${p1} ${p2}"

}

case class MultiPointState(target: Int, p1Points: Int = 0, p2Points: Int = 0)

sealed abstract class History(
    val lastTurn: List[Uci] = List.empty,
    val currentTurn: List[Uci] = List.empty,
    val forcedTurn: Boolean = false,
    val positionHashes: PositionHash = Array.empty,
    val variant: Option[Variant] = None,
    val castles: chess.Castles = chess.Castles.all,
    val checkCount: chess.CheckCount = chess.CheckCount(0, 0),
    val unmovedRooks: chess.UnmovedRooks = chess.UnmovedRooks.default,
    val kingMoves: draughts.KingMoves = draughts.KingMoves(),
    val score: Score = Score(0, 0),
    val captures: Score = Score(0, 0),
    val multiPointState: Option[MultiPointState] = None,
    val halfMoveClock: Int = 0
) {

  lazy val lastAction: Option[Uci] =
    if (currentTurn.nonEmpty) currentTurn.reverse.headOption else lastTurn.reverse.headOption

  lazy val recentTurn: List[Uci] = if (currentTurn.nonEmpty) currentTurn else lastTurn

  private def turnToUciString(turn: List[Uci]): Option[String] =
    if (turn.nonEmpty) Some(turn.map(_.uci).mkString(",")) else None

  lazy val lastTurnUciString = turnToUciString(lastTurn)

  lazy val currentTurnUciString = turnToUciString(currentTurn)

  lazy val recentTurnUciString = turnToUciString(recentTurn)

  override def toString = {
    val positions = (positionHashes grouped Hash.size).toList
    s"${recentTurnUciString.getOrElse("-")} ${positions.map(Hash.debug).mkString(" ")}"
  }

}

object History {

  final case class Chess(h: chess.History)
      extends History(
        lastTurn = h.lastTurn.map(Uci.wrap),
        currentTurn = h.currentTurn.map(Uci.wrap),
        positionHashes = h.positionHashes,
        castles = h.castles,
        checkCount = h.checkCount,
        unmovedRooks = h.unmovedRooks,
        halfMoveClock = h.halfMoveClock
      )

  final case class Draughts(h: draughts.DraughtsHistory)
      extends History(
        lastTurn = h.lastMove.map(Uci.wrap).toList,
        positionHashes = h.positionHashes,
        variant = Some(Variant.Draughts(h.variant)),
        kingMoves = h.kingMoves
      )

  final case class FairySF(h: fairysf.History)
      extends History(
        lastTurn = h.lastTurn.map(Uci.wrap),
        currentTurn = h.currentTurn.map(Uci.wrap),
        positionHashes = h.positionHashes,
        halfMoveClock = h.halfMoveClock
      )

  final case class Samurai(h: samurai.History)
      extends History(
        lastTurn = h.lastTurn.map(Uci.wrap),
        currentTurn = h.currentTurn.map(Uci.wrap),
        positionHashes = h.positionHashes,
        halfMoveClock = h.halfMoveClock
      )

  final case class Togyzkumalak(h: togyzkumalak.History)
      extends History(
        lastTurn = h.lastTurn.map(Uci.wrap),
        currentTurn = h.currentTurn.map(Uci.wrap),
        positionHashes = h.positionHashes,
        halfMoveClock = h.halfMoveClock,
        score = h.score
      )

  final case class Go(h: go.History)
      extends History(
        lastTurn = h.lastTurn.map(Uci.wrap),
        currentTurn = h.currentTurn.map(Uci.wrap),
        positionHashes = h.positionHashes,
        halfMoveClock = h.halfMoveClock,
        score = h.score,
        captures = h.captures
      )

  final case class Backgammon(h: backgammon.History)
      extends History(
        lastTurn = h.lastTurn.map(Uci.wrap),
        currentTurn = h.currentTurn.map(Uci.wrap),
        forcedTurn = h.forcedTurn,
        positionHashes = h.positionHashes,
        halfMoveClock = h.halfMoveClock,
        score = h.score,
        multiPointState = h.multiPointState
      )

//  @deprecated("Alex", since="1.5.5") final case class Abalone(h: abalone.History)
//      extends History(
//        lastTurn = h.lastTurn.map(Uci.wrap),
//        currentTurn = h.currentTurn.map(Uci.wrap),
//        positionHashes = h.positionHashes,
//        halfMoveClock = h.halfMoveClock,
//        score = h.score
//      )
  final case class Abalone(h: abalone.HHistory)
      extends History(
        lastTurn = h.lastTurn.map(Uci.wrap),
        currentTurn = h.currentTurn.map(Uci.wrap),
        positionHashes = h.positionHashes,
        halfMoveClock = h.halfMoveClock,
        score = h.score
      )

  final case class Dameo(h: dameo.History)
      extends History(
        lastTurn = h.lastTurn.map(Uci.wrap),
        currentTurn = h.currentTurn.map(Uci.wrap),
        positionHashes = h.positionHashes,
        halfMoveClock = h.halfMoveClock
      )

  implicit def chessHistory(h: chess.History)               = Chess(h)
  implicit def draughtsHistory(h: draughts.DraughtsHistory) = Draughts(h)
  implicit def fairysfHistory(h: fairysf.History)           = FairySF(h)
  implicit def samuraiHistory(h: samurai.History)           = Samurai(h)
  implicit def togyzkumalakHistory(h: togyzkumalak.History) = Togyzkumalak(h)
  implicit def goHistory(h: go.History)                     = Go(h)
  implicit def backgammonHistory(h: backgammon.History)     = Backgammon(h)
  implicit def abaloneHistory(h: abalone.HHistory)           = Abalone(h)
  implicit def dameoHistory(h: dameo.History)               = Dameo(h)

  // lila
  def apply(
      lib: GameLogic,
      lastTurn: List[Uci] = List.empty,
      currentTurn: List[Uci] = List.empty,
      forcedTurn: Boolean = false,
      positionHashes: PositionHash = Array.empty,
      variant: Option[Variant] = None,
      castles: chess.Castles = chess.Castles.all,
      checkCount: chess.CheckCount = chess.CheckCount(0, 0),
      unmovedRooks: chess.UnmovedRooks = chess.UnmovedRooks.default,
      kingMoves: draughts.KingMoves = draughts.KingMoves(),
      score: Score = Score(0, 0),
      captures: Score = Score(0, 0),
      multiPointState: Option[MultiPointState] = None,
      halfMoveClock: Int = 0
  ): History = lib match {
    case GameLogic.Draughts()     =>
      Draughts(
        draughts.DraughtsHistory(
          lastMove = lastTurn.headOption.map(lm => lm.toDraughts),
          positionHashes = positionHashes,
          variant = variant match {
            case Some(Variant.Draughts(variant)) => variant
            case None                            => strategygames.draughts.variant.Standard
            case _                               => sys.error("Mismatched variant types for draughts history")
          },
          kingMoves = kingMoves
        )
      )
    case GameLogic.Chess()        =>
      Chess(
        chess.History(
          lastTurn = lastTurn.map(lm => lm.toChess),
          currentTurn = currentTurn.map(lm => lm.toChess),
          positionHashes = positionHashes,
          castles = castles,
          checkCount = checkCount,
          unmovedRooks = unmovedRooks,
          halfMoveClock = halfMoveClock
        )
      )
    case GameLogic.FairySF()      =>
      FairySF(
        fairysf.History(
          lastTurn = lastTurn.map(lm => lm.toFairySF),
          currentTurn = currentTurn.map(lm => lm.toFairySF),
          positionHashes = positionHashes,
          halfMoveClock = halfMoveClock
        )
      )
    case GameLogic.Samurai()      =>
      Samurai(
        samurai.History(
          lastTurn = lastTurn.map(lm => lm.toSamurai),
          currentTurn = currentTurn.map(lm => lm.toSamurai),
          positionHashes = positionHashes,
          halfMoveClock = halfMoveClock
        )
      )
    case GameLogic.Togyzkumalak() =>
      Togyzkumalak(
        togyzkumalak.History(
          lastTurn = lastTurn.map(lm => lm.toTogyzkumalak),
          currentTurn = currentTurn.map(lm => lm.toTogyzkumalak),
          positionHashes = positionHashes,
          halfMoveClock = halfMoveClock,
          score = score
        )
      )
    case GameLogic.Go()           =>
      Go(
        go.History(
          lastTurn = lastTurn.map(lm => lm.toGo),
          currentTurn = currentTurn.map(lm => lm.toGo),
          positionHashes = positionHashes,
          halfMoveClock = halfMoveClock,
          score = score,
          captures = captures
        )
      )
    case GameLogic.Backgammon()   =>
      Backgammon(
        backgammon.History(
          lastTurn = lastTurn.map(lm => lm.toBackgammon),
          currentTurn = currentTurn.map(lm => lm.toBackgammon),
          forcedTurn = forcedTurn,
          positionHashes = positionHashes,
          halfMoveClock = halfMoveClock,
          score = score,
          multiPointState = multiPointState
        )
      )
    case GameLogic.Abalone()      =>
      Abalone(
        abalone.HHistory(
          lastTurn = lastTurn.map(lm => lm.toAbalone),
          currentTurn = currentTurn.map(lm => lm.toAbalone),
          positionHashes = positionHashes,
          halfMoveClock = halfMoveClock,
          score = score
        )
      )
    case GameLogic.Dameo()        =>
      Dameo(
        dameo.History(
          lastTurn = lastTurn.map(lm => lm.toDameo),
          currentTurn = currentTurn.map(lm => lm.toDameo),
          positionHashes = positionHashes,
          halfMoveClock = halfMoveClock
        )
      )
    case _                        => sys.error("Mismatched gamelogic types 1")
  }

}
