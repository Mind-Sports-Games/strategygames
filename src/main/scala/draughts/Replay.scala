package strategygames.draughts

import cats.data.Validated
import cats.data.Validated.{ invalid, valid }
import cats.implicits._

import strategygames.{
  Action => StratAction,
  ActionStrs,
  Game => StratGame,
  Move => StratMove,
  Player,
  Situation => StratSituation
}
import strategygames.format.pgn.{ San, Tag, Tags }
import format.pdn.{ Parser, Reader, Std }
import format.{ FEN, Forsyth, Uci }
import variant._

case class Replay(setup: DraughtsGame, actions: List[Move], state: DraughtsGame) {

  lazy val chronoPlies = actions.reverse

  lazy val chronoActions: List[List[Move]] =
    chronoPlies
      .drop(1)
      .foldLeft(List(chronoPlies.take(1))) { case (turn, move) =>
        if (turn.head.head.player != move.player) {
          List(move) +: turn
        } else {
          (turn.head :+ move) +: turn.tail
        }
      }
      .reverse

  def addAction(move: Move, finalSquare: Boolean = false) = copy(
    actions = move.applyVariantEffect :: actions,
    state = state.apply(move, finalSquare)
  )

}

object Replay {

  def apply(game: DraughtsGame) = new Replay(game, Nil, game)

  def apply(
      actionStrs: ActionStrs,
      initialFen: Option[FEN],
      variant: Variant,
      finalSquare: Boolean
  ): Validated[String, Reader.Result] =
    actionStrs.some.filter(_.nonEmpty) toValid "[replay] pdn is empty" andThen { nonEmptyActionStrs =>
      Reader.replayResult(
        nonEmptyActionStrs,
        Tags(
          List(
            initialFen map { fen => Tag(_.FEN, fen.value) },
            variant.some.filterNot(_ == Standard) map { v => Tag(_.GameType, v.gameType) }
          ).flatten
        ),
        finalSquare
      )
    }

  // TODO: because this is primarily used in a Validation context, we should be able to
  //       return something that's runtime safe as well.
  def draughtsMove(action: StratAction) = action match {
    case StratMove.Draughts(m) => m
    case _                     => sys.error("Invalid draughts move")
  }

  // Both of the following commented out functions are unused by the rest of strategygames
  // and lila. Would need to upgrade to multiaction to use them
  // private def recursiveGames(game: DraughtsGame, sans: List[San]): Validated[String, List[DraughtsGame]] =
  //  sans match {
  //    case Nil         => valid(Nil)
  //    case san :: rest =>
  //      san(StratSituation.wrap(game.situation)) flatMap { move =>
  //        val newGame = StratGame.wrap(game)(move).toDraughts
  //        recursiveGames(newGame, rest) map { newGame :: _ }
  //      }
  //  }

  // def games(
  //    moveStrs: Iterable[String],
  //    initialFen: Option[FEN],
  //    variant: Variant
  // ): Validated[String, List[DraughtsGame]] =
  //  Parser.moves(moveStrs, variant) andThen { moves =>
  //    val game = makeGame(variant, initialFen)
  //    recursiveGames(game, moves.value) map { game :: _ }
  //  }

  type ErrorMessage = String
  def gameWithUciWhileValid(
      actionStrs: ActionStrs,
      initialFen: FEN,
      variant: Variant,
      iteratedCapts: Boolean = false
  ): (DraughtsGame, List[(DraughtsGame, Uci.WithSan)], Option[ErrorMessage]) = {

    def mk(
        g: DraughtsGame,
        plies: List[(San, String)],
        ambs: List[(San, String)]
    ): (List[(DraughtsGame, Uci.WithSan)], Option[ErrorMessage]) = {
      var newAmb                                                         = none[(San, String)]
      val res: (List[(DraughtsGame, Uci.WithSan)], Option[ErrorMessage]) = plies match {
        case (san, sanStr) :: rest =>
          san(
            StratSituation.wrap(g.situation),
            iteratedCapts,
            if (ambs.isEmpty) None
            else ambs.collect { case (ambSan, ambUci) if ambSan == san => ambUci }.some
          ).fold(
            err => (Nil, err.some),
            action => {
              val move    = action match {
                case StratMove.Draughts(m) => m
                case _                     => sys.error("Invalid draughts move")
              }
              val newGame = g(move)
              val uci     = move.toUci
              if (
                iteratedCapts && move.capture.fold(false)(_.length > 1) && move.situationBefore
                  .ambiguitiesMove(move) > ambs.length + 1
              )
                newAmb = (san -> uci.uci).some
              mk(newGame, rest, newAmb.fold(ambs)(_ :: ambs)) match {
                case (next, msg) => ((newGame, Uci.WithSan(uci, sanStr)) :: next, msg)
              }
            }
          )
        case _                     => (Nil, None)
      }
      if (res._2.isDefined && newAmb.isDefined) mk(g, plies, newAmb.get :: ambs)
      else res
    }

    val init = makeGame(variant, initialFen.some)
    Parser
      // Its ok to flatten actionStrs as the game is built back up again from the Situation
      // If we don't want to flatten then we need to do something like samurai gamelogic
      // where we use startPlayer and activePlayer
      .sans(actionStrs.flatten, variant)
      .fold(
        err => List.empty[(DraughtsGame, Uci.WithSan)] -> err.some,
        sans => mk(init, sans.value zip actionStrs.flatten, Nil)
      ) match {
      case (games, err) => (init, games, err)
    }
  }

  def unambiguousPdnMoves(
      pdnMoves: Seq[String],
      initialFen: Option[FEN],
      variant: Variant
  ): Option[List[String]] = {

    def mk(
        sit: Situation,
        moves: List[San],
        ambs: List[(San, String)]
    ): (List[String], Option[ErrorMessage]) = {
      var newAmb                                    = none[(San, String)]
      val res: (List[String], Option[ErrorMessage]) = moves match {
        case san :: rest =>
          san(
            StratSituation.wrap(sit),
            true,
            if (ambs.isEmpty) None
            else ambs.collect { case (ambSan, ambUci) if ambSan == san => ambUci }.some
          ).map(draughtsMove)
            .fold(
              err => (Nil, err.some),
              move => {
                val after       = Situation.withPlayerAfter(move.afterWithLastMove(true), sit.player)
                val ambiguities =
                  if (move.capture.fold(false)(_.length > 1)) move.situationBefore.ambiguitiesMove(move)
                  else 0
                if (ambiguities > ambs.length + 1)
                  newAmb = (san -> move.toUci.uci).some
                mk(after, rest, newAmb.fold(ambs)(_ :: ambs)) match {
                  case (next, msg) =>
                    val san = if (ambiguities > 1) move.toFullSan else move.toSan
                    (san :: next, msg)
                }
              }
            )
        case _           => (Nil, None)
      }
      if (res._2.isDefined && newAmb.isDefined) mk(sit, moves, newAmb.get :: ambs)
      else res
    }

    val init = initialFenToSituation(initialFen, variant)
    Parser
      .sans(pdnMoves, variant)
      .fold(
        err => Nil -> err.head.some,
        sans => mk(init, sans.value, Nil)
      ) match {
      case (_, Some(_)) => None
      case (moves, _)   => Option(moves)
    }
  }

  def exportScanMoves(
      uciMoves: Seq[String],
      initialFen: FEN,
      variant: Variant,
      debugId: String,
      iteratedCapts: Boolean = false
  ): List[String] = {

    def mk(
        g: DraughtsGame,
        moves: List[String],
        ambs: List[(String, String)]
    ): (List[String], Option[ErrorMessage]) = {
      var newAmb                                    = none[(String, String)]
      val res: (List[String], Option[ErrorMessage]) = moves match {
        case uci :: rest =>
          Uci.Move(uci) match {
            case Some(uciMove) =>
              Std(List(uciMove.orig, uciMove.dest), uciMove.capture.fold(false)(_.nonEmpty))
                .move(
                  g.situation,
                  true,
                  if (ambs.isEmpty) None
                  else ambs.collect { case (ambFrom, ambUci) if ambFrom == uci => ambUci }.some,
                  uciMove.capture
                )
                .fold(
                  err => {
                    // TODO: Warning?
                    // logger.warn(s"exportScanMoves($iteratedCapts) $debugId: $uci -> ${uciMove.orig}${if (uciMove.capture.fold(false)(_.nonEmpty)) "x" else "-"}${uciMove.dest} - error ${err.head}")
                    (Nil, err.some)
                  },
                  move => {
                    val newGame  = g(move, true)
                    val scanMove = move.toScanMove
                    if (
                      iteratedCapts && move.capture.fold(false)(_.length > 1) && move.situationBefore
                        .ambiguitiesMove(move) > ambs.length + 1
                    )
                      newAmb = (uci -> move.toUci.uci).some
                    mk(newGame, rest, newAmb.fold(ambs)(_ :: ambs)) match {
                      case (next, msg) => (scanMove :: next, msg)
                    }
                  }
                )
            case _             => (Nil, None)
          }
        case _           => (Nil, None)
      }
      if (res._2.isDefined && newAmb.isDefined) mk(g, moves, newAmb.get :: ambs)
      else res
    }

    val init     = makeGame(variant, initialFen.some)
    val moveList =
      if (iteratedCapts) uciMoves.toList
      else
        uciMoves.foldRight(List[String]()) { (move, moves) =>
          moves.headOption match {
            case Some(lastMove) if lastMove.take(2) == move.slice(2, 4) =>
              (move.take(2) + lastMove) :: moves.tail
            case _                                                      => move.take(4) :: moves
          }
        }
    mk(init, moveList, Nil) match {
      case (moves, None) => moves
      case _             => Nil
    }
  }

  private def recursiveUcis(
      sit: Situation,
      sans: List[San],
      finalSquare: Boolean = false
  ): Validated[String, List[Uci]] =
    sans match {
      case Nil         => valid(Nil)
      case san :: rest =>
        san(StratSituation.wrap(sit), finalSquare).map(draughtsMove) flatMap { move =>
          val after = Situation.withPlayerAfter(move.afterWithLastMove(finalSquare), sit.player)
          recursiveUcis(after, rest, finalSquare) map { move.toUci :: _ }
        }
    }

  private def recursiveSituations(
      sit: Situation,
      sans: List[San],
      finalSquare: Boolean = false
  ): Validated[String, List[Situation]] =
    sans match {
      case Nil         => valid(Nil)
      case san :: rest =>
        san(StratSituation.wrap(sit), finalSquare).map(draughtsMove) flatMap { move =>
          val after = Situation.withPlayerAfter(move.afterWithLastMove(finalSquare), sit.player)
          recursiveSituations(after, rest, finalSquare) map { after :: _ }
        }
    }

  private def recursiveSituationsFromUci(
      sit: Situation,
      ucis: List[Uci],
      finalSquare: Boolean = false
  ): Validated[String, List[Situation]] =
    ucis match {
      case Nil         => valid(Nil)
      case uci :: rest =>
        uci(sit, finalSquare) andThen { move =>
          val after = Situation.withPlayerAfter(move.afterWithLastMove(finalSquare), sit.player)
          recursiveSituationsFromUci(after, rest, finalSquare) map { after :: _ }
        }
    }

  private def recursiveReplayFromUci(
      replay: Replay,
      ucis: List[Uci],
      finalSquare: Boolean = false
  ): Validated[String, Replay] =
    ucis match {
      case Nil         => valid(replay)
      case uci :: rest =>
        uci(replay.state.situation, finalSquare) andThen { action =>
          recursiveReplayFromUci(replay.addAction(action, finalSquare), rest, finalSquare)
        }
    }

  private def initialFenToSituation(initialFen: Option[FEN], variant: Variant): Situation =
    initialFen.flatMap { fen => Forsyth.<<@(variant, fen) } | Situation(variant)

  def boards(
      actionStrs: ActionStrs,
      initialFen: Option[FEN],
      variant: Variant,
      finalSquare: Boolean = false
  ): Validated[String, List[Board]] =
    situations(actionStrs, initialFen, variant, finalSquare) map (_ map (_.board))

  def ucis(
      moveStrs: Iterable[String],
      initialSit: Situation,
      finalSquare: Boolean = false
  ): Validated[String, List[Uci]] =
    Parser.sans(moveStrs, initialSit.board.variant) andThen { moves =>
      recursiveUcis(initialSit, moves.value, finalSquare)
    }

  def situations(
      actionStrs: ActionStrs,
      initialFen: Option[FEN],
      variant: Variant,
      finalSquare: Boolean = false
  ): Validated[String, List[Situation]] = {
    val sit = initialFenToSituation(initialFen, variant)
    // Its ok to flatten actionStrs as the game is built back up again from the Situation
    Parser.sans(actionStrs.flatten, sit.board.variant) andThen { sans =>
      recursiveSituations(sit, sans.value, finalSquare) map { sit :: _ }
    }
  }

  def boardsFromUci(
      moves: List[Uci],
      initialFen: Option[FEN],
      variant: Variant,
      finalSquare: Boolean = false
  ): Validated[String, List[Board]] =
    situationsFromUci(moves, initialFen, variant, finalSquare) map (_ map (_.board))

  def situationsFromUci(
      moves: List[Uci],
      initialFen: Option[FEN],
      variant: Variant,
      finalSquare: Boolean = false
  ): Validated[String, List[Situation]] = {
    val sit = initialFenToSituation(initialFen, variant)
    recursiveSituationsFromUci(sit, moves, finalSquare) map { sit :: _ }
  }

  private def recursiveGamesFromUci(
      game: DraughtsGame,
      ucis: List[Uci],
      finalSquare: Boolean = false
  ): Validated[String, List[DraughtsGame]] =
    ucis match {
      case Nil                     => valid(List(game))
      case (uci: Uci.Move) :: rest =>
        game.apply(uci, finalSquare) andThen { case (game, _) =>
          recursiveGamesFromUci(game, rest, finalSquare) map { game :: _ }
        }
    }

  def gameFromUciStrings(
      uciStrings: List[String],
      initialFen: Option[FEN],
      variant: Variant,
      finalSquare: Boolean = false
  ): Validated[String, DraughtsGame] = {
    val init = makeGame(variant, initialFen)
    val ucis = uciStrings.flatMap(Uci.apply(_))
    if (uciStrings.size != ucis.size) invalid("Invalid Ucis")
    else recursiveGamesFromUci(init, ucis, finalSquare).map(_.last)
  }

  def apply(
      ucis: List[Uci],
      initialFen: Option[FEN],
      variant: Variant,
      finalSquare: Boolean = false
  ): Validated[String, Replay] =
    recursiveReplayFromUci(Replay(makeGame(variant, initialFen)), ucis, finalSquare)

  def plyAtFen(
      actionStrs: ActionStrs,
      initialFen: Option[FEN],
      variant: Variant,
      atFen: FEN
  ): Validated[String, Int] =
    if (Forsyth.<<@(variant, atFen).isEmpty) invalid(s"Invalid FEN $atFen")
    else {

      // we don't want to compare the full move number, to match transpositions
      def truncateFen(fen: FEN) = fen.value.split(' ').take(4) mkString " "
      val atFenTruncated        = truncateFen(atFen)
      def compareFen(fen: FEN)  = truncateFen(fen) == atFenTruncated

      def recursivePlyAtFen(sit: Situation, sans: List[San], ply: Int, turn: Int): Validated[String, Int] =
        sans match {
          case Nil         => invalid(s"Can't find $atFenTruncated, reached ply $ply, turn $turn")
          case san :: rest =>
            san(StratSituation.wrap(sit)).map(draughtsMove) flatMap { move =>
              val after        = move.situationAfter
              val newPlies     = ply + 1
              val newTurnCount = turn + (if (sit.player != after.player) 1 else 0)
              val fen          = Forsyth >> DraughtsGame(after, plies = newPlies, turnCount = newTurnCount)
              if (compareFen(fen)) Validated.valid(ply)
              else recursivePlyAtFen(after, rest, newPlies, newTurnCount)
            }
        }

      val sit = initialFen.flatMap {
        Forsyth.<<@(variant, _)
      } | Situation(variant)

      // Its ok to flatten actionStrs as the game is built back up again from the Situation
      Parser.sans(actionStrs.flatten, sit.board.variant) andThen { sans =>
        recursivePlyAtFen(sit, sans.value, 0, 0)
      }
    }

  private def makeGame(variant: Variant, initialFen: Option[FEN]): DraughtsGame = {
    val g = DraughtsGame(variant.some, initialFen)
    g.copy(
      startedAtPly = g.plies,
      startedAtTurn = g.turnCount
    )
  }
}
