package strategygames.abalone

import cats.data.Validated
import cats.data.Validated.{invalid, valid}
import cats.implicits.catsSyntaxOptionId
import strategygames.abalone.format.pgn.{Parser, Reader}
import strategygames.abalone.format.{FEN, Forsyth, Uci}
import strategygames.abalone.variant.Variant
import strategygames.format.pgn.San
import strategygames.{ActionStrs, Player, Action => StratAction, Move => StratMove, Situation => StratSituation}

case class Replay(setup: Game, actions: List[Move], state: Game) {
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

  def addAction(move: Move) =
    copy(
      actions = move.applyVariantEffect :: actions,
      state = state.apply(move)
    )
}


object Replay {
  def apply(game: Game) = new Replay(game, Nil, game)

  def apply(
             actionStrs: ActionStrs,
             startPlayer: Player,
             activePlayer: Player,
             initialFen: Option[FEN],
             variant: Variant
           ): Validated[String, Reader.Result] = {
    val fen = initialFen.getOrElse(variant.initialFen)
    val (init, gameWithActions, error) =
      gameWithActionWhileValid(actionStrs, startPlayer, activePlayer, fen, variant)
    val game =
      gameWithActions.reverse.lastOption.map(_._1).getOrElse(init)

    error match {
      case None =>
        Validated.valid(
          Reader.Result.Complete(
            new Replay(init, gameWithActions.reverse.map(_._2), game)
          )
        )
      case Some(msg) => Validated.invalid(msg)
    }
  }

  // TODO: because this is primarily used in a Validation context, we should be able to
  //       return something that's runtime safe as well.
  def abaloneMove(action: StratAction) = action match {
    case StratMove.Abalone(m) => m
    case _ => sys.error("Invalid abalone move")
  }

  def replayMove(before: Game, orig: Pos, dest: Pos, endTurn: Boolean): Move = {
    val after = before.situation.board.variant.boardAfter(before.situation, orig, dest)

    Move(
      before.situation.player,
      orig = orig,
      dest = dest,
      situationBefore = before.situation,
      after = after,
      autoEndTurn = endTurn,
      capture = if (before.situation.board.pieces.size != after.pieces.size) Some(dest) else None
      //capture = if (before.situation.board.variant.boardType.isCell(dest) None else Some(dest)// Also valid
    )
  }

  def actionStrsWithEndTurn(actionStrs: ActionStrs): Seq[(String, Boolean)] =
    actionStrs.zipWithIndex.map { case (a, i) =>
      a.zipWithIndex.map { case (a1, i1) => (a1, i1 == a.size - 1 && i != actionStrs.size - 1) }
    }.flatten

  private def combineActionStrsWithEndTurn(
                                            actionStrs: ActionStrs,
                                            startPlayer: Player,
                                            activePlayer: Player
                                          ): Seq[(String, Boolean)] =
    actionStrsWithEndTurn(
      if (Player.fromTurnCount(actionStrs.size + startPlayer.fold(0, 1)) == activePlayer)
        actionStrs :+ Vector()
      else actionStrs
    )

  private def gameWithActionWhileValid(
                                        actionStrs: ActionStrs,
                                        startPlayer: Player,
                                        activePlayer: Player,
                                        initialFen: FEN,
                                        variant: Variant
                                      ): (Game, List[(Game, Move)], Option[String]) = {
    val init = makeGame(variant, initialFen.some)
    var state = init
    var errors = ""

    def replayMoveFromUci(
                           orig: Option[Pos],
                           dest: Option[Pos],
                           endTurn: Boolean
                         ): (Game, Move) =
      (orig, dest) match {
        case (Some(orig), Some(dest)) => {
          val move = replayMove(state, orig, dest, endTurn)
          state = state(move)
          (state, move)
        }
        case (orig, dest) => {
          val uciMove = s"${orig}${dest}"
          errors += uciMove + ","
          sys.error(s"Invalid move for replay: ${uciMove}")
        }
      }

    val gameWithActions: List[(Game, Move)] =
      combineActionStrsWithEndTurn(actionStrs, startPlayer, activePlayer).toList.map {
        case (Uci.Move.moveR(orig, dest), endTurn) =>
          replayMoveFromUci(
            Pos.fromKey(orig),
            Pos.fromKey(dest),
            endTurn
          )
        case (action: String, _) =>
          sys.error(s"Invalid move for replay: $action")
      }

    (init, gameWithActions, errors match {
      case "" => None;
      case _ => errors.some
    })
  }

  def gameWithUciWhileValid(
                             actionStrs: ActionStrs,
                             startPlayer: Player,
                             activePlayer: Player,
                             initialFen: FEN,
                             variant: Variant
                           ): (Game, List[(Game, Uci.WithSan)], Option[String]) = {
    val (game, gameWithActions, error) = gameWithActionWhileValid(
      actionStrs,
      startPlayer,
      activePlayer,
      initialFen,
      variant
    )
    (
      game,
      gameWithActions.map { v => {
        val (state, action) = v
        (state, Uci.WithSan(action.toUci, "NOSAN"))
      }
      },
      error
    )
  }

  private def recursiveSituations(sit: Situation, sans: List[San]): Validated[String, List[Situation]] =
    sans match {
      case Nil => valid(Nil)
      case san :: rest =>
        san(StratSituation.wrap(sit)).map(abaloneMove) flatMap { move =>
          val after = Situation(move.finalizeAfter, !sit.player) //TODO Alex
          recursiveSituations(after, rest) map {
            after :: _
          }
        }
    }

  private def recursiveSituationsFromUci(
                                          sit: Situation,
                                          ucis: List[Uci]
                                        ): Validated[String, List[Situation]] =
    ucis match {
      case Nil => valid(Nil)
      case uci :: rest =>
        uci(sit) andThen { move =>
          val after = Situation(move.finalizeAfter, !sit.player) //TODO Alex
          recursiveSituationsFromUci(after, rest) map {
            after :: _
          }
        }
    }

  private def recursiveReplayFromUci(replay: Replay, ucis: List[Uci]): Validated[String, Replay] =
    ucis match {
      case Nil => valid(replay)
      case uci :: rest =>
        uci(replay.state.situation) andThen { action =>
          recursiveReplayFromUci(replay.addAction(action), rest)
        }
    }

  private def initialFenToSituation(
                                     initialFen: Option[FEN],
                                     variant: Variant
                                   ): Situation = {
    initialFen.flatMap(Forsyth.<<) | Situation(Variant.default)
  } withVariant variant

  def boards(
              actionStrs: ActionStrs,
              initialFen: Option[FEN],
              variant: Variant
            ): Validated[String, List[Board]] = situations(actionStrs, initialFen, variant) map (_ map (_.board))

  def situations(
                  actionStrs: ActionStrs,
                  initialFen: Option[FEN],
                  variant: Variant
                ): Validated[String, List[Situation]] = {
    val sit = initialFenToSituation(initialFen, variant)
    // seemingly this isn't used
    Parser.sans(actionStrs.flatten, sit.board.variant) andThen { sans =>
      recursiveSituations(sit, sans.value) map {
        sit :: _
      }
    }
  }

  def boardsFromUci(
                     ucis: List[Uci],
                     initialFen: Option[FEN],
                     variant: Variant
                   ): Validated[String, List[Board]] = situationsFromUci(ucis, initialFen, variant) map (_ map (_.board))

  def situationsFromUci(
                         ucis: List[Uci],
                         initialFen: Option[FEN],
                         variant: Variant
                       ): Validated[String, List[Situation]] = {
    val sit = initialFenToSituation(initialFen, variant)
    recursiveSituationsFromUci(sit, ucis) map {
      sit :: _
    }
  }

  private def recursiveGamesFromUci(
                                     game: Game,
                                     ucis: List[Uci]
                                   ): Validated[String, List[Game]] =
    ucis match {
      case Nil => valid(List(game))
      case (uci: Uci.Move) :: rest =>
        game.apply(uci) andThen { case (game, _) =>
          recursiveGamesFromUci(game, rest) map {
            game :: _
          }
        }
    }

  def gameFromUciStrings(
                          uciStrings: List[String],
                          initialFen: Option[FEN],
                          variant: Variant
                        ): Validated[String, Game] = {
    val init = makeGame(variant, initialFen)
    val ucis = uciStrings.flatMap(Uci.apply(_))
    if (uciStrings.size != ucis.size) invalid("Invalid Ucis")
    else recursiveGamesFromUci(init, ucis).map(_.last)
  }

  def apply(
             ucis: List[Uci],
             initialFen: Option[FEN],
             variant: Variant
           ): Validated[String, Replay] =
    recursiveReplayFromUci(Replay(makeGame(variant, initialFen)), ucis)

  def plyAtFen(
                actionStrs: ActionStrs,
                initialFen: Option[FEN],
                variant: Variant,
                atFen: FEN
              ): Validated[String, Int] =
    if (Forsyth.<<@(variant, atFen).isEmpty) invalid(s"Invalid FEN $atFen")
    else {
      // We don't want to compare the full move number, to match transpositions
      def truncateFen(fen: FEN) = fen.value.split(' ').take(4) mkString " "

      val atFenTruncated = truncateFen(atFen)

      def compareFen(fen: FEN) = truncateFen(fen) == atFenTruncated

      def recursivePlyAtFen(sit: Situation, sans: List[San], ply: Int, turn: Int): Validated[String, Int] =
        sans match {
          case Nil => invalid(s"Can't find $atFenTruncated, reached ply $ply, turn $turn")
          case san :: rest =>
            san(StratSituation.wrap(sit)).map(abaloneMove) flatMap { move =>
              val after = move.situationAfter //TODO Alex
              val newPlies = ply + 1
              val newTurnCount = turn + (if (sit.player != after.player) 1 else 0)
              val fen = Forsyth >> Game(after, plies = newPlies, turnCount = newTurnCount)
              if (compareFen(fen)) Validated.valid(ply)
              else recursivePlyAtFen(after, rest, newPlies, newTurnCount)
            }
        }

      val sit = initialFen.flatMap {
        Forsyth.<<@(variant, _)
      } | Situation(variant)

      // seemingly this isn't used
      Parser.sans(actionStrs.flatten, sit.board.variant) andThen { sans =>
        recursivePlyAtFen(sit, sans.value, 0, 0)
      }
    }

  private def makeGame(variant: Variant, initialFen: Option[FEN]): Game = {
    val g = Game(variant.some, initialFen)
    g.copy(
      startedAtPly = g.plies,
      startedAtTurn = g.turnCount
    )
  }
}