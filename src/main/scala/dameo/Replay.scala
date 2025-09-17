package strategygames.dameo

import cats.data.Validated
import cats.data.Validated.{ invalid, valid }
import cats.implicits._

import strategygames.format.pgn.San
import strategygames.dameo.format.pdn.{ Parser, Reader }
import strategygames.dameo.format.{ FEN, Forsyth, Uci }
import strategygames.{ Action => StratAction, ActionStrs, Move => StratMove, Situation => StratSituation }

case class Replay(setup: Game, actions: List[Move], state: Game) {

  lazy val chronoPlies = actions.reverse

  lazy val chronoActions: List[List[Move]] =
    chronoPlies
      .drop(1)
      .foldLeft(List(chronoPlies.take(1))) { case (turn, action) =>
        if (turn.head.head.player != action.player) {
          List(action) +: turn
        } else {
          (turn.head :+ action) +: turn.tail
        }
      }
      .reverse

  def addAction(move: Move) =
    copy(
      actions = move :: actions,
      state = state.apply(move)
    )

}

object Replay {

  def apply(game: Game) = new Replay(game, Nil, game)

  def apply(
      actionStrs: ActionStrs,
      initialFen: Option[FEN],
      variant: strategygames.dameo.variant.Variant
  ): Validated[String, Reader.Result] = {
    val fen                            = initialFen.getOrElse(variant.initialFen)
    val (init, gameWithActions, error) =
      gameWithActionWhileValid(actionStrs, fen, variant)
    val game                           =
      gameWithActions.reverse.lastOption.map(_._1).getOrElse(init)

    error match {
      case None      =>
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
  private def dameoAction(action: StratAction) = action match {
    case StratMove.Dameo(m) => m
    case _                  => sys.error(s"Invalid dameo action $action")
  }

  private def gameWithActionWhileValid(
      actionStrs: ActionStrs,
      initialFen: FEN,
      variant: strategygames.dameo.variant.Variant
  ): (Game, List[(Game, Move)], Option[String]) = {
    val init   = makeGame(variant, initialFen.some)
    var state  = init
    var errors = ""

    def replayMoveFromUci(
        orig: Option[Pos],
        dest: Option[Pos]
    ): (Game, Move) =
      (orig, dest) match {
        case (Some(orig), Some(dest)) => {
          state.situation.board.move(orig, dest) match {
            case Some(_) => {
              val actor = state.situation.board.actors(orig)
              val actorMoves = actor.captures ++ actor.noncaptures
              actorMoves.find(_.dest == dest) match {
                case Some(move) => {
                  state = state(move)
                  (state, move)
                }
                case _          => {
                  val uciMove = s"${orig}${dest}"
                  errors += uciMove + ","
                  sys.error(s"Invalid move for replay: ${uciMove}")
                }
              }
            }
            case _                => {
              val uciMove = s"${orig}${dest}"
              errors += uciMove + ","
              sys.error(s"Invalid move for replay: ${uciMove}")
            }
          }
        }
        case (orig, dest)             => {
          val uciMove = s"${orig}${dest}"
          errors += uciMove + ","
          sys.error(s"Invalid move for replay: ${uciMove}")
        }
      }

    val gameWithActions: List[(Game, Move)] =
      actionStrs.flatMap { turnStrs =>
        turnStrs.map {
          case uci => {
            uci match {
              case Uci.Move.moveR(orig, dest, _) =>
                replayMoveFromUci(
                  Pos.fromKey(orig),
                  Pos.fromKey(dest)
                )
              case (action: String)                      =>
                sys.error(s"Invalid action for replay: $action")
            }
          }
        }
      }.toList

    (init, gameWithActions, errors match { case "" => None; case _ => errors.some })
  }

  def gameWithUciWhileValid(
      actionStrs: ActionStrs,
      initialFen: FEN,
      variant: strategygames.dameo.variant.Variant
  ): (Game, List[(Game, Uci.WithSan)], Option[String]) = {
    val (game, gameWithActions, error) = gameWithActionWhileValid(
      actionStrs,
      initialFen,
      variant
    )
    (
      game,
      gameWithActions.map { v =>
        {
          val (state, action) = v
          (state, Uci.WithSan(action.toUci, "NOSAN"))
        }
      },
      error
    )
  }

  private def recursiveSituations(sit: Situation, sans: List[San]): Validated[String, List[Situation]] =
    sans match {
      case Nil         => valid(Nil)
      case san :: rest =>
        san(StratSituation.wrap(sit)).map(dameoAction) flatMap { move =>
          val after = Situation(move.finalizeAfter, !sit.player)
          recursiveSituations(after, rest) map { after :: _ }
        }
    }

  private def recursiveSituationsFromUci(
      sit: Situation,
      ucis: List[Uci]
  ): Validated[String, List[Situation]] =
    ucis match {
      case Nil         => valid(Nil)
      case uci :: rest =>
        uci(sit) andThen { move =>
          val after = Situation(move.finalizeAfter, !sit.player)
          recursiveSituationsFromUci(after, rest) map { after :: _ }
        }
    }

  private def recursiveReplayFromUci(replay: Replay, ucis: List[Uci]): Validated[String, Replay] =
    ucis match {
      case Nil         => valid(replay)
      case uci :: rest =>
        uci(replay.state.situation) andThen { action =>
          recursiveReplayFromUci(replay.addAction(action), rest)
        }
    }

  private def initialFenToSituation(
      initialFen: Option[FEN],
      variant: strategygames.dameo.variant.Variant
  ): Situation = {
    initialFen.flatMap(Forsyth.<<) | Situation(strategygames.dameo.variant.Variant.default)
  } withVariant variant

  def boards(
      actionStrs: ActionStrs,
      initialFen: Option[FEN],
      variant: strategygames.dameo.variant.Variant
  ): Validated[String, List[Board]] = situations(actionStrs, initialFen, variant) map (_ map (_.board))

  def situations(
      actionStrs: ActionStrs,
      initialFen: Option[FEN],
      variant: strategygames.dameo.variant.Variant
  ): Validated[String, List[Situation]] = {
    val sit = initialFenToSituation(initialFen, variant)
    // seemingly this isn't used
    Parser.sans(actionStrs.flatten, sit.board.variant) andThen { sans =>
      recursiveSituations(sit, sans.value) map { sit :: _ }
    }
  }

  def boardsFromUci(
      ucis: List[Uci],
      initialFen: Option[FEN],
      variant: strategygames.dameo.variant.Variant
  ): Validated[String, List[Board]] = situationsFromUci(ucis, initialFen, variant) map (_ map (_.board))

  def situationsFromUci(
      ucis: List[Uci],
      initialFen: Option[FEN],
      variant: strategygames.dameo.variant.Variant
  ): Validated[String, List[Situation]] = {
    val sit = initialFenToSituation(initialFen, variant)
    recursiveSituationsFromUci(sit, ucis) map { sit :: _ }
  }

  private def recursiveGamesFromUci(
      game: Game,
      ucis: List[Uci]
  ): Validated[String, List[Game]] =
    ucis match {
      case Nil         => valid(List(game))
      case uci :: rest =>
        game.apply(uci) andThen { case (game, _) =>
          recursiveGamesFromUci(game, rest) map { game :: _ }
        }
    }

  def gameFromUciStrings(
      uciStrings: List[String],
      initialFen: Option[FEN],
      variant: strategygames.dameo.variant.Variant
  ): Validated[String, Game] = {
    val init = makeGame(variant, initialFen)
    val ucis = uciStrings.flatMap(Uci.apply(_))
    if (uciStrings.size != ucis.size) invalid("Invalid Ucis")
    else recursiveGamesFromUci(init, ucis).map(_.last)
  }

  def apply(
      ucis: List[Uci],
      initialFen: Option[FEN],
      variant: strategygames.dameo.variant.Variant
  ): Validated[String, Replay] =
    recursiveReplayFromUci(Replay(makeGame(variant, initialFen)), ucis)

  def plyAtFen(
      actionStrs: ActionStrs,
      initialFen: Option[FEN],
      variant: strategygames.dameo.variant.Variant,
      atFen: FEN
  ): Validated[String, Int] =
    if (Forsyth.<<@(variant, atFen).isEmpty) invalid(s"Invalid FEN $atFen")
    else {

      // we don't want to compare the full move number, to match transpositions
      def truncateFen(fen: FEN) = fen.value.split(' ').take(FEN.fullMoveIndex) mkString " "
      val atFenTruncated        = truncateFen(atFen)
      def compareFen(fen: FEN)  = truncateFen(fen) == atFenTruncated

      def recursivePlyAtFen(sit: Situation, sans: List[San], ply: Int, turn: Int): Validated[String, Int] =
        sans match {
          case Nil         => invalid(s"Can't find $atFenTruncated, reached ply $ply, turn $turn")
          case san :: rest =>
            san(StratSituation.wrap(sit)).map(dameoAction) flatMap { move =>
              val after        = move.situationAfter
              val newPlies     = ply + 1
              val newTurnCount = turn + (if (sit.player != after.player) 1 else 0)
              val fen          = Forsyth >> Game(after, plies = newPlies, turnCount = newTurnCount)
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

  private def makeGame(variant: strategygames.dameo.variant.Variant, initialFen: Option[FEN]): Game = {
    val g = Game(variant.some, initialFen)
    g.copy(
      startedAtPly = g.plies,
      startedAtTurn = g.turnCount
    )
  }
}
