package strategygames.backgammon

import cats.data.Validated
import cats.data.Validated.{ invalid, valid }
import cats.implicits._

import strategygames.format.pgn.San
import strategygames.backgammon.format.pgn.{ Parser, Reader }
import strategygames.format.pgn.{ Tag, Tags }
import strategygames.backgammon.format.{ FEN, Forsyth, Uci }
import strategygames.{
  Action => StratAction,
  ActionStrs,
  DiceRoll => StratDiceRoll,
  Drop => StratDrop,
  EndTurn => StratEndTurn,
  Lift => StratLift,
  Move => StratMove,
  Player,
  Situation => StratSituation
}

case class Replay(setup: Game, actions: List[Action], state: Game) {

  lazy val chronoPlies = actions.reverse

  lazy val chronoActions: List[List[Action]] =
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

  def addAction(action: Action) = action match {
    case m: Move      =>
      copy(
        actions = m :: actions,
        state = state.apply(m)
      )
    case d: Drop      =>
      copy(
        actions = d :: actions,
        state = state.applyDrop(d)
      )
    case l: Lift      =>
      copy(
        actions = l :: actions,
        state = state.applyLift(l)
      )
    case dr: DiceRoll =>
      copy(
        actions = dr :: actions,
        state = state.applyDiceRoll(dr)
      )
    case et: EndTurn  =>
      copy(
        actions = et :: actions,
        state = state.applyEndTurn(et)
      )
  }

}

object Replay {

  def apply(game: Game) = new Replay(game, Nil, game)

  def apply(
      actionStrs: ActionStrs,
      initialFen: Option[FEN],
      variant: strategygames.backgammon.variant.Variant
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
  private def backgammonAction(action: StratAction) = action match {
    case StratMove.Backgammon(m)      => m
    case StratDrop.Backgammon(d)      => d
    case StratLift.Backgammon(l)      => l
    case StratDiceRoll.Backgammon(dr) => dr
    case StratEndTurn.Backgammon(et)  => et
    case _                            => sys.error("Invalid backgammon action")
  }

  def replayMove(before: Game, orig: Pos, dest: Pos): Move =
    Move(
      piece = before.situation.board.pieces(orig)._1,
      orig = orig,
      dest = dest,
      situationBefore = before.situation,
      after = before.situation.board.variant.boardAfter(
        before.situation,
        Some(orig),
        Some(dest),
        (orig.index - dest.index).abs
      )
      // capture = before.situation.board.piecesOf(!before.situation.player).get(dest).map(_ => dest)
    )

  def replayDrop(before: Game, role: Role, dest: Pos): Drop =
    Drop(
      piece = Piece(before.situation.player, role),
      pos = dest,
      situationBefore = before.situation,
      after = before.situation.board.variant.boardAfter(
        before.situation,
        None,
        Some(dest),
        Pos.barIndex(before.situation.player) + (dest.index * Pos.indexDirection(before.situation.player))
      )
    )

  private def liftDistance(orig: Pos, player: Player): Int =
    Pos.barIndex(player) + (orig.index * Pos.indexDirection(player))

  def replayLift(before: Game, orig: Pos): Lift =
    Lift(
      pos = orig,
      situationBefore = before.situation,
      after = before.situation.board.variant.boardAfter(
        before.situation,
        Some(orig),
        None,
        before.situation.board.unusedDice.filter(_ >= liftDistance(orig, before.situation.player)).min
      )
    )

  def replayDiceRoll(before: Game, dice: List[Int]): DiceRoll = {
    DiceRoll(
      dice,
      situationBefore = before.situation,
      after = before.situation.board.setDice(dice)
    )
  }

  def replayEndTurn(before: Game): EndTurn = {
    EndTurn(
      situationBefore = before.situation,
      after = before.situation.board
    )
  }

  private def gameWithActionWhileValid(
      actionStrs: ActionStrs,
      initialFen: FEN,
      variant: strategygames.backgammon.variant.Variant
  ): (Game, List[(Game, Action)], Option[String]) = {
    val init   = makeGame(variant, initialFen.some)
    var state  = init
    var errors = ""

    def replayMoveFromUci(orig: Option[Pos], dest: Option[Pos]): (Game, Move) =
      (orig, dest) match {
        case (Some(orig), Some(dest)) => {
          val move = replayMove(state, orig, dest)
          state = state(move)
          (state, move)
        }
        case (orig, dest)             => {
          val uciMove = s"${orig}${dest}"
          errors += uciMove + ","
          sys.error(s"Invalid move for replay: ${uciMove}")
        }
      }

    def replayDropFromUci(role: Option[Role], dest: Option[Pos]): (Game, Drop) =
      (role, dest) match {
        case (Some(role), Some(dest)) => {
          val drop = replayDrop(state, role, dest)
          state = state.applyDrop(drop)
          (state, drop)
        }
        case (role, dest)             => {
          val uciDrop = s"${role}@${dest}"
          errors += uciDrop + ","
          sys.error(s"Invalid drop for replay: ${uciDrop}")
        }
      }

    def replayLiftFromUci(orig: Option[Pos]): (Game, Lift) =
      orig match {
        case Some(orig) => {
          val lift = replayLift(state, orig)
          state = state.applyLift(lift)
          (state, lift)
        }
        case orig       => {
          val uciLift = s"^${orig}"
          errors += uciLift + ","
          sys.error(s"Invalid lift for replay: ${uciLift}")
        }
      }

    def replayDiceRollFromUci(dice: List[Int]): (Game, DiceRoll) = {
      val diceRoll = replayDiceRoll(state, dice)
      state = state.applyDiceRoll(diceRoll)
      (state, diceRoll)
    }

    def replayEndTurnFromUci(): (Game, Action) = {
      val endTurn = replayEndTurn(state)
      state = state.applyEndTurn(endTurn)
      (state, endTurn)
    }

    val gameWithActions: List[(Game, Action)] =
      // can flatten as specific EndTurn action marks turn end
      actionStrs.flatten.toList.map {
        case Uci.Move.moveR(orig, dest) =>
          replayMoveFromUci(
            Pos.fromKey(orig),
            Pos.fromKey(dest)
          )
        case Uci.Drop.dropR(role, dest) =>
          replayDropFromUci(
            Role.allByForsyth(init.situation.board.variant.gameFamily).get(role(0)),
            Pos.fromKey(dest)
          )
        case Uci.Lift.liftR(orig)       =>
          replayLiftFromUci(Pos.fromKey(orig))
        case Uci.DiceRoll.diceRollR(dr) =>
          replayDiceRollFromUci(Uci.DiceRoll.fromStrings(dr).dice)
        case Uci.EndTurn.endTurnR()     =>
          replayEndTurnFromUci()
        case (action: String)           =>
          sys.error(s"Invalid action for replay: $action")
      }

    (init, gameWithActions, errors match { case "" => None; case _ => errors.some })
  }

  def gameWithUciWhileValid(
      actionStrs: ActionStrs,
      initialFen: FEN,
      variant: strategygames.backgammon.variant.Variant
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
        san(StratSituation.wrap(sit)).map(backgammonAction) flatMap { move =>
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
      variant: strategygames.backgammon.variant.Variant
  ): Situation = {
    initialFen.flatMap(Forsyth.<<) | Situation(strategygames.backgammon.variant.Variant.default)
  } withVariant variant

  def boards(
      actionStrs: ActionStrs,
      initialFen: Option[FEN],
      variant: strategygames.backgammon.variant.Variant
  ): Validated[String, List[Board]] = situations(actionStrs, initialFen, variant) map (_ map (_.board))

  def situations(
      actionStrs: ActionStrs,
      initialFen: Option[FEN],
      variant: strategygames.backgammon.variant.Variant
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
      variant: strategygames.backgammon.variant.Variant
  ): Validated[String, List[Board]] = situationsFromUci(ucis, initialFen, variant) map (_ map (_.board))

  def situationsFromUci(
      ucis: List[Uci],
      initialFen: Option[FEN],
      variant: strategygames.backgammon.variant.Variant
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
      variant: strategygames.backgammon.variant.Variant
  ): Validated[String, Game] = {
    val init = makeGame(variant, initialFen)
    val ucis = uciStrings.flatMap(Uci.apply(_))
    if (uciStrings.size != ucis.size) invalid("Invalid Ucis")
    else recursiveGamesFromUci(init, ucis).map(_.last)
  }

  def apply(
      ucis: List[Uci],
      initialFen: Option[FEN],
      variant: strategygames.backgammon.variant.Variant
  ): Validated[String, Replay] =
    recursiveReplayFromUci(Replay(makeGame(variant, initialFen)), ucis)

  def plyAtFen(
      actionStrs: ActionStrs,
      initialFen: Option[FEN],
      variant: strategygames.backgammon.variant.Variant,
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
            san(StratSituation.wrap(sit)).map(backgammonAction) flatMap { move =>
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

  private def makeGame(variant: strategygames.backgammon.variant.Variant, initialFen: Option[FEN]): Game = {
    val g = Game(variant.some, initialFen)
    g.copy(
      startedAtPly = g.plies,
      startedAtTurn = g.turnCount
    )
  }
}
