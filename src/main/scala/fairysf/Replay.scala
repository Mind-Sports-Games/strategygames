package strategygames.fairysf

import cats.data.Validated
import cats.data.Validated.{ invalid, valid }
import cats.implicits._
import scalalib.extensions.*

import strategygames.format.pgn.San
import strategygames.fairysf.format.pgn.{ Parser, Reader }
import strategygames.fairysf.format.{ FEN, Forsyth, Uci }
import strategygames.{
  Action => StratAction,
  ActionStrs,
  Drop => StratDrop,
  Move => StratMove,
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
    case m: Move =>
      copy(
        actions = m.applyVariantEffect :: actions,
        state = state.apply(m)
      )
    case d: Drop =>
      copy(
        actions = d :: actions,
        state = state.applyDrop(d)
      )
  }

}

object Replay {

  def apply(game: Game) = new Replay(game, Nil, game)

  def apply(
      actionStrs: ActionStrs,
      initialFen: Option[FEN],
      variant: strategygames.fairysf.variant.Variant
  ): Validated[String, Reader.Result] = {
    val fen                            = initialFen.getOrElse(variant.initialFen)
    val (init, gameWithActions, error) = gameWithActionWhileValid(actionStrs, fen, variant)
    val game                           = gameWithActions.reverse.lastOption.map(_._1).getOrElse(init)

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

  private def fairysfAction(action: StratAction) = action match {
    case StratMove.FairySF(m) => m
    case StratDrop.FairySF(d) => d
    case _                    => sys.error("Invalid fairysf action")
  }

  def replayMove(
      before: Game,
      orig: Pos,
      dest: Pos,
      promotion: String,
      apiPosition: Api.Position,
      uciMoves: List[String]
  ): Move =
    Move(
      piece =
        if (orig == dest)
          // flipello pass. Will remove reference to FlipCounter when refactoring in PLA-309
          Piece(before.situation.player, FlipCounter)
        else before.situation.board.pieces(orig),
      orig = orig,
      dest = dest,
      situationBefore = before.situation,
      after = before.situation.board.copy(
        pieces = apiPosition.pieceMap,
        uciMoves = uciMoves,
        pocketData = apiPosition.pocketData,
        position = apiPosition.some
      ),
      autoEndTurn = true,
      capture = None,
      promotion = promotion match {
        case "" => None
        case _  =>
          Role.promotable(
            before.board.variant.gameFamily,
            before.board.pieces(orig).role.forsyth
          )
      },
      castle = None,
      enpassant = false
    )

  def replayMoveWithoutAPI(
      before: Game,
      piece: Piece,
      orig: Pos,
      dest: Pos,
      promotion: String
  ): Move =
    Move(
      piece = piece,
      orig = orig,
      dest = dest,
      situationBefore = before.situation,
      after = before.situation.board.copy(
        pieces = before.situation.board.pieces - orig + ((dest, piece))
      ),
      autoEndTurn = false,
      capture = None,
      promotion = promotion match {
        case "" => None
        case _  =>
          Role.promotable(
            before.board.variant.gameFamily,
            before.board.pieces(orig).role.forsyth
          )
      },
      castle = None,
      enpassant = false
    )

  def replayDrop(
      before: Game,
      role: Role,
      dest: Pos,
      apiPosition: Api.Position,
      uciMoves: List[String]
  ): Drop = {
    val piece = Piece(before.situation.player, role)
    Drop(
      piece = piece,
      pos = dest,
      situationBefore = before.situation,
      after = before.situation.board.copy(
        pieces = before.situation.board.pieces + ((dest, piece)),
        uciMoves = uciMoves,
        pocketData = apiPosition.pocketData,
        position = apiPosition.some
      ),
      autoEndTurn = true
    )
  }

  private def gameWithActionWhileValid(
      actionStrs: ActionStrs,
      initialFen: FEN,
      variant: strategygames.fairysf.variant.Variant
  ): (Game, List[(Game, Action)], Option[String]) = {

    val init     = makeGame(variant, initialFen.some)
    var state    = init
    var errors   = ""
    var uciMoves = init.situation.board.uciMoves

    def getApiPosition(uciMove: String) = state.board.apiPosition.makeMoves(List(uciMove))

    def replayMoveFromUci(orig: Option[Pos], dest: Option[Pos], promotion: String): (Game, Action) =
      (orig, dest) match {
        case (Some(orig), Some(dest)) => {
          if (variant.switchPlayerAfterMove) {
            val uciMove = s"${orig.key}${dest.key}${promotion}"
            uciMoves = uciMoves :+ uciMove
            val move    = replayMove(state, orig, dest, promotion, getApiPosition(uciMove), uciMoves)
            state = state.apply(move)
            (state, move)
          } else {
            // Amazons
            val move = replayMoveWithoutAPI(state, state.situation.board.pieces(orig), orig, dest, promotion)
            state = state.apply(move)
            (state, move)
          }
        }
        case (orig, dest)             => {
          val uciMove = s"${orig}${dest}${promotion}"
          errors += uciMove + ","
          sys.error(s"Invalid move for replay: ${uciMove}")
        }
      }

    def replayDropFromUci(
        role: Option[Role],
        dest: Option[Pos],
        prevStr: Option[String]
    ): (Game, Action) =
      (role, dest, prevStr) match {
        case (Some(role), Some(dest), None)                                        => {
          val uciDrop = s"${role.forsyth}@${dest.key}"
          uciMoves = uciMoves :+ uciDrop
          val drop    = replayDrop(state, role, dest, getApiPosition(uciDrop), uciMoves)
          state = state.applyDrop(drop)
          (state, drop)
        }
        // Amazons
        case (Some(role), Some(dest), Some(Uci.Move.moveR(prevOrig, prevDest, _))) => {
          val uciMove = s"${prevOrig}${prevDest},${prevDest}${dest.key}"
          uciMoves = uciMoves :+ uciMove
          val drop    = replayDrop(state, role, dest, getApiPosition(uciMove), uciMoves)
          state = state.applyDrop(drop)
          (state, drop)
        }
        case (role, dest, _)                                                       => {
          val uciDrop = s"${role}@${dest}"
          errors += uciDrop + ","
          sys.error(s"Invalid drop for replay: ${uciDrop}")
        }
      }

    def parseFairyUciWithPrevious(fairyUci: String, prevStr: Option[String]): (Game, Action) =
      fairyUci match {
        case Uci.Move.moveR(orig, dest, promotion) =>
          replayMoveFromUci(
            Pos.fromKey(orig),
            Pos.fromKey(dest),
            promotion
          )
        case Uci.Drop.dropR(role, dest)            =>
          replayDropFromUci(
            Role.allByForsyth(init.situation.board.variant.gameFamily).get(role(0)),
            Pos.fromKey(dest),
            prevStr
          )
        case fairyUci: String                      => sys.error(s"Invalid fairyUci for replay: $fairyUci")
      }

    def parseFairyUci(fairyUci: String): (Game, Action) =
      parseFairyUciWithPrevious(fairyUci, None)

    def gameWithActions: List[(Game, Action)] =
      if (!variant.switchPlayerAfterMove) {
        // Amazons. Don't want doubleMoveFormat from Parser, so dont ask for it
        // We flatten actionStrs and handle as non multimove due to needing to merge
        // Amazons Moves and Drops into a single action for the FairySF API
        // If we don't want to flatten then we need to do something like samurai gamelogic
        // where we use startPlayer and activePlayer
        val fairyUcis       = Parser.flatActionStrsToFairyUciMoves(actionStrs.flatten)
        val firstFairyUci   = fairyUcis.headOption.toList
        val pairedFairyUcis = if (fairyUcis == firstFairyUci) List() else fairyUcis.sliding(2)
        (firstFairyUci.map(parseFairyUci)) ::: pairedFairyUcis.flatMap {
          case List(prev, fairyUci) =>
            Some(parseFairyUciWithPrevious(fairyUci, Some(prev)))
          case _                    => None
        }.toList
      } else {
        // We flatten actionStrs and handle as non multimove as these variants are
        // guaranteed to be non-multimove due to FairySF API
        // If we don't want to flatten then we need to do something like samurai gamelogic
        // where we use startPlayer and activePlayer
        Parser.flatActionStrsToFairyUciMoves(actionStrs.flatten).map(parseFairyUci)
      }

    (init, gameWithActions, errors match { case "" => None; case _ => errors.some })
  }

  def gameWithUciWhileValid(
      actionStrs: ActionStrs,
      initialFen: FEN,
      variant: strategygames.fairysf.variant.Variant
  ): (Game, List[(Game, Uci.WithSan)], Option[String]) = {
    val (game, gameWithActions, error) = gameWithActionWhileValid(actionStrs, initialFen, variant)
    (
      game,
      gameWithActions.map { v =>
        {
          val (state, action) = v
          val gf              = state.board.variant.gameFamily
          (state, Uci.WithSan(Uci(gf, action.toUci.uci).get, "NOSAN"))
        }
      },
      error
    )
  }

  private def recursiveSituations(sit: Situation, sans: List[San]): Validated[String, List[Situation]] =
    sans match {
      case Nil         => valid(Nil)
      case san :: rest =>
        san(StratSituation.wrap(sit)).map(fairysfAction) andThen { action =>
          val after = Situation(action.finalizeAfter, !sit.player)
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
        uci(sit) andThen { action =>
          val after = Situation(action.finalizeAfter, !sit.player)
          recursiveSituationsFromUci(after, rest) map { after :: _ }
        }
    }

  private def recursiveReplayFromUci(replay: Replay, ucis: List[Uci]): Validated[String, Replay] =
    ucis match {
      case Nil         => valid(replay)
      case uci :: rest =>
        uci(replay.state.situation) andThen { action =>
          recursiveReplayFromUci(replay addAction action, rest)
        }
    }

  private def initialFenToSituation(
      initialFen: Option[FEN],
      variant: strategygames.fairysf.variant.Variant
  ): Situation = {
    initialFen.flatMap(Forsyth.<<) | Situation(variant)
  } withVariant variant

  def boards(
      actionStrs: ActionStrs,
      initialFen: Option[FEN],
      variant: strategygames.fairysf.variant.Variant
  ): Validated[String, List[Board]] = situations(actionStrs, initialFen, variant) map (_ map (_.board))

  def situations(
      actionStrs: ActionStrs,
      initialFen: Option[FEN],
      variant: strategygames.fairysf.variant.Variant
  ): Validated[String, List[Situation]] = {
    val sit = initialFenToSituation(initialFen, variant)
    // seemingly this isn't used
    Parser.sans(actionStrs.flatten, sit.board.variant) andThen { sans =>
      recursiveSituations(sit, sans.value) map { sit :: _ }
    }
  }

  def boardsFromUci(
      moves: List[Uci],
      initialFen: Option[FEN],
      variant: strategygames.fairysf.variant.Variant
  ): Validated[String, List[Board]] = situationsFromUci(moves, initialFen, variant) map (_ map (_.board))

  def situationsFromUci(
      moves: List[Uci],
      initialFen: Option[FEN],
      variant: strategygames.fairysf.variant.Variant
  ): Validated[String, List[Situation]] = {
    val sit = initialFenToSituation(initialFen, variant)
    recursiveSituationsFromUci(sit, moves) map { sit :: _ }
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
      variant: strategygames.fairysf.variant.Variant
  ): Validated[String, Game] = {
    val init = makeGame(variant, initialFen)
    val ucis = uciStrings.flatMap(uci => Uci.apply(variant.gameFamily, uci))
    if (uciStrings.size != ucis.size) invalid("Invalid Ucis")
    else recursiveGamesFromUci(init, ucis).map(_.last)
  }

  def apply(
      ucis: List[Uci],
      initialFen: Option[FEN],
      variant: strategygames.fairysf.variant.Variant
  ): Validated[String, Replay] =
    recursiveReplayFromUci(Replay(makeGame(variant, initialFen)), ucis)

  def plyAtFen(
      actionStrs: ActionStrs,
      initialFen: Option[FEN],
      variant: strategygames.fairysf.variant.Variant,
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
            san(StratSituation.wrap(sit)).map(fairysfAction) andThen { action =>
              val after        = action.situationAfter
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

  private def makeGame(variant: strategygames.fairysf.variant.Variant, initialFen: Option[FEN]): Game = {
    val g = Game(variant.some, initialFen)
    g.copy(
      startedAtPly = g.plies,
      startedAtTurn = g.turnCount
    )
  }
}
