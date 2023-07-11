package strategygames.fairysf

import cats.data.Validated
import cats.data.Validated.{ invalid, valid }
import cats.implicits._

import strategygames.format.pgn.San
import strategygames.fairysf.format.pgn.{ Parser, Reader }
import strategygames.fairysf.format.{ FEN, Forsyth, Uci }
import strategygames.{ Actions, Situation => StratSituation }

case class Replay(setup: Game, plies: List[MoveOrDrop], state: Game) {

  lazy val chronoPlies = plies.reverse

  lazy val chronoActions: List[List[MoveOrDrop]] =
    chronoPlies
      .drop(1)
      .foldLeft(List(chronoPlies.take(1))) { case (turn, mod) =>
        if (
          turn.head.head.fold(_.situationBefore.player, _.situationBefore.player) != mod.fold(
            _.situationBefore.player,
            _.situationBefore.player
          )
        ) {
          List(mod) +: turn
        } else {
          (turn.head :+ mod) +: turn.tail
        }
      }
      .reverse

  def addPly(moveOrDrop: MoveOrDrop) =
    copy(
      plies = moveOrDrop.left.map(_.applyVariantEffect) :: plies,
      state = moveOrDrop.fold(state.apply, state.applyDrop)
    )

}

object Replay {

  def apply(game: Game) = new Replay(game, Nil, game)

  def apply(
      actions: Actions,
      initialFen: Option[FEN],
      variant: strategygames.fairysf.variant.Variant
  ): Validated[String, Reader.Result] = {
    val fen                 = initialFen.getOrElse(variant.initialFen)
    val (init, plys, error) = gameActionWhileValid(actions, fen, variant)
    val game                = plys.reverse.last._1
    error match {
      case None      => Validated.valid(Reader.Result.Complete(new Replay(init, plys.reverse.map(_._2), game)))
      case Some(msg) => Validated.invalid(msg)
    }
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
      piece = before.situation.board.pieces(orig),
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
        position = apiPosition.some
      ),
      autoEndTurn = true
    )
  }

  private def gameActionWhileValid(
      actions: Actions,
      initialFen: FEN,
      variant: strategygames.fairysf.variant.Variant
  ): (Game, List[(Game, MoveOrDrop)], Option[String]) = {

    val init     = makeGame(variant, initialFen.some)
    var state    = init
    var uciMoves = init.situation.board.uciMoves
    var errors   = ""

    def getApiPosition(uciMove: String) = state.board.apiPosition.makeMoves(List(uciMove))

    def replayMoveFromUci(orig: Option[Pos], dest: Option[Pos], promotion: String): (Game, MoveOrDrop) =
      (orig, dest) match {
        case (Some(orig), Some(dest)) => {
          if (variant.switchPlayerAfterMove) {
            val uciMove = s"${orig.key}${dest.key}${promotion}"
            uciMoves = uciMoves :+ uciMove
            val move    = replayMove(state, orig, dest, promotion, getApiPosition(uciMove), uciMoves)
            state = state.apply(move)
            (state, move.asLeft)
          } else {
            // Amazons
            val move = replayMoveWithoutAPI(state, state.situation.board.pieces(orig), orig, dest, promotion)
            state = state.apply(move)
            (state, move.asLeft)
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
    ): (Game, MoveOrDrop) =
      (role, dest, prevStr) match {
        case (Some(role), Some(dest), None)                                        => {
          val uciDrop = s"${role.forsyth}@${dest.key}"
          uciMoves = uciMoves :+ uciDrop
          val drop    = replayDrop(state, role, dest, getApiPosition(uciDrop), uciMoves)
          state = state.applyDrop(drop)
          (state, drop.asRight)
        }
        // Amazons
        case (Some(role), Some(dest), Some(Uci.Move.moveR(prevOrig, prevDest, _))) => {
          val uciMove = s"${prevOrig}${prevDest},${prevDest}${dest.key}"
          uciMoves = uciMoves :+ uciMove
          val drop    = replayDrop(state, role, dest, getApiPosition(uciMove), uciMoves)
          state = state.applyDrop(drop)
          (state, drop.asRight)
        }
        case (role, dest, _)                                                       => {
          val uciDrop = s"${role}@${dest}"
          errors += uciDrop + ","
          sys.error(s"Invalid drop for replay: ${uciDrop}")
        }
      }

    def parseMoveOrDropWithPrevious(action: String, prevStr: Option[String]): (Game, MoveOrDrop) =
      action match {
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
        case action: String                        => sys.error(s"Invalid moveordrop for replay: $action")
      }

    def parseMoveOrDrop(action: String): (Game, MoveOrDrop) =
      parseMoveOrDropWithPrevious(action, None)

    def plys: List[(Game, MoveOrDrop)] =
      if (!variant.switchPlayerAfterMove) {
        // Amazons. Don't want doubleMoveFormat from Parser, so dont ask for it
        // We flatten actions and handle as non multimove due to needing to merge
        // Amazons Moves and Drops into a single action for the FairySF API
        val plys       = Parser.pgnMovesToUciMoves(actions.flatten)
        val firstPly   = plys.headOption.toList
        val pairedPlys = if (plys == firstPly) List() else plys.sliding(2)
        (firstPly.map(parseMoveOrDrop)) ::: pairedPlys.map { case List(prev, ply) =>
          parseMoveOrDropWithPrevious(ply, Some(prev))
        }.toList
      } else {
        // We flatten actions and handle as non multimove as these variants are
        // guaranteed to be non-multimove due to FairySF API
        Parser.pgnMovesToUciMoves(actions.flatten).map(parseMoveOrDrop)
      }

    (init, plys, errors match { case "" => None; case _ => errors.some })
  }

  def gamePlyWhileValid(
      actions: Actions,
      initialFen: FEN,
      variant: strategygames.fairysf.variant.Variant
  ): (Game, List[(Game, Uci.WithSan)], Option[String]) = {
    val (game, plys, error) = gameActionWhileValid(actions, initialFen, variant)
    (
      game,
      plys.map { v =>
        {
          val (state, moveOrDrop) = v
          val gf                  = state.board.variant.gameFamily
          (state, Uci.WithSan(Uci(gf, moveOrDrop.fold(_.toUci.uci, _.toUci.uci)).get, "NOSAN"))
        }
      },
      error
    )
  }

  private def recursiveSituations(sit: Situation, sans: List[San]): Validated[String, List[Situation]] =
    sans match {
      case Nil         => valid(Nil)
      case san :: rest =>
        san(StratSituation.wrap(sit)) flatMap { moveOrDrop =>
          val after = Situation(
            moveOrDrop.fold(m => m.finalizeAfter().toFairySF, d => d.finalizeAfter.toFairySF),
            !sit.player
          )
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
        uci(sit) andThen { moveOrDrop =>
          val after = Situation(moveOrDrop.fold(_.finalizeAfter, _.finalizeAfter), !sit.player)
          recursiveSituationsFromUci(after, rest) map { after :: _ }
        }
    }

  private def recursiveReplayFromUci(replay: Replay, ucis: List[Uci]): Validated[String, Replay] =
    ucis match {
      case Nil         => valid(replay)
      case uci :: rest =>
        uci(replay.state.situation) andThen { ply =>
          recursiveReplayFromUci(replay addPly ply, rest)
        }
    }

  private def initialFenToSituation(
      initialFen: Option[FEN],
      variant: strategygames.fairysf.variant.Variant
  ): Situation = {
    initialFen.flatMap(Forsyth.<<) | Situation(variant)
  } withVariant variant

  def boards(
      actions: Actions,
      initialFen: Option[FEN],
      variant: strategygames.fairysf.variant.Variant
  ): Validated[String, List[Board]] = situations(actions, initialFen, variant) map (_ map (_.board))

  def situations(
      actions: Actions,
      initialFen: Option[FEN],
      variant: strategygames.fairysf.variant.Variant
  ): Validated[String, List[Situation]] = {
    val sit = initialFenToSituation(initialFen, variant)
    Parser.moves(actions.flatten, sit.board.variant) andThen { moves =>
      recursiveSituations(sit, moves.value) map { sit :: _ }
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

  def apply(
      plies: List[Uci],
      initialFen: Option[FEN],
      variant: strategygames.fairysf.variant.Variant
  ): Validated[String, Replay] =
    recursiveReplayFromUci(Replay(makeGame(variant, initialFen)), plies)

  def plyAtFen(
      actions: Actions,
      initialFen: Option[FEN],
      variant: strategygames.fairysf.variant.Variant,
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
            san(StratSituation.wrap(sit)) flatMap { moveOrDrop =>
              val after        = moveOrDrop.fold(m => m.situationAfter.toFairySF, d => d.situationAfter.toFairySF)
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

      Parser.moves(actions.flatten, sit.board.variant) andThen { moves =>
        recursivePlyAtFen(sit, moves.value, 0, 0)
      }
    }

  private def makeGame(variant: strategygames.fairysf.variant.Variant, initialFen: Option[FEN]): Game = {
    val g = Game(variant.some, initialFen)
    g.copy(
      startedAtTurn = g.currentTurnCount,
      startPlayer = g.situation.player
    )
  }
}
