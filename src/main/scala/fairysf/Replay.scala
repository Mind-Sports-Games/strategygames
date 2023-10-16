package strategygames.fairysf

import cats.data.Validated
import cats.data.Validated.{ invalid, valid }
import cats.implicits._

import strategygames.Player
import strategygames.format.pgn.San
import strategygames.fairysf.format.pgn.{ Parser, Reader }
import strategygames.fairysf.format.{ FEN, Forsyth, Uci }
import strategygames.{
  Action => StratAction,
  Drop => StratDrop,
  Move => StratMove,
  Situation => StratSituation
}

case class Replay(setup: Game, moves: List[Action], state: Game) {

  lazy val chronoMoves = moves.reverse

  def addMove(action: Action) = action match {
    case m: Move =>
      copy(
        moves = m.applyVariantEffect :: moves,
        state = state.apply(m)
      )
    case d: Drop =>
      copy(
        moves = d :: moves,
        state = state.applyDrop(d)
      )
  }

  def moveAtPly(ply: Int): Option[Action] =
    chronoMoves lift (ply - 1 - setup.startedAtTurn)
}

object Replay {

  def apply(game: Game) = new Replay(game, Nil, game)

  def apply(
      moveStrs: Iterable[String],
      initialFen: Option[FEN],
      variant: strategygames.fairysf.variant.Variant
  ): Validated[String, Reader.Result] = {
    val fen                  = initialFen.getOrElse(variant.initialFen)
    val (init, moves, error) = gameMoveWhileValid__impl(moveStrs.toSeq, fen, variant)
    val game                 = moves.reverse.last._1
    error match {
      case None      => Validated.valid(Reader.Result.Complete(new Replay(init, moves.reverse.map(_._2), game)))
      case Some(msg) => Validated.invalid(msg)
    }
  }

  def fairysfAction(action: StratAction) = action match {
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
      )
    )
  }

  private def gameMoveWhileValid__impl(
      moveStrs: Seq[String],
      initialFen: FEN,
      variant: strategygames.fairysf.variant.Variant
  ): (Game, List[(Game, Action)], Option[String]) = {

    val init     = makeGame(variant, initialFen.some)
    var state    = init
    var uciMoves = init.situation.board.uciMoves
    var errors   = ""

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

    def parseActionWithPrevious(moveStr: String, prevStr: Option[String]): (Game, Action) =
      moveStr match {
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
        case moveStr: String                       => sys.error(s"Invalid action for replay: $moveStr")
      }

    def parseAction(moveStr: String): (Game, Action) =
      parseActionWithPrevious(moveStr, None)

    def moves: List[(Game, Action)] =
      if (!variant.switchPlayerAfterMove) {
        // Amazons. Don't want doubleMoveFormat from Parser, so dont ask for it
        val moves       = Parser.pgnMovesToUciMoves(moveStrs)
        val firstMove   = moves.headOption.toList
        val pairedMoves = if (moves == firstMove) List() else moves.sliding(2)
        (firstMove.map(parseAction)) ::: pairedMoves.map { case List(prev, move) =>
          parseActionWithPrevious(move, Some(prev))
        }.toList
      } else {
        Parser.pgnMovesToUciMoves(moveStrs).map(parseAction)
      }

    (init, moves, errors match { case "" => None; case _ => errors.some })
  }

  def gameMoveWhileValid(
      moveStrs: Seq[String],
      initialFen: FEN,
      variant: strategygames.fairysf.variant.Variant
  ): (Game, List[(Game, Uci.WithSan)], Option[String]) = {
    val (game, moves, error) = gameMoveWhileValid__impl(moveStrs, initialFen, variant)
    (
      game,
      moves.map { v =>
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
        san(StratSituation.wrap(sit)).map(fairysfAction) flatMap { action =>
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
          recursiveReplayFromUci(replay addMove action, rest)
        }
    }

  private def initialFenToSituation(
      initialFen: Option[FEN],
      variant: strategygames.fairysf.variant.Variant
  ): Situation = {
    initialFen.flatMap(Forsyth.<<) | Situation(variant)
  } withVariant variant

  def boards(
      moveStrs: Iterable[String],
      initialFen: Option[FEN],
      variant: strategygames.fairysf.variant.Variant
  ): Validated[String, List[Board]] = situations(moveStrs, initialFen, variant) map (_ map (_.board))

  def situations(
      moveStrs: Iterable[String],
      initialFen: Option[FEN],
      variant: strategygames.fairysf.variant.Variant
  ): Validated[String, List[Situation]] = {
    val sit = initialFenToSituation(initialFen, variant)
    Parser.moves(moveStrs, sit.board.variant) andThen { moves =>
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
      moves: List[Uci],
      initialFen: Option[FEN],
      variant: strategygames.fairysf.variant.Variant
  ): Validated[String, Replay] =
    recursiveReplayFromUci(Replay(makeGame(variant, initialFen)), moves)

  def plyAtFen(
      moveStrs: Iterable[String],
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

      def recursivePlyAtFen(sit: Situation, sans: List[San], ply: Int): Validated[String, Int] =
        sans match {
          case Nil         => invalid(s"Can't find $atFenTruncated, reached ply $ply")
          case san :: rest =>
            san(StratSituation.wrap(sit)).map(fairysfAction) flatMap { action =>
              val after = action.finalizeAfter
              val fen   =
                Forsyth >> Game(Situation(after, Player.fromPly(ply, variant.plysPerTurn)), turns = ply)
              if (compareFen(fen)) Validated.valid(ply)
              else recursivePlyAtFen(Situation(after, !sit.player), rest, ply + 1)
            }
        }

      val sit = initialFen.flatMap {
        Forsyth.<<@(variant, _)
      } | Situation(variant)

      Parser.moves(moveStrs, sit.board.variant) andThen { moves =>
        recursivePlyAtFen(sit, moves.value, 1)
      }
    }

  private def makeGame(variant: strategygames.fairysf.variant.Variant, initialFen: Option[FEN]): Game = {
    val g = Game(variant.some, initialFen)
    g.copy(startedAtTurn = g.turns)
  }
}
