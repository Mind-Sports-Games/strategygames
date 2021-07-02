package draughts

import format.pdn._
import format.{ FEN, Forsyth, Uci }
import variant.Variant
import scalaz.Validation.FlatMap._
import scalaz.Validation.{ failureNel, success }

case class Replay(setup: DraughtsGame, moves: List[Move], state: DraughtsGame) {

  lazy val chronoMoves = moves.reverse

  def addMove(move: Move, finalSquare: Boolean = false) = copy(
    moves = move.applyVariantEffect :: moves,
    state = state.apply(move, finalSquare)
  )

  def moveAtPly(ply: Int): Option[Move] =
    chronoMoves lift (ply - 1 - setup.startedAtTurn)
}

object Replay {

  def apply(game: DraughtsGame) = new Replay(game, Nil, game)

  def apply(
    moveStrs: Traversable[String],
    initialFen: Option[String],
    variant: Variant,
    finalSquare: Boolean
  ): Valid[Reader.Result] =
    moveStrs.some.filter(_.nonEmpty) toValid "[replay] pdn is empty" flatMap { nonEmptyMoves =>
      Reader.moves(
        nonEmptyMoves,
        Tags(List(
          initialFen map { fen => Tag(_.FEN, fen) },
          variant.some.filterNot(_.standard) map { v => Tag(_.GameType, v.gameType) }
        ).flatten),
        finalSquare
      )
    }

  private def recursiveGames(game: DraughtsGame, sans: List[San]): Valid[List[DraughtsGame]] =
    sans match {
      case Nil => success(Nil)
      case san :: rest => san(game.situation) flatMap { move =>
        val newGame = game.apply(move)
        recursiveGames(newGame, rest) map { newGame :: _ }
      }
    }

  def games(
    moveStrs: Traversable[String],
    initialFen: Option[String],
    variant: Variant
  ): Valid[List[DraughtsGame]] =
    Parser.moves(moveStrs, variant) flatMap { moves =>
      val game = makeGame(variant, initialFen)
      recursiveGames(game, moves.value) map { game :: _ }
    }

  type ErrorMessage = String
  def gameMoveWhileValid(
    moveStrs: Seq[String],
    initialFen: String,
    variant: Variant,
    iteratedCapts: Boolean = false
  ): (DraughtsGame, List[(DraughtsGame, Uci.WithSan)], Option[ErrorMessage]) = {

    def mk(g: DraughtsGame, moves: List[(San, String)], ambs: List[(San, String)]): (List[(DraughtsGame, Uci.WithSan)], Option[ErrorMessage]) = {
      var newAmb = none[(San, String)]
      val res = moves match {
        case (san, sanStr) :: rest =>
          san(g.situation, iteratedCapts, if (ambs.isEmpty) None else ambs.collect({ case (ambSan, ambUci) if ambSan == san => ambUci }).some).fold(
            err => (Nil, err.head.some),
            move => {
              val newGame = g(move)
              val uci = move.toUci
              if (iteratedCapts && move.capture.fold(false)(_.length > 1) && move.situationBefore.ambiguitiesMove(move) > ambs.length + 1)
                newAmb = (san -> uci.uci).some
              mk(newGame, rest, newAmb.fold(ambs)(_ :: ambs)) match {
                case (next, msg) => ((newGame, Uci.WithSan(uci, sanStr)) :: next, msg)
              }
            }
          )
        case _ => (Nil, None)
      }
      if (res._2.isDefined && newAmb.isDefined) mk(g, moves, newAmb.get :: ambs)
      else res
    }

    val init = makeGame(variant, initialFen.some)
    Parser.moves(moveStrs, variant).fold(
      err => List.empty[(DraughtsGame, Uci.WithSan)] -> err.head.some,
      moves => mk(init, moves.value zip moveStrs, Nil)
    ) match {
        case (games, err) => (init, games, err)
      }
  }

  def unambiguousPdnMoves(
    pdnMoves: Seq[String],
    initialFen: Option[String],
    variant: Variant
  ): Valid[List[String]] = {

    def mk(sit: Situation, moves: List[San], ambs: List[(San, String)]): (List[String], Option[ErrorMessage]) = {
      var newAmb = none[(San, String)]
      val res = moves match {
        case san :: rest =>
          san(sit, true, if (ambs.isEmpty) None else ambs.collect({ case (ambSan, ambUci) if ambSan == san => ambUci }).some).fold(
            err => (Nil, err.head.some),
            move => {
              val after = Situation.withColorAfter(move.afterWithLastMove(true), sit.color)
              val ambiguities = if (move.capture.fold(false)(_.length > 1)) move.situationBefore.ambiguitiesMove(move) else 0
              if (ambiguities > ambs.length + 1)
                newAmb = (san -> move.toUci.uci).some
              mk(after, rest, newAmb.fold(ambs)(_ :: ambs)) match {
                case (next, msg) =>
                  val san = if (ambiguities > 1) move.toFullSan else move.toSan
                  (san :: next, msg)
              }
            }
          )
        case _ => (Nil, None)
      }
      if (res._2.isDefined && newAmb.isDefined) mk(sit, moves, newAmb.get :: ambs)
      else res
    }

    val init = initialFenToSituation(initialFen.map(FEN), variant)
    Parser.moves(pdnMoves, variant).fold(
      err => Nil -> err.head.some,
      moves => mk(init, moves.value, Nil)
    ) match {
        case (_, Some(err)) => failureNel(err)
        case (moves, _) => success(moves)
      }
  }

  def exportScanMoves(
    uciMoves: Seq[String],
    initialFen: String,
    variant: Variant,
    debugId: String,
    iteratedCapts: Boolean = false
  ): List[String] = {

    def mk(g: DraughtsGame, moves: List[String], ambs: List[(String, String)]): (List[String], Option[ErrorMessage]) = {
      var newAmb = none[(String, String)]
      val res = moves match {
        case uci :: rest => Uci.Move(uci) match {
          case Some(uciMove) => Std(List(uciMove.orig, uciMove.dest), uciMove.capture.fold(false)(_.nonEmpty)).move(
            g.situation, true,
            if (ambs.isEmpty) None else ambs.collect({ case (ambFrom, ambUci) if ambFrom == uci => ambUci }).some,
            uciMove.capture
          ).fold(
              err => {
                logger.warn(s"exportScanMoves($iteratedCapts) $debugId: $uci -> ${uciMove.orig}${if (uciMove.capture.fold(false)(_.nonEmpty)) "x" else "-"}${uciMove.dest} - error ${err.head}")
                (Nil, err.head.some)
              },
              move => {
                val newGame = g(move, true)
                val scanMove = move.toScanMove
                if (iteratedCapts && move.capture.fold(false)(_.length > 1) && move.situationBefore.ambiguitiesMove(move) > ambs.length + 1)
                  newAmb = (uci -> move.toUci.uci).some
                mk(newGame, rest, newAmb.fold(ambs)(_ :: ambs)) match {
                  case (next, msg) => (scanMove :: next, msg)
                }
              }
            )
          case _ => (Nil, None)
        }
        case _ => (Nil, None)
      }
      if (res._2.isDefined && newAmb.isDefined) mk(g, moves, newAmb.get :: ambs)
      else res
    }

    val init = makeGame(variant, initialFen.some)
    val moveList = if (iteratedCapts) uciMoves.toList
    else uciMoves.foldRight(List[String]()) {
      (move, moves) =>
        moves.headOption match {
          case Some(lastMove) if lastMove.take(2) == move.slice(2, 4) =>
            (move.take(2) + lastMove) :: moves.tail
          case _ => move.take(4) :: moves
        }
    }
    mk(init, moveList, Nil) match {
      case (moves, None) => moves
      case _ => Nil
    }
  }

  private def recursiveUcis(sit: Situation, sans: List[San], finalSquare: Boolean = false): Valid[List[Uci]] =
    sans match {
      case Nil => success(Nil)
      case san :: rest => san(sit, finalSquare) flatMap { move =>
        val after = Situation.withColorAfter(move.afterWithLastMove(finalSquare), sit.color)
        recursiveUcis(after, rest, finalSquare) map { move.toUci :: _ }
      }
    }

  private def recursiveSituations(sit: Situation, sans: List[San], finalSquare: Boolean = false): Valid[List[Situation]] =
    sans match {
      case Nil => success(Nil)
      case san :: rest => san(sit, finalSquare) flatMap { moveOrDrop =>
        val after = Situation.withColorAfter(moveOrDrop.afterWithLastMove(finalSquare), sit.color)
        recursiveSituations(after, rest, finalSquare) map { after :: _ }
      }
    }

  private def recursiveSituationsFromUci(sit: Situation, ucis: List[Uci], finalSquare: Boolean = false): Valid[List[Situation]] =
    ucis match {
      case Nil => success(Nil)
      case uci :: rest => uci(sit, finalSquare) flatMap { move =>
        val after = Situation.withColorAfter(move.afterWithLastMove(finalSquare), sit.color)
        recursiveSituationsFromUci(after, rest, finalSquare) map { after :: _ }
      }
    }

  private def recursiveReplayFromUci(replay: Replay, ucis: List[Uci], finalSquare: Boolean = false): Valid[Replay] =
    ucis match {
      case Nil => success(replay)
      case uci :: rest => uci(replay.state.situation, finalSquare) flatMap { move =>
        recursiveReplayFromUci(replay.addMove(move, finalSquare), rest, finalSquare)
      }
    }

  private def initialFenToSituation(initialFen: Option[FEN], variant: Variant): Situation =
    initialFen.flatMap { fen => Forsyth.<<@(variant, fen.value) } | Situation(variant)

  def boards(
    moveStrs: Traversable[String],
    initialFen: Option[FEN],
    variant: Variant,
    finalSquare: Boolean = false
  ): Valid[List[Board]] = situations(moveStrs, initialFen, variant, finalSquare) map (_ map (_.board))

  def ucis(
    moveStrs: Traversable[String],
    initialSit: Situation,
    finalSquare: Boolean = false
  ): Valid[List[Uci]] =
    Parser.moves(moveStrs, initialSit.board.variant) flatMap { moves =>
      recursiveUcis(initialSit, moves.value, finalSquare)
    }

  def situations(
    moveStrs: Traversable[String],
    initialFen: Option[FEN],
    variant: Variant,
    finalSquare: Boolean = false
  ): Valid[List[Situation]] = {
    val sit = initialFenToSituation(initialFen, variant)
    Parser.moves(moveStrs, sit.board.variant) flatMap { moves =>
      recursiveSituations(sit, moves.value, finalSquare) map { sit :: _ }
    }
  }

  def boardsFromUci(
    moves: List[Uci],
    initialFen: Option[FEN],
    variant: Variant
  ): Valid[List[Board]] = situationsFromUci(moves, initialFen, variant) map (_ map (_.board))

  def situationsFromUci(
    moves: List[Uci],
    initialFen: Option[FEN],
    variant: Variant,
    finalSquare: Boolean = false
  ): Valid[List[Situation]] = {
    val sit = initialFenToSituation(initialFen, variant)
    recursiveSituationsFromUci(sit, moves, finalSquare) map { sit :: _ }
  }

  def apply(
    moves: List[Uci],
    initialFen: Option[String],
    variant: Variant,
    finalSquare: Boolean = false
  ): Valid[Replay] =
    recursiveReplayFromUci(Replay(makeGame(variant, initialFen)), moves, finalSquare)

  def plyAtFen(
    moveStrs: Traversable[String],
    initialFen: Option[String],
    variant: Variant,
    atFen: String
  ): Valid[Int] =
    if (Forsyth.<<@(variant, atFen).isEmpty) failureNel(s"Invalid FEN $atFen")
    else {

      // we don't want to compare the full move number, to match transpositions
      def truncateFen(fen: String) = fen.split(' ').take(4) mkString " "
      val atFenTruncated = truncateFen(atFen)
      def compareFen(fen: String) = truncateFen(fen) == atFenTruncated

      def recursivePlyAtFen(sit: Situation, sans: List[San], ply: Int): Valid[Int] =
        sans match {
          case Nil => failureNel(s"Can't find $atFenTruncated, reached ply $ply")
          case san :: rest => san(sit) flatMap { move =>
            val after = move.finalizeAfter()
            val fen = Forsyth >> DraughtsGame(Situation(after, Color.fromPly(ply)), turns = ply)
            if (compareFen(fen)) scalaz.Success(ply)
            else recursivePlyAtFen(Situation.withColorAfter(after, sit.color), rest, ply + 1)
          }
        }

      val sit = initialFen.flatMap {
        Forsyth.<<@(variant, _)
      } | Situation(variant)

      Parser.moves(moveStrs, sit.board.variant) flatMap { moves =>
        recursivePlyAtFen(sit, moves.value, 1)
      }
    }

  private def makeGame(variant: Variant, initialFen: Option[String]): DraughtsGame = {
    val g = DraughtsGame(variant.some, initialFen)
    g.copy(startedAtTurn = g.turns)
  }
}
