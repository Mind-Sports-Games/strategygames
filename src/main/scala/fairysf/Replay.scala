package strategygames.fairysf

import cats.data.Validated
import cats.data.Validated.{ invalid, valid }
import cats.implicits._

import strategygames.{ Color, Game => StratGame, Move => StratMove, MoveOrDrop, Situation => StratSituation }
import strategygames.format.pgn.{ San, Tag, Tags }
import format.pdn.{ Parser, Reader, Std }
import format.{ FEN, Forsyth, Uci }
import variant.Variant

case class Replay(setup: Game, moves: List[Move], state: Game) {

  lazy val chronoMoves = moves.reverse

  def addMove(move: Move, finalSquare: Boolean = false) = copy(
    moves = move.applyVariantEffect :: moves,
    state = state.apply(move, finalSquare)
  )

  def moveAtPly(ply: Int): Option[Move] =
    chronoMoves lift (ply - 1 - setup.startedAtTurn)
}

object Replay {

  def apply(game: Game) = new Replay(game, Nil, game)

  def apply(
      moveStrs: Iterable[String],
      initialFen: Option[FEN],
      variant: Variant,
      finalSquare: Boolean
  ): Validated[String, Reader.Result] =
    moveStrs.some.filter(_.nonEmpty) toValid "[replay] pdn is empty" andThen { nonEmptyMoves =>
      Reader.moves(
        nonEmptyMoves,
        Tags(
          List(
            initialFen map { fen => Tag(_.FEN, fen.value) },
            variant.some.filterNot(_.shogi) map { v => Tag(_.GameType, v.gameType) }
          ).flatten
        ),
        finalSquare
      )
    }

  // TODO: because this is primarily used in a Validation context, we should be able to
  //       return something that's runtime safe as well.
  def draughtsMove(moveOrDrop: MoveOrDrop) = moveOrDrop match {
    case Left(StratMove.Draughts(m)) => m
    case _                           => sys.error("Invalid draughts move")
  }

  private def recursiveGames(game: Game, sans: List[San]): Validated[String, List[Game]] =
    ???
    //sans match {
    //  case Nil => valid(Nil)
    //  case san :: rest =>
    //    san(StratSituation.wrap(game.situation)) flatMap { move =>
    //      val newGame = StratGame.wrap(game)(move).toDraughts
    //      recursiveGames(newGame, rest) map { newGame :: _ }
    //    }
    //}

  def games(
      moveStrs: Iterable[String],
      initialFen: Option[FEN],
      variant: Variant
  ): Validated[String, List[Game]] =
    Parser.moves(moveStrs, variant) andThen { moves =>
      val game = makeGame(variant, initialFen)
      recursiveGames(game, moves.value) map { game :: _ }
    }

  type ErrorMessage = String
  def gameMoveWhileValid(
      moveStrs: Seq[String],
      initialFen: FEN,
      variant: Variant,
      iteratedCapts: Boolean = false
  ): (Game, List[(Game, Uci.WithSan)], Option[ErrorMessage]) = {

    def mk(
        g: Game,
        moves: List[(San, String)],
        ambs: List[(San, String)]
    ): (List[(Game, Uci.WithSan)], Option[ErrorMessage]) = {
      var newAmb = none[(San, String)]
      val res: (List[(Game, Uci.WithSan)], Option[ErrorMessage]) = moves match {
        case (san, sanStr) :: rest =>
          ???
          //san(
          //  StratSituation.wrap(g.situation),
          //  iteratedCapts,
          //  if (ambs.isEmpty) None
          //  else ambs.collect({ case (ambSan, ambUci) if ambSan == san => ambUci }).some
          //).fold(
          //  err => (Nil, err.some),
          //  moveOrDrop => {
          //    val move = moveOrDrop match {
          //      case Left(StratMove.Draughts(m)) => m
          //      case _                           => sys.error("Invalid draughts move")
          //    }
          //    val newGame = g(move)
          //    val uci     = move.toUci
          //    if (
          //      iteratedCapts && move.capture.fold(false)(_.length > 1) && move.situationBefore
          //        .ambiguitiesMove(move) > ambs.length + 1
          //    )
          //      newAmb = (san -> uci.uci).some
          //    mk(newGame, rest, newAmb.fold(ambs)(_ :: ambs)) match {
          //      case (next, msg) => ((newGame, Uci.WithSan(uci, sanStr)) :: next, msg)
          //    }
          //  }
          //)
        case _ => (Nil, None)
      }
      if (res._2.isDefined && newAmb.isDefined) mk(g, moves, newAmb.get :: ambs)
      else res
    }

    val init = makeGame(variant, initialFen.some)
    Parser
      .moves(moveStrs, variant)
      .fold(
        err => List.empty[(Game, Uci.WithSan)] -> err.some,
        moves => mk(init, moves.value zip moveStrs, Nil)
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
      var newAmb = none[(San, String)]
      val res: (List[String], Option[ErrorMessage]) = moves match {
        case san :: rest =>
          ???
          //san(
          //  StratSituation.wrap(sit),
          //  true,
          //  if (ambs.isEmpty) None
          //  else ambs.collect({ case (ambSan, ambUci) if ambSan == san => ambUci }).some
          //).map(draughtsMove)
          //  .fold(
          //    err => (Nil, err.some),
          //    move => {
          //      val after = Situation.withColorAfter(move.afterWithLastMove(true), sit.color)
          //      val ambiguities =
          //        if (move.capture.fold(false)(_.length > 1)) move.situationBefore.ambiguitiesMove(move)
          //        else 0
          //      if (ambiguities > ambs.length + 1)
          //        newAmb = (san -> move.toUci.uci).some
          //      mk(after, rest, newAmb.fold(ambs)(_ :: ambs)) match {
          //        case (next, msg) =>
          //          val san = if (ambiguities > 1) move.toFullSan else move.toSan
          //          (san :: next, msg)
          //      }
          //    }
          //  )
        case _ => (Nil, None)
      }
      if (res._2.isDefined && newAmb.isDefined) mk(sit, moves, newAmb.get :: ambs)
      else res
    }

    val init = initialFenToSituation(initialFen, variant)
    Parser
      .moves(pdnMoves, variant)
      .fold(
        err => Nil -> err.head.some,
        moves => mk(init, moves.value, Nil)
      ) match {
      case (_, Some(_)) => None
      case (moves, _)     => Option(moves)
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
        g: Game,
        moves: List[String],
        ambs: List[(String, String)]
    ): (List[String], Option[ErrorMessage]) = {
      var newAmb = none[(String, String)]
      val res: (List[String], Option[ErrorMessage]) = moves match {
        case uci :: rest =>
          Uci.Move(uci) match {
            case Some(uciMove) =>
              Std(List(uciMove.orig, uciMove.dest), uciMove.capture.fold(false)(_.nonEmpty))
                .move(
                  g.situation,
                  true,
                  if (ambs.isEmpty) None
                  else ambs.collect({ case (ambFrom, ambUci) if ambFrom == uci => ambUci }).some,
                  uciMove.capture
                )
                .fold(
                  err => {
                    // TODO: Warning?
                    //logger.warn(s"exportScanMoves($iteratedCapts) $debugId: $uci -> ${uciMove.orig}${if (uciMove.capture.fold(false)(_.nonEmpty)) "x" else "-"}${uciMove.dest} - error ${err.head}")
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
            case _ => (Nil, None)
          }
        case _ => (Nil, None)
      }
      if (res._2.isDefined && newAmb.isDefined) mk(g, moves, newAmb.get :: ambs)
      else res
    }

    val init = makeGame(variant, initialFen.some)
    val moveList =
      if (iteratedCapts) uciMoves.toList
      else
        uciMoves.foldRight(List[String]()) { (move, moves) =>
          moves.headOption match {
            case Some(lastMove) if lastMove.take(2) == move.slice(2, 4) =>
              (move.take(2) + lastMove) :: moves.tail
            case _ => move.take(4) :: moves
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
      case Nil => valid(Nil)
      case san :: rest =>
        ???
        //san(StratSituation.wrap(sit), finalSquare).map(draughtsMove) flatMap { move =>
        //  val after = Situation.withColorAfter(move.afterWithLastMove(finalSquare), sit.color)
        //  recursiveUcis(after, rest, finalSquare) map { move.toUci :: _ }
        //}
    }

  private def recursiveSituations(
      sit: Situation,
      sans: List[San],
      finalSquare: Boolean = false
  ): Validated[String, List[Situation]] =
    sans match {
      case Nil => valid(Nil)
      case san :: rest =>
        ???
        //san(StratSituation.wrap(sit), finalSquare).map(draughtsMove) flatMap { move =>
        //  val after = Situation.withColorAfter(move.afterWithLastMove(finalSquare), sit.color)
        //  recursiveSituations(after, rest, finalSquare) map { after :: _ }
        //}
    }

  private def recursiveSituationsFromUci(
      sit: Situation,
      ucis: List[Uci],
      finalSquare: Boolean = false
  ): Validated[String, List[Situation]] =
    ucis match {
      case Nil => valid(Nil)
      case uci :: rest =>
        uci(sit, finalSquare) andThen { move =>
          val after = Situation.withColorAfter(move.afterWithLastMove(finalSquare), sit.color)
          recursiveSituationsFromUci(after, rest, finalSquare) map { after :: _ }
        }
    }

  private def recursiveReplayFromUci(
      replay: Replay,
      ucis: List[Uci],
      finalSquare: Boolean = false
  ): Validated[String, Replay] =
    ucis match {
      case Nil => valid(replay)
      case uci :: rest =>
        uci(replay.state.situation, finalSquare) andThen { move =>
          recursiveReplayFromUci(replay.addMove(move, finalSquare), rest, finalSquare)
        }
    }

  private def initialFenToSituation(initialFen: Option[FEN], variant: Variant): Situation =
    initialFen.flatMap { fen => Forsyth.<<@(variant, fen) } | Situation(variant)

  def boards(
      moveStrs: Iterable[String],
      initialFen: Option[FEN],
      variant: Variant,
      finalSquare: Boolean = false
  ): Validated[String, List[Board]] =
    situations(moveStrs, initialFen, variant, finalSquare) map (_ map (_.board))

  def ucis(
      moveStrs: Iterable[String],
      initialSit: Situation,
      finalSquare: Boolean = false
  ): Validated[String, List[Uci]] =
    Parser.moves(moveStrs, initialSit.board.variant) andThen { moves =>
      recursiveUcis(initialSit, moves.value, finalSquare)
    }

  def situations(
      moveStrs: Iterable[String],
      initialFen: Option[FEN],
      variant: Variant,
      finalSquare: Boolean = false
  ): Validated[String, List[Situation]] = {
    val sit = initialFenToSituation(initialFen, variant)
    Parser.moves(moveStrs, sit.board.variant) andThen { moves =>
      recursiveSituations(sit, moves.value, finalSquare) map { sit :: _ }
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

  def apply(
      moves: List[Uci],
      initialFen: Option[FEN],
      variant: Variant,
      finalSquare: Boolean = false
  ): Validated[String, Replay] =
    recursiveReplayFromUci(Replay(makeGame(variant, initialFen)), moves, finalSquare)

  def plyAtFen(
      moveStrs: Iterable[String],
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

      def recursivePlyAtFen(sit: Situation, sans: List[San], ply: Int): Validated[String, Int] =
        sans match {
          case Nil => invalid(s"Can't find $atFenTruncated, reached ply $ply")
          case san :: rest =>
            ???
            //san(StratSituation.wrap(sit)).map(draughtsMove) flatMap { move =>
            //  val after = move.finalizeAfter()
            //  val fen   = Forsyth >> Game(Situation(after, Color.fromPly(ply)), turns = ply)
            //  if (compareFen(fen)) Validated.valid(ply)
            //  else recursivePlyAtFen(Situation.withColorAfter(after, sit.color), rest, ply + 1)
            //}
        }

      val sit = initialFen.flatMap {
        Forsyth.<<@(variant, _)
      } | Situation(variant)

      Parser.moves(moveStrs, sit.board.variant) andThen { moves =>
        recursivePlyAtFen(sit, moves.value, 1)
      }
    }

  private def makeGame(variant: Variant, initialFen: Option[FEN]): Game = {
    val g = Game(variant.some, initialFen)
    g.copy(startedAtTurn = g.turns)
  }
}
