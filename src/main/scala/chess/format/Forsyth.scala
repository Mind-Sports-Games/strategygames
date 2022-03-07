package strategygames.chess.format

import cats.implicits._
import strategygames.{ Player, Pocket, Pockets }
import strategygames.chess._
import strategygames.chess.variant.{ Standard, Variant }

/** Transform a game to standard Forsyth Edwards Notation
  * http://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation
  *
  * Crazyhouse & Threecheck extensions:
  * https://github.com/ddugovic/Stockfish/wiki/FEN-extensions
  * http://scidb.sourceforge.net/help/en/FEN.html#ThreeCheck
  */
object Forsyth {

  val initial = FEN("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")

  def <<@(variant: Variant, fen: FEN): Option[Situation] =
    makeBoard(variant, fen) map { board =>
      val splitted    = fen.value split ' '
      val playerOption = splitted lift 1 flatMap (_ lift 0) flatMap Player.apply
      val situation = playerOption match {
        case Some(player)             => Situation(board, player)
        case _ if board.check(P2) => Situation(board, P2) // user in check will move first
        case _                       => Situation(board, P1)
      }
      splitted
        .lift(2)
        .fold(situation) { strCastles =>
          val (castles, unmovedRooks) = strCastles.foldLeft(Castles.none -> Set.empty[Pos]) {
            case ((c, r), ch) =>
              val player = Player.fromP1(ch.isUpper)
              val rooks = board
                .piecesOf(player)
                .collect {
                  case (pos, piece) if piece.is(Rook) && pos.rank == Rank.backRank(player) => pos
                }
                .toList
                .sortBy(_.file)
              (for {
                kingPos <- board.kingPosOf(player)
                rookPos <- (ch.toLower match {
                  case 'k'  => rooks.reverse.find(_ ?> kingPos)
                  case 'q'  => rooks.find(_ ?< kingPos)
                  case file => rooks.find(_.file.char == file)
                })
                side <- Side.kingRookSide(kingPos, rookPos)
              } yield (c.add(player, side), r + rookPos)).getOrElse((c, r))
          }

          val fifthRank   = if (situation.player == P1) Rank.Fifth else Rank.Fourth
          val sixthRank   = if (situation.player == P1) Rank.Sixth else Rank.Third
          val seventhRank = if (situation.player == P1) Rank.Seventh else Rank.Second
          val lastMove = for {
            pos <- splitted lift 3 flatMap Pos.fromKey
            if pos.rank == sixthRank
            orig = Pos(pos.file, seventhRank)
            dest = Pos(pos.file, fifthRank)
            if situation.board(dest).contains(Piece(!situation.player, Pawn)) &&
              situation.board(pos.file, sixthRank).isEmpty &&
              situation.board(orig).isEmpty
          } yield Uci.Move(orig, dest)

          situation withHistory {
            val history = History(
              lastMove = lastMove,
              positionHashes = Array.empty,
              castles = castles,
              unmovedRooks = UnmovedRooks(unmovedRooks)
            )
            val checkCount =
              splitted
                .lift(4)
                .flatMap(makeCheckCount(_, variant))
                .orElse(splitted.lift(6).flatMap(makeCheckCount(_,variant)))
            checkCount.fold(history)(history.withCheckCount)
          }
        } fixCastles
    }

  def <<(fen: FEN): Option[Situation] = <<@(Standard, fen)


  def makeCheckCount(str: String, gameVariant: Variant): Option[CheckCount] = {
      val numchecks = gameVariant match {
        case variant.FiveCheck => 5 
        case variant.ThreeCheck => 3
        case _ => 0
      }
      str.toList match {
        case '+' :: w :: '+' :: b :: Nil =>
          for {
            p1 <- w.toString.toIntOption if p1 <= numchecks
            p2 <- b.toString.toIntOption if p2 <= numchecks
          } yield CheckCount(p2, p1)
        case w :: '+' :: b :: Nil =>
          for {
            p1 <- w.toString.toIntOption if p1 <= numchecks
            p2 <- b.toString.toIntOption if p2 <= numchecks
          } yield CheckCount(numchecks - p2, numchecks - p1)
        case _ => None
      }
  }

  case class SituationPlus(situation: Situation, fullMoveNumber: Int) {

    def turns = fullMoveNumber * 2 - situation.player.fold(2, 1)
  }

  def <<<@(variant: Variant, fen: FEN): Option[SituationPlus] =
    <<@(variant, fen) map { sit =>
      val splitted       = fen.value.split(' ').drop(4).dropWhile(_.contains('+'))
      val fullMoveNumber = splitted lift 1 flatMap (_.toIntOption) map (_ max 1 min 500)
      val halfMoveClock  = splitted lift 0 flatMap (_.toIntOption) map (_ max 0 min 100)
      SituationPlus(
        halfMoveClock.map(sit.history.setHalfMoveClock).fold(sit)(sit.withHistory),
        fullMoveNumber | 1
      )
    }

  def <<<(fen: FEN): Option[SituationPlus] = <<<@(Standard, fen)

  // only cares about pieces positions on the board (first part of FEN string)
  def makeBoard(variant: Variant, fen: FEN): Option[Board] = {
    val (position, pockets) = fen.value.takeWhile(' ' !=) match {
      case word if word.count('/' ==) == 8 =>
        val splitted = word.split('/')
        splitted.take(8).mkString("/") -> splitted.lift(8)
      case word if word.contains('[') && word.endsWith("]") =>
        word.span('[' !=) match {
          case (position, pockets) => position -> pockets.stripPrefix("[").stripSuffix("]").some
        }
      case word => word -> None
    }
    makePiecesWithCrazyPromoted(position.toList, 0, 7) map { case (pieces, promoted) =>
      val board = Board(pieces, variant = variant)
      if (promoted.isEmpty) board else board.withCrazyData(_.copy(promoted = promoted))
    } map { board =>
      pockets.fold(board) { str =>
        val (p1, p2) = str.toList.flatMap(Piece.fromChar).partition(_ is P1)
        board.withCrazyData(
          _.copy(
            pockets = Pockets(
              p1 = Pocket(p1.map(_.role).map(strategygames.Role.ChessRole)),
              p2 = Pocket(p2.map(_.role).map(strategygames.Role.ChessRole))
            )
          )
        )
      }
    }
  }

  private def makePiecesWithCrazyPromoted(
      chars: List[Char],
      x: Int,
      y: Int
  ): Option[(List[(Pos, Piece)], Set[Pos])] =
    chars match {
      case Nil                               => Option((Nil, Set.empty))
      case '/' :: rest                       => makePiecesWithCrazyPromoted(rest, 0, y - 1)
      case c :: rest if '1' <= c && c <= '8' => makePiecesWithCrazyPromoted(rest, x + (c - '0').toInt, y)
      case c :: '~' :: rest =>
        for {
          pos                        <- Pos.at(x, y)
          piece                      <- Piece.fromChar(c)
          (nextPieces, nextPromoted) <- makePiecesWithCrazyPromoted(rest, x + 1, y)
        } yield (pos -> piece :: nextPieces, nextPromoted + pos)
      case c :: rest =>
        for {
          pos                        <- Pos.at(x, y)
          piece                      <- Piece.fromChar(c)
          (nextPieces, nextPromoted) <- makePiecesWithCrazyPromoted(rest, x + 1, y)
        } yield (pos -> piece :: nextPieces, nextPromoted)
    }

  def >>(situation: Situation): FEN = >>(SituationPlus(situation, 1))

  def >>(parsed: SituationPlus): FEN =
    parsed match {
      case SituationPlus(situation, _) => >>(Game(situation, turns = parsed.turns))
    }

  def >>(game: Game): FEN = FEN {
    {
      List(
        exportBoard(game.board) + exportCrazyPocket(game.board),
        game.player.letter,
        exportCastles(game.board),
        game.situation.enPassantSquare.map(_.toString).getOrElse("-"),
        game.halfMoveClock,
        game.fullMoveNumber
      ) ::: {
        if (game.board.variant == variant.ThreeCheck || game.board.variant == variant.FiveCheck) List(exportCheckCount(game.board))
        else List()
      }
    } mkString " "
  }

  def exportStandardPositionTurnCastlingEp(situation: Situation): String =
    List(
      exportBoard(situation.board),
      situation.player.letter,
      exportCastles(situation.board),
      situation.enPassantSquare.map(_.toString).getOrElse("-")
    ) mkString " "

  private def exportCheckCount(board: Board) =
    board.history.checkCount match {
      case CheckCount(p1, p2) => s"+$p2+$p1"
    }

  private def exportCrazyPocket(board: Board) =
    board.pocketData match {
      case Some(PocketData(pockets, _)) =>
        "/" +
          pockets.p1.roles.map(_.forsyth).map(_.toUpper).mkString +
          pockets.p2.roles.map(_.forsyth).mkString
      case _ => ""
    }

  implicit private val posOrdering = Ordering.by[Pos, File](_.file)

  private[chess] def exportCastles(board: Board): String = {

    lazy val wr = board.pieces.collect {
      case (pos, piece) if pos.rank == Rank.backRank(P1) && piece == Piece(P1, Rook) => pos
    }
    lazy val br = board.pieces.collect {
      case (pos, piece) if pos.rank == Rank.backRank(P2) && piece == Piece(P2, Rook) => pos
    }

    lazy val wur = board.unmovedRooks.pos.filter(_.rank == Rank.backRank(P1))
    lazy val bur = board.unmovedRooks.pos.filter(_.rank == Rank.backRank(P2))

    {
      // castling rights with inner rooks are represented by their file name
      (if (board.castles.p1KingSide && wr.nonEmpty && wur.nonEmpty)
         (if (wur contains wr.max) "K" else wur.max.file.toUpperCaseString)
       else "") +
        (if (board.castles.p1QueenSide && wr.nonEmpty && wur.nonEmpty)
           (if (wur contains wr.min) "Q" else wur.min.file.toUpperCaseString)
         else "") +
        (if (board.castles.p2KingSide && br.nonEmpty && bur.nonEmpty)
           (if (bur contains br.max) "k" else bur.max.file)
         else "") +
        (if (board.castles.p2QueenSide && br.nonEmpty && bur.nonEmpty)
           (if (bur contains br.min) "q" else bur.min.file)
         else "")
    } match {
      case "" => "-"
      case n  => n
    }
  }

  def exportBoard(board: Board): String = {
    val fen   = new scala.collection.mutable.StringBuilder(70)
    var empty = 0
    for (y <- Rank.allReversed) {
      empty = 0
      for (x <- File.all) {
        board(x, y) match {
          case None => empty = empty + 1
          case Some(piece) =>
            if (empty == 0) fen append piece.forsyth.toString
            else {
              fen append (empty.toString + piece.forsyth)
              empty = 0
            }
            if (piece.role != Pawn && board.pocketData.fold(false)(_.promoted.contains(Pos(x, y))))
              fen append '~'
        }
      }
      if (empty > 0) fen append empty
      if (y > Rank.First) fen append '/'
    }
    fen.toString
  }

  def boardAndPlayer(situation: Situation): String =
    boardAndPlayer(situation.board, situation.player)

  def boardAndPlayer(board: Board, turnPlayer: Player): String =
    s"${exportBoard(board)} ${turnPlayer.letter}"
}
