//package strategygames.abalone.format
//
//import strategygames.abalone._
//import strategygames.abalone.variant.Variant
//import strategygames.{Player, Score}
//
///** Transform a game to standard Forsyth Edwards Notation
//  * http://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation
//  */
//@deprecated("Alex", since="1.5.5") object Forsyth {
//
//  val initial = FEN("ss1SS/sssSSS/1ss1SS1/8/9/8/1SS1ss1/SSSsss/SS1ss 0 0 b 0 1")
//
//  def <<@(variant: Variant, fen: FEN): Option[Situation] = {
//    Some(
//      Situation(
//        Board(
//          pieces = fen.pieces,
//          history = History(),
//          variant = variant
//        ),
//        fen.value.split(' ')(3) match {
//          case "b" => P1
//          case "w" => P2
//          case _ => sys.error("Invalid player in fen")
//        }
//      ).withHistory(
//        History(
//          score = Score(fen.player1Score, fen.player2Score),
//          halfMoveClock = fen.halfMovesSinceLastCapture.getOrElse(0)
//        )
//      )
//    )
//  }
//
//  def <<(fen: FEN): Option[Situation] = <<@(Variant.default, fen)
//
//  case class SituationPlus(situation: Situation, fullTurnCount: Int) {
//    def turnCount = fullTurnCount * 2 - situation.player.fold(2, 1)
//
//    // when we get a multiaction variant we should set this
//    def plies = turnCount
//  }
//
//  def <<<@(variant: Variant, fen: FEN): Option[SituationPlus] =
//    <<@(variant, fen) map { sit =>
//      SituationPlus(
//        // not doing half move clock history like we do in chess
//        sit,
//        fen.value.split(' ').last.toIntOption.map(_ max 1 min 500) | 1
//      )
//    }
//
//  def <<<(fen: FEN): Option[SituationPlus] = <<<@(Variant.default, fen)
//
//  def >>(situation: Situation): FEN = >>(SituationPlus(situation, 1))
//
//  def >>(parsed: SituationPlus): FEN =
//    parsed match {
//      case SituationPlus(situation, _) =>
//        >>(Game(situation, plies = parsed.plies, turnCount = parsed.turnCount))
//    }
//
//  def >>(game: Game): FEN = {
//    val boardFen = boardPart(game.situation.board)
//    val scoreStr = game.situation.board.history.score.fenStr
//    val player = game.situation.player.fold('b', 'w')
//    val halfMoves = game.situation.board.history.halfMoveClock
//    val fullMoves = game.fullTurnCount
//    FEN(s"${boardFen} ${scoreStr} ${player} ${halfMoves} ${fullMoves}")
//  }
//
//  def exportBoard(board: Board): String = {
//    val boardFen = boardPart(board)
//    val scoreStr = board.history.score.fenStr
//    s"${boardFen} ${scoreStr}"
//  }
//
//  // return the FEN
//  def boardPart(board: Board): String = {
//    val fen = new scala.collection.mutable.StringBuilder(70)
//    var empty = 0
//    for (y <- Rank.allReversed) {
//      empty = 0
//      for (x <- File.all) {
//        board(x, y) match {
//          case None => if (Pos(x, y).isDefined) empty = empty + 1
//          case Some(piece) =>
//            if (empty > 0) {
//              fen.append(empty)
//              empty = 0
//            }
//            if (piece.player == Player.P1)
//              fen.append(piece.forsyth.toString.toUpperCase())
//            else
//              fen.append(piece.forsyth.toString.toLowerCase())
//        }
//      }
//      if (empty > 0) fen.append(s"${empty},")
//      fen.append('/')
//    }
//    fen.toString.replace(",/", "/").dropRight(1)
//  }
//
//  def boardAndPlayer(situation: Situation): String = boardAndPlayer(situation.board, situation.player)
//
//  def boardAndPlayer(board: Board, turnPlayer: Player): String = s"${exportBoard(board)} ${turnPlayer.letter}"
//}