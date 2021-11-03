package strategygames.fairysf.format

import cats.implicits._
import strategygames.{ Color, Pocket, Pockets }
import strategygames.fairysf._
import strategygames.fairysf.variant.{ Variant }

/** Transform a game to standard Forsyth Edwards Notation
  * http://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation
  *
  * Crazyhouse & Threecheck extensions:
  * https://github.com/ddugovic/Stockfish/wiki/FEN-extensions
  * http://scidb.sourceforge.net/help/en/FEN.html#ThreeCheck
  */
object Forsyth {

  //lishogi
  //val initial = FEN("lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 1")
  //pychess shogi
  val initial = FEN("lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL[-] w 0 1")

  //stub
  def <<@(variant: Variant, fen: FEN): Option[Situation] = Some(Situation(
    Board(
      Api.pieceMapFromFen(variant.fairysfName.name, fen.value),
      History(),
      variant,
      if (variant.dropsVariant){
        val piecesInHand = Api.piecesInHand(variant.fairysfName.name, fen.value)
        PocketData(
          Pockets(
            Pocket(piecesInHand.filter(_.color == White).toList.map(
              p => strategygames.Role.FairySFRole(p.role)
            )),
            Pocket(piecesInHand.filter(_.color == Black).toList.map(
              p => strategygames.Role.FairySFRole(p.role)
            ))
          ),
          //Can make an empty Set of Pos because we dont have to track promoted pieces
          //(FairySF presumably does this)
          Set[Pos]()
        ).some
      } else None
    ),
    fen.value.split(' ')(1) match {
      case "w" => White
      case "b" => Black
      case _ => sys.error("Invalid color in fen")
    }
  ))

  def <<(fen: FEN): Option[Situation] = <<@(Variant.default, fen)

  case class SituationPlus(situation: Situation, fullMoveNumber: Int) {

    def turns = fullMoveNumber * 2 - situation.color.fold(2, 1)
  }

  def <<<@(variant: Variant, fen: FEN): Option[SituationPlus] =
    <<@(variant, fen) map { sit =>
      SituationPlus(
        //not doing half move clock history like we do in chess
        sit,
        fen.value.split(' ').last.toIntOption.map(_ max 1 min 500) | 1
      )
    }

  def <<<(fen: FEN): Option[SituationPlus] = <<<@(Variant.default, fen)

  def >>(situation: Situation): FEN = >>(SituationPlus(situation, 1))

  def >>(parsed: SituationPlus): FEN =
    parsed match {
      case SituationPlus(situation, _) => >>(Game(situation, turns = parsed.turns))
    }

  def >>(game: Game): FEN = exportBoardFen(game.situation.board) //TODO: ???

  //TODO: might need to update this
  def exportBoard(board: Board): String = {
    val fen   = new scala.collection.mutable.StringBuilder(70)
    var empty = 0
    for (y <- Rank.allReversed) {
      empty = 0
      for (x <- File.all) {
        board(x, y) match {
          case None => empty = empty + 1
          case Some(piece) =>
            //TODO: handle shogi promoted pieces correctly
            if (empty == 0) fen append piece.forsyth.toString
            else {
              fen append (empty.toString + piece.forsyth)
              empty = 0
            }
            if (board.pocketData.fold(false)(_.promoted.contains(Pos(x, y))))
              fen append '~'
        }
      }
      if (empty > 0) fen append empty
      if (y > Rank.First) fen append '/'
    }
    fen.toString
  }

  def exportBoardFen(board: Board): FEN = FEN(exportBoard(board))

  def boardAndColor(situation: Situation): String =
    boardAndColor(situation.board, situation.color)

  def boardAndColor(board: Board, turnColor: Color): String =
    s"${exportBoard(board)} ${turnColor.letter}"
}
