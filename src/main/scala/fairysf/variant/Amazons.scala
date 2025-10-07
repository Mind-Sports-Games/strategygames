package strategygames.fairysf
package variant

import cats.syntax.option._

import strategygames.fairysf.format.{ FEN, Uci }
import strategygames.{ GameFamily, Player }

case object Amazons
    extends Variant(
      id = 8,
      key = "amazons",
      name = "Amazons",
      standardInitialPosition = true,
      fishnetKey = "amazons",
      boardSize = Board.Dim10x10
    ) {

  def gameFamily: GameFamily = GameFamily.Amazons()

  def perfIcon: Char = '€'
  def perfId: Int    = 206

  override def baseVariant           = true
  override def repetitionEnabled     = false
  override def dropsVariant          = true
  override def canOfferDraw          = false
  override val switchPlayerAfterMove = false

  override def hasAnalysisBoard: Boolean = true
  override def hasFishnet: Boolean       = false

  // cache this rather than checking with the API everytime
  override def initialFen =
    format.FEN(
      "3q2q3/10/10/q8q/10/10/Q8Q/10/10/3Q2Q3[PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPpppppppppppppppppppppppppppppppppppppppppppppp] w - - 0 1"
    )

  private def boardPart(board: Board): String = {
    val fen   = new scala.collection.mutable.StringBuilder(70)
    var empty = 0
    for (y <- Rank.allReversed) {
      empty = 0
      for (x <- File.all) {
        board(x, y) match {
          case None        => empty = empty + 1
          case Some(piece) =>
            if (empty > 0) {
              fen.append(empty)
              empty = 0
            }
            if (piece.player == Player.P1)
              fen.append(piece.forsyth.toString.toUpperCase())
            else fen.append(piece.forsyth.toString.toLowerCase())
        }
      }
      if (empty > 0) fen.append(s"${empty},")
      fen.append('/')
    }
    fen.toString.replace("P", "p").replace(",/", "/").dropRight(1)
  }

  private def fullPockets: String = s"[${"P" * 46}${"p" * 46}]"

  override def exportBoardFen(board: Board): FEN =
    FEN(
      List(
        s"${boardPart(board)}${fullPockets}",
        board.apiPosition.fen.value.split(" ").drop(1).mkString(" "),
        board.history.currentTurn.headOption.map(u => s"½${u.uci}").getOrElse("")
      ).mkString(" ").strip
    )

  private def makePieceMapFromBoardFen(boardFEN: String): PieceMap = {

    def makePieceMap(
        chars: List[Char],
        gf: GameFamily,
        x: Int,
        y: Int
    ): Option[List[(Pos, Piece)]] =
      chars match {
        case Nil                               => Option(Nil)
        case '/' :: rest                       => makePieceMap(rest, gf, 0, y - 1)
        case '1' :: '0' :: rest                => makePieceMap(rest, gf, x + 10, y)
        case c :: rest if '1' <= c && c <= '9' => makePieceMap(rest, gf, x + (c - '0').toInt, y)
        case c :: rest                         =>
          for {
            pos        <- Pos.at(x, y)
            piece      <- Piece.fromChar(gf, c)
            nextPieces <- makePieceMap(rest, gf, x + 1, y)
          } yield (pos -> piece :: nextPieces)
      }

    makePieceMap(
      boardFEN.toList,
      gameFamily,
      0,
      boardSize.height - 1
    ).toList.flatten.toMap
  }

  override def <<@(
    fen: FEN,
    pieceMap: Option[PieceMap] = None,
    history: Option[History] = None
  ): Option[Situation] = {
    if (fen.value.contains("½")) {
      val fenPieceMap      = makePieceMapFromBoardFen(fen.boardPart(Some(boardSize.height)))
      val halfMove         = fen.value.dropWhile('½' !=).drop(1)
      val uciMove          = Uci.Move.apply(gameFamily, halfMove) match {
        case Some(u) => u
        case None    => sys.error(s"Invalid ½ move: ${halfMove}")
      }
      val validatedUciMove = if (uciMove.orig == uciMove.dest) {
        val activePlayer = fenPieceMap.get(uciMove.orig) match {
          case Some(piece) => piece.player
          case None        => sys.error(s"No piece found for ½ move: ${halfMove}")
        }
        val apiPosition = Api.positionFromVariantNameAndFEN(fishnetKey, fen.value.takeWhile('½' !=))
        (Situation(
          Board(
            pieces = fenPieceMap,
            history = History(),
            variant = this,
            pocketData = apiPosition.pocketData,
            position = apiPosition.some
          ),
          activePlayer
        ).moves.get(uciMove.orig) match {
          //select random move to this position as a fake move to allow us to create a fairy position
          case Some(moves) if moves.length > 0 => moves.head
          case _                               => sys.error(s"Impossible ½ move: ${halfMove}")
        }).toUci.invert
      } else uciMove
      val previousBoardFen = exportBoardFen(
        Board(
          // unapply ½ move
          pieces = fenPieceMap.get(validatedUciMove.dest) match {
            case Some(piece) => fenPieceMap + ((validatedUciMove.orig, piece)) - validatedUciMove.dest
            case None        => fenPieceMap
          },
          variant = this
        )
      )
      super.<<@(
        FEN(
          (previousBoardFen.value.split(' ').headOption.toList ++ fen.value.split(' ').drop(1))
            .mkString(" ")
        ),
        Some(fenPieceMap),
        Some(History(currentTurn = List(validatedUciMove)))
      )
    } else super.<<@(fen, pieceMap, history)
  }

  override def validMoves(situation: Situation): Map[Pos, List[Move]] =
    situation.board.history.lastAction match {
      case Some(_: Uci.Move) => Map.empty
      case _                 =>
        situation.board.apiPosition.legalMoves
          .map(_.split(",").headOption)
          .map {
            case Some(Uci.Move.moveR(orig, dest, promotion)) =>
              (
                Pos.fromKey(orig),
                Pos.fromKey(dest),
                promotion
              )
            case Some(x)                                     => sys.error(s"Ilegal move for Amazons: ${x}")
            case _                                           => sys.error(s"Illegal unknown move for Amazons.")
          }
          .distinct
          .map {
            case (Some(orig), Some(dest), _) => {
              val piece = situation.board.pieces(orig)
              (
                orig,
                Move(
                  piece = piece,
                  orig = orig,
                  dest = dest,
                  situationBefore = situation,
                  after = situation.board.copy(
                    pieces = situation.board.pieces - orig + ((dest, piece))
                  ),
                  autoEndTurn = false, // always false for Amazons as we follow a Move with a Drop
                  capture = None,
                  promotion = None,
                  castle = None,
                  enpassant = false
                )
              )
            }
            case (orig, dest, prom)          => sys.error(s"Invalid position from uci: ${orig}${dest}${prom}")
          }
          .groupBy(_._1)
          .map { case (k, v) => (k, v.toList.map(_._2)) }
    }

  private val defaultDropRole: Role = AmazonArrow

  override def validDrops(situation: Situation): List[Drop] =
    situation.board.history.lastAction match {
      case Some(lastMove: Uci.Move) =>
        situation.board.apiPosition.legalMoves
          .filter(_.startsWith(s"${lastMove.uci},"))
          .map(_.split(",").reverse.headOption)
          .flatMap {
            case Some(Uci.Move.moveR(_, dest, _)) => Some(Pos.fromKey(dest))
            case _                                => None
          }
          .map {
            case Some(dest) => {
              // val uciDrop     = s"${defaultDropRole.forsyth}@${dest.key}"
              val uciMove     = s"${lastMove.uci},${lastMove.dest.key}${dest.key}"
              val newPosition = situation.board.apiPosition.makeMoves(List(uciMove))
              val piece       = Piece(situation.player, defaultDropRole)
              Drop(
                piece = piece,
                pos = dest,
                situationBefore = situation,
                after = situation.board.copy(
                  pieces = situation.board.pieces + ((dest, piece)),
                  uciMoves = situation.board.uciMoves :+ uciMove,
                  position = newPosition.some
                ),
                autoEndTurn = true
              )
            }
            case dest       => sys.error(s"Invalid position from uci: ${defaultDropRole}@${dest}")
          }
          .toList
      case _                        => List()
    }

  override def valid(board: Board, strict: Boolean): Boolean =
    Api.validateFEN(fishnetKey, board.apiPosition.fen.value)

  override def staleMate(situation: Situation): Boolean     = false
  override def specialEnd(situation: Situation): Boolean    = situation.board.apiPosition.legalMoves.isEmpty
  override def winner(situation: Situation): Option[Player] =
    if (specialEnd(situation)) Option(!situation.player)
    else None

  override def pliesFromFen(fenTurnCount: Int, player: Player, currentTurnPlies: Int = 0) =
    (fenTurnCount - 1) * 4 + player.fold(0, 2) + currentTurnPlies
}
