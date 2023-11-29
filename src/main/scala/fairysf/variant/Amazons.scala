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
      fairysfName = FairySFName("amazons"),
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

  override def hasAnalysisBoard: Boolean = false
  override def hasFishnet: Boolean       = true

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
    fen.toString.replace(",/", "/").dropRight(1)
  }

  private def fullPockets: String = s"[${"P" * 46}${"p" * 46}]"

  override def exportBoardFen(board: Board): FEN =
    FEN(
      s"${boardPart(board)}${fullPockets} ${board.apiPosition.fen.value.split(" ").drop(1).mkString(" ")}"
    )

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
          .map { case Some(Uci.Move.moveR(_, dest, _)) => Pos.fromKey(dest) }
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
    Api.validateFEN(fairysfName.name, board.apiPosition.fen.value)

  override def staleMate(situation: Situation): Boolean     = false
  override def specialEnd(situation: Situation): Boolean    = situation.board.apiPosition.legalMoves.isEmpty
  override def winner(situation: Situation): Option[Player] =
    if (specialEnd(situation)) Option(!situation.player)
    else None

}
