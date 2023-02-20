package strategygames.fairysf
package variant

import cats.syntax.option._

import strategygames.fairysf.format.{ FEN, Forsyth, Uci }
import strategygames.GameFamily

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
  override val switchPlayerAfterMove = false

  // cache this rather than checking with the API everytime
  override def initialFen =
    format.FEN(
      "3q2q3/10/10/q8q/10/10/Q8Q/10/10/3Q2Q3[PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPpppppppppppppppppppppppppppppppppppppppppppppp] w - - 0 1"
    )

  override def validMoves(situation: Situation): Map[Pos, List[Move]] =
    situation.board.history.lastMove match {
      case Some(_: Uci.Move) => Map.empty()
      case _                 =>
        situation.board.apiPosition.legalMoves
          .map(_.split(",").headOption)
          .map { case Some(Uci.Move.moveR(orig, dest, promotion)) =>
            (
              Pos.fromKey(orig),
              Pos.fromKey(dest),
              promotion
            )
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
    situation.board.history.lastMove match {
      case Some(lastMove: Uci.Move) =>
        situation.board.apiPosition.legalMoves
          .filter(_.startsWith(lastMove.uci))
          .map(_.split(",").reverse.headOption)
          .map { case Some(Uci.Move.moveR(_, dest, _)) => Pos.fromKey(dest) }
          .map {
            case Some(dest) => {
              // val uciDrop     = s"${defaultDropRole.forsyth}@${dest.key}"
              val uciMove     = s"${lastMove.uci},${lastMove.dest.key}${dest.key}"
              val newPosition = situation.board.apiPosition.makeMoves(List(uciMove))
              Drop(
                piece = Piece(situation.player, defaultDropRole),
                pos = dest,
                situationBefore = situation,
                after = situation.board.copy(
                  pieces = newPosition.pieceMap,
                  uciMoves = situation.board.uciMoves :+ uciMove,
                  pocketData = newPosition.pocketData,
                  position = newPosition.some
                )
              )
            }
            case dest       => sys.error(s"Invalid position from uci: ${defaultDropRole}@${dest}")
          }
          .toList
      case _                        => List()
    }

}
