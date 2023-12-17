package strategygames.chess.variant

import strategygames.chess._
import strategygames.chess.format.FEN
import strategygames.Player

case object Monster
    extends Variant(
      id = 15,
      key = "monster",
      name = "Monster",
      standardInitialPosition = false
    ) {

  def perfId: Int    = 23
  def perfIcon: Char = ''

  override def hasAnalysisBoard: Boolean = false
  override def hasFishnet: Boolean       = false

  override def exoticChessVariant       = true
  // override def p1IsBetterVariant        = true
  override def blindModeVariant         = false
  override def materialImbalanceVariant = true

  lazy val pieces: Map[Pos, Piece] = {

    val p1Pieces = Map(
      Pos.E1 -> Piece(P1, King),
      Pos.C2 -> Piece(P1, Pawn),
      Pos.D2 -> Piece(P1, Pawn),
      Pos.E2 -> Piece(P1, Pawn),
      Pos.F2 -> Piece(P1, Pawn)
    )

    val p2Pieces =
      (for (y <- List(Rank.Seventh, Rank.Eighth); x <- File.all) yield {
        Pos(x, y) -> (y match {
          case Rank.Eighth => Piece(P2, backRank(x.index))
          case _           => Piece(P2, Pawn)
        })
      }).toMap

    p1Pieces ++ p2Pieces
  }

  override val castles = Castles("kq")

  override val initialFen = FEN(
    "rnbqkbnr/pppppppp/8/8/8/8/2PPPP2/4K3 w kq - 0 1"
  )

  override def lastActionOfTurn(situation: Situation): Boolean =
    situation.player match {
      case P1 => situation.board.lastActionPlayer == Some(P1)
      case P2 => true
    }

  private def oneMoveKingSafety(
      m: Move,
      filter: Piece => Boolean,
      kingPos: Option[Pos]
  ): Boolean =
    ! {
      kingPos exists {
        super.kingThreatened(m.after, m.playerAfter, _, filter)
      }
    }

  override def kingSafety(
      m: Move,
      filter: Piece => Boolean,
      kingPos: Option[Pos]
  ): Boolean =
    m.player match {
      case P1 if lastActionOfTurn(m.situationBefore) =>
        oneMoveKingSafety(m, filter, kingPos)
      case P1                                        =>
        m.situationAfter.moves.values.flatten.size > 0 || m.situationAfter.board.checkP2
      case P2                                        =>
        super.kingSafety(m, filter, kingPos)
      // oneMoveKingSafety(
      //  m,
      //  filter,
      //  kingPos
      // ) &&
      // !m.situationAfter.moves.values.flatten
      //  .map(nextMove => oneMoveKingSafety(nextMove, _ => true, kingPos))
      //  .toList
      //  .contains(false)
    }

  override def kingThreatened(
      board: Board,
      player: Player,
      to: Pos,
      filter: Piece => Boolean = _ => true
  ): Boolean = {
    player match {
      case P1 if board.history.currentTurn.isEmpty => {
        super.kingThreatened(board, player, to, filter) || Situation(
          board.updateHistory { h =>
            h.copy(
              lastTurn = List.empty
            )
          },
          P1
        ).moves.values.flatten
          .map(nextMove =>
            super.kingThreatened(nextMove.after, player, to, _ => true) ||
              (if (nextMove.promotion.nonEmpty)
                 super.kingThreatened(
                   nextMove.after.copy(
                     pieces = nextMove.after.pieces + (nextMove.dest -> Piece(
                       nextMove.player,
                       Knight
                     ))
                   ),
                   player,
                   to,
                   _ => true
                 )
               else false)
          )
          .toList
          .contains(true)
      }
      case _                                       => super.kingThreatened(board, player, to, filter)
    }
  }

  override def checkmate(situation: Situation) =
    situation.check && !situation.board.check(
      !situation.player
    ) && situation.moves.isEmpty

  override def valid(board: Board, strict: Boolean) =
    validSide(board, strict)(P2) && {
      val roles = board.rolesOf(P1)
      roles.count(_ == King) == 1 &&
      (!strict || roles.count(_ == Pawn) <= 4) &&
      !pawnsOnPromotionRank(board, P1) &&
      board.piecesOf(P1).size <= 5
    }

}
