package strategygames.chess

import strategygames.Player

/** Utility methods for helping to determine whether a situation is a draw or a draw
  * on a player flagging.
  *
  * See http://www.e4ec.org/immr.html
  */
object InsufficientMatingMaterial {

  def nonKingPieces(board: Board) = board.pieces filter (_._2.role != King)

  def bishopsOnOppositePlayers(board: Board) =
    (board.pieces collect { case (pos, Piece(_, Bishop)) => pos.isLight } toList).distinct
    .lengthCompare(2) == 0

  /*
   * Returns true if a pawn cannot progress forward because it is blocked by a pawn
   */
  def pawnBlockedByPawn(pawn: Actor, board: Board) =
    pawn.moves.isEmpty && {
      val blockingPosition = Actor.posAheadOfPawn(pawn.pos, pawn.piece.player)
      blockingPosition.flatMap(board.apply).exists(_.is(Pawn))
    }

  /*
   * Determines whether a board position is an automatic draw due to neither player
   * being able to mate the other as informed by the traditional chess rules.
   */
  def apply(board: Board) = {
    lazy val kingsAndBishopsOnly = board.pieces forall { p =>
      (p._2 is King) || (p._2 is Bishop)
    }
    val kingsAndMinorsOnly = board.pieces forall { p =>
      (p._2 is King) || (p._2 is Bishop) || (p._2 is Knight)
    }

    kingsAndMinorsOnly && (board.pieces.size <= 3 || (kingsAndBishopsOnly && !bishopsOnOppositePlayers(board)))
  }

  /*
   * Determines whether a player does not have mating material. In general:
   * King by itself is not mating material
   * King + knight mates against king + any(rook, bishop, knight, pawn)
   * King + bishop mates against king + any(bishop, knight, pawn)
   * King + bishop(s) versus king + bishop(s) depends upon bishop square players
   */
  def apply(board: Board, player: Player) = {

    val kingsAndMinorsOnlyOfPlayer = board.piecesOf(player) forall { p =>
      (p._2 is King) || (p._2 is Bishop) || (p._2 is Knight)
    }
    lazy val nonKingRolesOfPlayer  = board rolesOf player filter (King !=)
    lazy val rolesOfOpponentPlayer = board rolesOf !player

    kingsAndMinorsOnlyOfPlayer && (nonKingRolesOfPlayer.distinct match {
      case Nil => true
      case List(Knight) =>
        nonKingRolesOfPlayer.lengthCompare(
          1
        ) == 0 && !(rolesOfOpponentPlayer filter (King !=) exists (Queen !=))
      case List(Bishop) =>
        !(rolesOfOpponentPlayer.exists(r => r == Knight || r == Pawn) || bishopsOnOppositePlayers(board))
      case _ => false
    })
  }
}
