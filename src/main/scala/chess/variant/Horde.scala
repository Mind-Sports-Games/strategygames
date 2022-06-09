package strategygames.chess.variant

import strategygames.chess._
import strategygames.chess.format.FEN
import strategygames.Player

case object Horde
    extends Variant(
      id = 8,
      key = "horde",
      name = "Horde",
      standardInitialPosition = false
    ) {

  def perfId: Int    = 16
  def perfIcon: Char = '_'

  override def exoticChessVariant       = true
  override def p1IsBetterVariant        = true
  override def blindModeVariant         = false
  override def materialImbalanceVariant = true

  /** In Horde chess p1 advances against p2 with a horde of pawns.
    */
  lazy val pieces: Map[Pos, Piece] = {

    val frontPawns = List(Pos.B5, Pos.C5, Pos.F5, Pos.G5).map { _ -> Piece(P1, Pawn) }

    val p1PawnsHorde = frontPawns ++ (for {
      x <- File.all
      y <- Rank.all.take(4)
    } yield (Pos(x, y) -> Piece(P1, Pawn))) toMap

    val p2Pieces = (for (y <- List(Rank.Seventh, Rank.Eighth); x <- File.all) yield {
      Pos(x, y) -> (y match {
        case Rank.Eighth  => Piece(P2, backRank(x.index))
        case Rank.Seventh => Piece(P2, Pawn)
      })
    }).toMap

    p2Pieces ++ p1PawnsHorde
  }

  override val castles = Castles("kq")

  override val initialFen = FEN("rnbqkbnr/pppppppp/8/1PP2PP1/PPPPPPPP/PPPPPPPP/PPPPPPPP/PPPPPPPP w kq - 0 1")

  override def valid(board: Board, strict: Boolean) =
    board.kingPosOf(P1).isEmpty && validSide(board, strict)(P2) && !pawnsOnPromotionRank(board, P1)

  /** The game has a special end condition when p2 manages to capture all of p1's pawns */
  override def specialEnd(situation: Situation) =
    situation.board.piecesOf(P1).isEmpty

  /** Any vs K + any where horde is stalemated and only king can move is a fortress draw This does not
    * consider imminent fortresses such as 8/p7/P7/8/8/P7/8/k7 b - - nor does it consider contrived fortresses
    * such as b7/pk6/P7/P7/8/8/8/8 b - -
    */
  private def hordeClosedPosition(board: Board) = {
    lazy val notKingBoard = board.kingPos.get(Player.p2).flatMap(board.take).getOrElse(board)
    val hordePos          = board.occupation(Player.p1) // may include promoted pieces
    val mateInOne         =
      hordePos.sizeIs == 1 && hordePos.forall(pos => pieceThreatened(board, Player.p2, pos, (_ => true)))
    !mateInOne && notKingBoard.actors.values.forall(actor => actor.moves.isEmpty)
  }

  /** In horde chess, p2 can win unless a fortress stalemate is unavoidable. Auto-drawing the game should
    * almost never happen, but it did in https://lichess.org/xQ2RsU8N#121
    */
  override def isInsufficientMaterial(board: Board) = hordeClosedPosition(board)

  /** In horde chess, the horde cannot win on * V K or [BN]{2} v K or just one piece since they lack a king
    * for checkmate support. Technically there are some positions where stalemate is unavoidable which this
    * method does not detect; however, such are trivial to premove.
    */
  override def opponentHasInsufficientMaterial(situation: Situation): Boolean = {
    val board          = situation.board
    val opponentPlayer = !situation.player
    lazy val fortress  = hordeClosedPosition(board) // costly function call
    if (opponentPlayer == Player.p1) {
      lazy val notKingPieces            = InsufficientMatingMaterial.nonKingPieces(board) toList
      val horde                         = board.piecesOf(Player.p1)
      lazy val hordeBishopSquarePlayers = horde.filter(_._2.is(Bishop)).toList.map(_._1.isLight).distinct
      lazy val hordeRoles               = horde.map(_._2.role)
      lazy val army                     = board.piecesOf(Player.p2)
      lazy val armyPawnsOrRooks         = army.count(p => p._2.is(Pawn) || p._2.is(Rook))
      lazy val armyPawnsOrBishops       = army.filter(p => p._2.is(Pawn) || p._2.is(Bishop))
      lazy val armyPawnsOrKnights       = army.count(p => p._2.is(Pawn) || p._2.is(Knight))
      lazy val armyNonQueens            = army.count(_._2.isNot(Queen))
      lazy val armyNonQueensOrRooks     = army.count(p => p._2.isNot(Queen) && p._2.isNot(Rook))
      lazy val armyNonQueensOrBishops   = army.count(p => p._2.isNot(Queen) && p._2.isNot(Bishop))
      lazy val armyBishopSquarePlayers  = army.filter(_._2.is(Bishop)).toList.map(_._1.isLight).distinct
      if (horde.sizeIs == 1) {
        hordeRoles match {
          case List(Knight) =>
            army.sizeIs < 4 || armyNonQueensOrRooks == 0 || armyNonQueensOrBishops == 0 || (armyNonQueensOrBishops + armyBishopSquarePlayers.size) < 4
          case List(Bishop) =>
            notKingPieces.count(p =>
              p._2.is(Pawn) || (p._2.is(Bishop) && p._1.isLight != horde.head._1.isLight)
            ) < 2
          case List(Rook)   => army.sizeIs < 3 || armyPawnsOrRooks == 0 || armyPawnsOrKnights == 0
          case _            => armyPawnsOrRooks == 0
        }
      } else if (
        (hordeRoles.forall(
          _ == Bishop
        ) && hordeBishopSquarePlayers.lengthCompare(1) == 0) && {
          armyPawnsOrKnights + armyPawnsOrBishops
            .count(p => p._1.isLight != horde.head._1.isLight) < 2
        }
      ) true
      else if (
        horde.sizeIs == 2 && hordeRoles
          .count(r => r == Queen || r == Rook || r == Pawn) < 2 && armyNonQueens <= 1
      )
        true
      else fortress
    } else fortress
  }

  override def isUnmovedPawn(player: Player, pos: Pos) =
    if (player.p1) pos.rank <= Rank.Second
    else pos.rank == Rank.Seventh
}
