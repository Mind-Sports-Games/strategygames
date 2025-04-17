package strategygames.dameo
package format

import cats.implicits._

import scala.annotation.nowarn

import strategygames.Player

import variant.{ Dameo, Variant }

/** Transform a game to draughts Forsyth Edwards Notation
  * https://en.wikipedia.org/wiki/Portable_Draughts_Notation Additions: Piece role g/p = Ghost man or king of
  * that player, has been captured but not removed because the forced capture sequence is not finished yet.
  * Piece role a/b = Active Man or King, the piece performing the forced capture sequence in progress.
  * ":Hx" = Halfmove clock: This is the number of halfmoves since a forced draw material combination appears.
  * This is used to determine if a draw can be claimed. ":Fx" = Fullmove number: The number of the full move.
  * It starts at 1, and is incremented after P2's move.
  */
object Forsyth {

  val initial =
    FEN(
      "W:Wa1,b1,b2,c1,c2,c3,d1,d2,d3,e1,e2,e3,f1,f2,f3,g1,g2,h1:Ba8,b7,b8,c6,c7,c8,d6,d7,d8,e6,e7,e8,f6,f7,f8,g7,g8,h8:H0:F1"
    )

  def <<@(variant: Variant, fen: FEN): Option[Situation] = {
    /* Convert a FEN + Variant into a Situation */
    Some(
      Situation(
        Board(
          pieces = fen.pieces,
          history = History(halfMoveClock=fen.halfMoveClock.getOrElse(0)),
          variant = variant
        ),
        fen.player.get
      ).withHistory(
        History(
          halfMoveClock=fen.halfMoveClock.getOrElse(0)
        )
      )
    )
  }

  /* Convert a FEN into a situation using the default variant */
  def <<(fen: FEN): Option[Situation] = <<@(Dameo, fen)

  case class SituationPlus(situation: Situation, fullTurnCount: Int) {
    def turnCount = fullTurnCount * 2 - (if (situation.player.p1) 2 else 1)
    def plies     = situation.history.currentTurn.length
  }

  def <<<@(variant: Variant, fen: FEN): Option[SituationPlus] =
  /* Convert a FEN + Variant into a SituationPlus */
    <<@(variant, fen) map { sit =>
      val fullMoveNumber = fen.fullMove
      SituationPlus(
        sit,
        fullMoveNumber | 1
      )
    }

  def <<<(fen: FEN): Option[SituationPlus] = <<<@(Dameo, fen)

  def countGhosts(fen: FEN): Int =
    fen.pieces.values.count(x => List(GhostKing, GhostMan).contains(x.role))

  def countKings(fen: FEN): Int =
    fen.pieces.values.count(x => x.role == King)

  def >>(situation: Situation): FEN = >>(SituationPlus(situation, 1))

  def >>(parsed: SituationPlus): FEN =
    parsed match {
      case SituationPlus(situation, _) =>
        >>(Game(situation, plies = parsed.plies, turnCount = parsed.turnCount))
    }

  def >>(game: Game): FEN = {
    val player = game.situation.player.fold('W', 'B')
    val board = exportBoard(game.situation.board)
    val H = game.situation.board.history.halfMoveClock
    val F = game.fullTurnCount
    FEN(s"${player}:${board}:H${H}:F${F}")
  }

  @nowarn def exportBoard(board: Board): String = {
    val pieces = board.pieces.groupBy({
      case (_, piece) => piece.player}).transform(
        (_, playerPcs) => playerPcs.map(
          {case (pos, pc) =>
            pos.key + (if (pc.role == Man) "" else "." + pc.role.forsyth)}
        ).toList.sorted.mkString(","))
      s"W${pieces(P1)}:B${pieces(P2)}"
    }

  def boardAndPlayer(situation: Situation): String =
    boardAndPlayer(situation.board, situation.player)

  def boardAndPlayer(board: Board, turnPlayer: Player): String =
    s"${turnPlayer.letter.toUpper}:${exportBoard(board)}"
}
