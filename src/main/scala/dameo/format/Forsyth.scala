package strategygames.dameo
package format

import cats.implicits._

import scala.util.Try

import scala.annotation.nowarn

import strategygames.Player

import variant.{ Dameo, Variant }

//TODO Dameo Need to rewrite a lot of this
//Most of this has been copied out of Draughts gamelogic
//To demonstrate what goes on here, see also FEN.scala

/** Transform a game to draughts Forsyth Edwards Notation
  * https://en.wikipedia.org/wiki/Portable_Draughts_Notation Additions: Piece role g/p = Ghost man or king of
  * that player, has been captured but not removed because the forced capture sequence is not finished yet
  * ":Hx" = Halfmove clock: This is the number of halfmoves since a forced draw material combination appears.
  * This is used to determine if a draw can be claimed. ":Fx" = Fullmove number: The number of the full move.
  * It starts at 1, and is incremented after P2's move.
  */
object Forsyth {

  val initial =
    FEN(
      "W:Wa1,b1,b2,c1,c2,c3,d1,d2,d3,e1,e2,e3,f1,f2,f3,g1,g2,h1:Ba8,b7,b8,c6,c7,c8,d6,d7,d8,e6,e7,e8,f6,f7,f8,g7,g8,h8:H0:F1"
    )

  private def parseIntOption(str: String): Option[Int] =
    Try(Integer.parseInt(str)).toOption

  def <<@(variant: Variant, fen: FEN): Option[Situation] = {
    /* Convert a FEN + Variant into a Situation */
    Some(
      Situation(
        Board(
          pieces = fen.pieces,
          history = History(),
          variant = variant
        ),
        fen.player.get
      ).withHistory(
        History(
        )
      )
    )
  }

  /* Convert a FEN into a situation using the default variant */
  def <<(fen: FEN): Option[Situation] = <<@(Dameo, fen)

  case class SituationPlus(situation: Situation, fullTurnCount: Int) {
    def turnCount = fullTurnCount * 2 - (if (situation.player.p1) 2 else 1)
    // when we convert draughts to multiaction we should consider setting this
    // we may be able to deprecate this at that point as actions.flatten.size should count plies
    def plies     = turnCount

  }

  def <<<@(variant: Variant, fen: FEN): Option[SituationPlus] =
  /* Convert a FEN + Variant into a SituationPlus */
    <<@(variant, fen) map { sit =>
      val fullMoveNumber = fen.fullMove
      val halfMoveClock  = fen.halfMoveClock
      SituationPlus(
        halfMoveClock.map(sit.history.setHalfMoveClock).fold(sit)(sit.withHistory),
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

  @nowarn def compressedBoard(board: Board): String = ""

  def exportScanPosition(sit: Option[Situation]): String = sit.fold("")(_ => "")

  def shorten(fen: FEN): FEN = {
    val fen2 = if (fen.value.endsWith(":+0+0")) fen.value.dropRight(5) else fen.value
    if (fen2.endsWith(":H0:F1")) FEN(fen2.dropRight(6)) else FEN(fen2)
  }

  def getFullMove(fen: FEN): Option[Int] =
    fen.value.split(':') filter (s => s.length > 1 && s.charAt(0) == 'F') lift 0 flatMap parseIntOption

  def getPlayer(fen: FEN): Option[Player] = fen.value lift 0 flatMap Player.apply

  def getPly(fen: FEN): Option[Int] =
    getFullMove(fen) map { fullMove =>
      fullMove * 2 - (if (getPlayer(fen).exists(_.p1)) 2 else 1)
    }

}
