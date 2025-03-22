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
  * https://en.wikipedia.org/wiki/Portable_Draughts_Notation Additions: Piece role G/P = Ghost man or king of
  * that player, has been captured but not removed because the forced capture sequence is not finished yet
  * ":Hx" = Halfmove clock: This is the number of halfmoves since a forced draw material combination appears.
  * This is used to determine if a draw can be claimed. ":Fx" = Fullmove number: The number of the full move.
  * It starts at 1, and is incremented after P2's move.
  */
object Forsyth {

  val initial              =
    FEN(
      "W:Wa1,b1,b2,c1,c2,c3,d1,d2,d3,e1,e2,e3,f1,f2,f3,g1,g2,h1:Ba8,b7,b8,c6,c7,c8,d6,d7,d8,e6,e7,e8,f6,f7,f8,g7,g8,h8:H0:F1"
    )

  private def parseIntOption(str: String): Option[Int] =
    Try(Integer.parseInt(str)).toOption

  def <<@(variant: Variant, fen: FEN): Option[Situation] =
    makeBoard(variant, fen) map { board =>
      val situation = Player.apply(fen.value.charAt(0)) match {
        case Some(player) => Situation(board, player)
        case _            => Situation(board, P1)
      }

      situation withHistory {
        History(
          positionHashes = Array.empty
        )
      }

    }

  def <<(fen: FEN): Option[Situation] = <<@(Dameo, fen)

  case class SituationPlus(situation: Situation, fullTurnCount: Int) {

    def turnCount = fullTurnCount * 2 - (if (situation.player.p1) 2 else 1)
    // when we convert draughts to multiaction we should consider setting this
    // we may be able to deprecate this at that point as actions.flatten.size should count plies
    def plies     = turnCount

  }

  def <<<@(variant: Variant, fen: FEN): Option[SituationPlus] =
    <<@(variant, fen) map { sit =>
      val splitted       = fen.value.split(':')
      val fullMoveNumber = splitted find { s => s.length > 1 && s.charAt(0) == 'F' } flatMap { s =>
        parseIntOption(s drop 1)
      } map (_ max 1 min 500)
      val halfMoveClock  = splitted find { s => s.length > 1 && s.charAt(0) == 'H' } flatMap { s =>
        parseIntOption(s drop 1)
      } map (_ max 0 min 100)
      SituationPlus(
        halfMoveClock.map(sit.history.setHalfMoveClock).fold(sit)(sit.withHistory),
        fullMoveNumber | 1
      )
    }

  def <<<(fen: FEN): Option[SituationPlus] = <<<@(Dameo, fen)

  @nowarn def makeBoard(variant: Variant, fen: FEN): Option[Board] = None

  def toAlgebraic(variant: Variant, fen: FEN): Option[FEN] =
    <<<@(variant, fen) map { case parsed @ SituationPlus(situation, _) =>
      doExport(
        Game(situation, plies = parsed.plies, turnCount = parsed.turnCount),
        algebraic = true
      )
    }

  def countGhosts(fen: FEN): Int =
    fen.value.split(':').filter(_.nonEmpty).foldLeft(0) { (ghosts, line) =>
      Player.apply(line.charAt(0)).fold(ghosts) { _ =>
        line.drop(1).split(',').foldLeft(ghosts) { (lineGhosts, field) =>
          if (field.nonEmpty && "GP".indexOf(field.charAt(0)) != -1) lineGhosts + 1 else lineGhosts
        }
      }
    }

  def countKings(fen: FEN): Int =
    fen.value.split(':').filter(_.nonEmpty).foldLeft(0) { (kings, line) =>
      Player.apply(line.charAt(0)).fold(kings) { _ =>
        line.drop(1).split(',').foldLeft(kings) { (lineKings, field) =>
          if (field.nonEmpty && field.charAt(0) == 'K') lineKings + 1 else lineKings
        }
      }
    }

  def >>(situation: Situation): FEN = >>(SituationPlus(situation, 1))

  def >>(parsed: SituationPlus): FEN = parsed match {
    case SituationPlus(situation, _) =>
      >>(Game(situation, plies = parsed.plies, turnCount = parsed.turnCount))
  }

  def >>(game: Game): FEN = doExport(game, algebraic = false)

  private def doExport(game: Game, algebraic: Boolean): FEN = FEN {
    {
      List(
        game.player.letter.toUpper.toString,
        exportBoard(game.board, algebraic),
        "H" + game.halfMoveClock.toString,
        "F" + game.fullTurnCount.toString
      )
    } mkString ":"
  }

  def exportStandardPositionTurn(board: Board, ply: Int): String = List(
    Player(ply % 2 == 0).letter.toString,
    exportBoard(board)
  ) mkString ":"

  def exportKingMoves(board: Board) = board.history.kingMoves match {
    case KingMoves(p1, p2, p1King, p2King) =>
      s"+$p2${p2King.fold("")(_.toString)}+$p1${p1King.fold("")(_.toString)}"
  }

  @nowarn def exportBoard(board: Board, algebraic: Boolean = false): String = ""

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
