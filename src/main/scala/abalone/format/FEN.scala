package strategygames.abalone.format

import strategygames.Player
import strategygames.abalone.Piece
import strategygames.abalone.PieceMap
import strategygames.abalone.Pos
import strategygames.abalone.Stone
import strategygames.abalone.File

// FEN is described from bottomLeft to topRight
final case class FEN(value: String) extends AnyVal {
  // Belgian Daisy: pp1PP/pppPPP/1pp1PP1/8/9/8/1PP1pp1/PPPppp/PP1pp 0 0 b 0 0
  // Snakes: PPPPP/5P/6P/1ppppp1P/1p5P1/p1PPPPP1/p6/p5/ppppp 0 0 b 0 0

  override def toString = value

  def pieces: PieceMap = {
/*
  squared hex representation of the Belgian Daisy FEN :
                                          row   col
9 -   72  73  74  75 'P' 'P' '1' 'p' 'p'   8:   >3 (<9)
8 -   63  64  65 'P' 'P' 'P' 'p' 'p' 'p'   7:   >2 (<9) next line : +4
7 -   54  55 '1' 'P' 'P' '1' 'p' 'p' '1'   6:   >1 (<9) next line : +3
6 -   45         '8'                       5:   >0 (<9) next line : +2
5 -              '9'                       4:   <9 next line : +1
4 -              '8'                 35    3:   <8 next line : +1
3 -  '1' 'p' 'p' '1' 'P' 'P' '1' 25  26    2:   <7 next line : +2
2 -  'p' 'p' 'p' 'P' 'P' 'P' 15  16  17    1:   <6 next line : +3
1 -  'p' 'p' '1' 'P' 'P'  5   6   7   8    0:   <5 next line : +4
      |   |   |   |   |   |   |   |   |
      A   B   C   D   E   F   G   H   I
 */
    val position     = value.split(' ')(0)
    var currentIndex = 0
    var currentLine = 0
    position.map {
      case 'p' => {
        currentIndex = currentIndex + 1
        Some((Pos.apply(currentIndex - 1), new Piece(strategygames.abalone.P1, Stone)))
      }
      case 'P' => {
        currentIndex = currentIndex + 1
        Some((Pos.apply(currentIndex - 1), new Piece(strategygames.abalone.P2, Stone)))
      }
      case d if d.isDigit => {
        currentIndex = currentIndex + d.asDigit
        None
      }
      case '/' => {
        currentLine = currentLine + 1
        currentIndex =  if(currentLine < 5) {
          currentIndex + (File.all.size / 2 + 1) - (currentLine)
        }
        else {
          currentIndex + Math.abs((File.all.size / 2 + 1) - currentLine - 1)
        }
        None
      }
    }.flatten.flatMap { // flatMap = map + flatten, which gets rid of None and returns the Object instead of a Some(Object) which we would get with map
      case (Some(pos), piece) => Some(pos -> piece)
      case _ => None
    }.toMap
  }

  def player1Score: Int = intFromFen(1).getOrElse(0)

  def player2Score: Int = intFromFen(2).getOrElse(0)

  def player: Option[Player] =
    value.split(' ').lift(3) flatMap (_.headOption) flatMap  Player.apply map( !_ )

  def halfMovesSinceLastCapture: Option[Int] = intFromFen(4)

  def fullMove: Option[Int] = intFromFen(5)

  def ply: Option[Int] =
    fullMove map { fm =>
      fm * 2 - (if (player.exists(_.p1)) 2 else 1)
    }

  def initial = value == Forsyth.initial.value

  private def intFromFen(index: Int): Option[Int] =
    value.split(' ').lift(index).flatMap(_.toIntOption)
}

object FEN {

  def clean(source: String): FEN = FEN(source.replace("_", " ").trim)
}
