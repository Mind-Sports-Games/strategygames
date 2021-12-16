package strategygames

import variant.Variant

sealed abstract class GameLogic {
  def id: Int
  def name: String

  override def toString = s"Lib($name)"
}

object GameLogic {

  final case class Chess() extends GameLogic {
    def id = 0
    def name = "Chess"
  }

  final case class Draughts() extends GameLogic {
    def id = 1
    def name = "Draughts"
  }

  final case class FairySF() extends GameLogic {
    def id = 2
    def name = "Fairy Stockfish"
  }

  def all: List[GameLogic] = List(Chess(), Draughts())

  // TODO: I'm sure there is a better scala way of doing this
  def apply(id: Int): GameLogic = id match {
    case 1 => Draughts()
    case 2 => FairySF()
    case _ => Chess()
  }
}

sealed abstract class GameFamily {
  def id: Int
  def name: String
  def shortName: String
  def gameLogic: GameLogic
  def aiEnabled: Boolean
  def defaultVariant: Variant
  def variants: List[Variant]
  def displayPiece: String
  def pieceSetThemes: List[String]
  def pieceSetDefault: String

  override def toString = s"GameFamily($name)"
}

object GameFamily {

  final case class Chess() extends GameFamily {
    def id = GameLogic.Chess().id
    def name = GameLogic.Chess().name
    def shortName = GameLogic.Chess().name
    def gameLogic = GameLogic.Chess()
    def aiEnabled = true
    def defaultVariant = Variant.Chess(strategygames.chess.variant.Standard)
    def variants = Variant.all(GameLogic.Chess()).filter(_.gameFamily == this)
    def displayPiece = "wN"
    def pieceSetThemes = List("cburnett", "merida", "alpha", "california", "cardinal", "chess7",
                              "chessnut", "companion", "dubrovny", "fantasy", "fresca", "gioco",
                              "governor", "horsey", "icpieces", "kosal", "leipzig", "letter", "maestro",
                              "pirouetti", "pixel", "reillycraig", "riohacha", "shapes", "spatial",
                              "staunty", "tatiana")
    def pieceSetDefault= "cburnett"
  }

  final case class Draughts() extends GameFamily {
    def id = GameLogic.Draughts().id
    def name = GameLogic.Draughts().name
    def shortName = GameLogic.Draughts().name
    def gameLogic = GameLogic.Draughts()
    def aiEnabled = false
    def defaultVariant = Variant.Draughts(strategygames.draughts.variant.Standard)
    def variants = Variant.all(GameLogic.Draughts())
    def displayPiece = "wK"
    def pieceSetThemes = List("wide_crown", "fabirovsky", "check_yb")
    def pieceSetDefault= "wide_crown"
  }

  final case class LinesOfAction() extends GameFamily {
    def id = 2
    def name = "Lines Of Action"
    def shortName = "LOA"
    def gameLogic = GameLogic.Chess()
    def aiEnabled = false
    def defaultVariant = Variant.Chess(strategygames.chess.variant.LinesOfAction)
    def variants = Variant.all(GameLogic.Chess()).filter(_.gameFamily == this)
    def displayPiece = "wL"
    def pieceSetThemes = List("fabirovsky_loa", "check_yb_loa", "wide")
    def pieceSetDefault= "fabirovsky_loa"
  }

  final case class Shogi() extends GameFamily {
    def id = 3
    def name = "Shogi"
    def shortName = "Shogi"
    def gameLogic = GameLogic.FairySF()
    def aiEnabled = false
    def defaultVariant = Variant.FairySF(strategygames.fairysf.variant.Shogi)
    def variants = Variant.all(GameLogic.FairySF()).filter(_.gameFamily == this)
    def displayPiece = "0KE"
    def pieceSetThemes = List("2kanji", "ctw")
    def pieceSetDefault= "2kanji"
  }

  final case class Xiangqi() extends GameFamily {
    def id = 4
    def name = "Xiangqi"
    def shortName = "Xiangqi"
    def gameLogic = GameLogic.FairySF()
    def aiEnabled = false
    def defaultVariant = Variant.FairySF(strategygames.fairysf.variant.Xiangqi)
    def variants = Variant.all(GameLogic.FairySF()).filter(_.gameFamily == this)
    def displayPiece = "RH"
    def pieceSetThemes = List("2dhanzi", "ka")
    def pieceSetDefault= "2dhanzi"
  }

  def all: List[GameFamily] = List(
    Chess(),
    Draughts(),
    LinesOfAction(),
    Shogi(),
    Xiangqi()
  )

  // TODO: I'm sure there is a better scala way of doing this
  def apply(id: Int): GameFamily = id match {
    case 1 => Draughts()
    case 2 => LinesOfAction()
    case 3 => Shogi()
    case 4 => Xiangqi()
    case _ => Chess()
  }

}
