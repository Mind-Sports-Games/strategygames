package strategygames

import variant.Variant

sealed abstract class GameLogic {
  def id: Int
  def name: String

  override def toString = s"Lib($name)"
}

object GameLogic {

  final case class Chess() extends GameLogic {
    def id   = 0
    def name = "Chess"
  }

  final case class Draughts() extends GameLogic {
    def id   = 1
    def name = "Draughts"
  }

  final case class FairySF() extends GameLogic {
    def id   = 2
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
  def key: String
  def gameLogic: GameLogic
  def aiEnabled: Boolean
  def defaultVariant: Variant
  def variants: List[Variant]
  def displayPiece: String
  def pieceSetThemes: List[String]
  def pieceSetDefault: String
  def boardThemes: List[String]
  def boardThemeDefault: String
  def playerNames: Map[Player, String]
  def playerColors: Map[Player, String]

  override def toString = s"GameFamily($name)"
}

object GameFamily {

  final case class Chess() extends GameFamily {
    def id             = GameLogic.Chess().id
    def name           = GameLogic.Chess().name
    def key            = GameLogic.Chess().name.toLowerCase()
    def gameLogic      = GameLogic.Chess()
    def aiEnabled      = true
    def defaultVariant = Variant.Chess(strategygames.chess.variant.Standard)
    def variants       = Variant.all(GameLogic.Chess()).filter(_.gameFamily == this)
    def displayPiece   = "wN"
    def pieceSetThemes = List(
      "cburnett",
      "merida",
      "alpha",
      "california",
      "cardinal",
      "chess7",
      "chessnut",
      "companion",
      "dubrovny",
      "fantasy",
      "fresca",
      "gioco",
      "governor",
      "horsey",
      "icpieces",
      "kosal",
      "leipzig",
      "letter",
      "maestro",
      "pirouetti",
      "pixel",
      "reillycraig",
      "riohacha",
      "shapes",
      "spatial",
      "staunty",
      "tatiana"
    )
    def pieceSetDefault = "staunty"
    def boardThemes = List(
      "blue",
      "blue2",
      "blue3",
      "blue-marble",
      "canvas",
      "wood",
      "wood2",
      "wood3",
      "wood4",
      "maple",
      "maple2",
      "brown",
      "leather",
      "green",
      "marble",
      "green-plastic",
      "grey",
      "metal",
      "olive",
      "newspaper",
      "purple",
      "purple-diag",
      "pink",
      "ic",
      "horsey"
    )
    def boardThemeDefault = "maple"
    def playerNames       = Map(P1 -> "White", P2 -> "Black")
    def playerColors      = Map(P1 -> "white", P2 -> "black")
  }

  final case class Draughts() extends GameFamily {
    def id              = GameLogic.Draughts().id
    def name            = GameLogic.Draughts().name
    def key             = GameLogic.Draughts().name.toLowerCase()
    def gameLogic       = GameLogic.Draughts()
    def aiEnabled       = false
    def defaultVariant  = Variant.Draughts(strategygames.draughts.variant.Standard)
    def variants        = Variant.all(GameLogic.Draughts())
    def displayPiece    = "wK"
    def pieceSetThemes  = List("wide_crown", "fabirovsky", "check_yb")
    def pieceSetDefault = "wide_crown"
    def boardThemes = List(
      "blue",
      "blue2",
      "blue3",
      "canvas",
      "wood",
      "wood2",
      "wood3",
      "maple",
      "brown",
      "leather",
      "green",
      "marble",
      "grey",
      "metal",
      "olive",
      "purple"
    )
    def boardThemeDefault = "blue3"
    def playerNames       = Map(P1 -> "White", P2 -> "Black")
    def playerColors      = Map(P1 -> "white", P2 -> "black")
  }

  final case class LinesOfAction() extends GameFamily {
    def id              = 2
    def name            = "Lines Of Action"
    def key             = "loa"
    def gameLogic       = GameLogic.Chess()
    def aiEnabled       = false
    def defaultVariant  = Variant.Chess(strategygames.chess.variant.LinesOfAction)
    def variants        = Variant.all(GameLogic.Chess()).filter(_.gameFamily == this)
    def displayPiece    = "bL"
    def pieceSetThemes  = List("fabirovsky_loa", "check_yb_loa", "wide")
    def pieceSetDefault = "check_yb_loa"
    def boardThemes = List(
      "blue",
      "blue2",
      "blue3",
      "blue-marble",
      "canvas",
      "wood",
      "wood2",
      "wood3",
      "wood4",
      "maple",
      "maple2",
      "brown",
      "leather",
      "green",
      "marble",
      "green-plastic",
      "grey",
      "metal",
      "olive",
      "newspaper",
      "purple",
      "purple-diag",
      "pink",
      "ic",
      "horsey"
    )
    def boardThemeDefault = "marble"
    def playerNames       = Map(P1 -> "Black", P2 -> "White")
    def playerColors      = Map(P1 -> "black", P2 -> "white")
  }

  final case class Shogi() extends GameFamily {
    def id                = 3
    def name              = "Shogi"
    def key               = "shogi"
    def gameLogic         = GameLogic.FairySF()
    def aiEnabled         = false
    def defaultVariant    = Variant.FairySF(strategygames.fairysf.variant.Shogi)
    def variants          = Variant.all(GameLogic.FairySF()).filter(_.gameFamily == this)
    def displayPiece      = "0KE"
    def pieceSetThemes    = List("2kanji", "ctw")
    def pieceSetDefault   = "2kanji"
    def boardThemes       = List("wood", "clear")
    def boardThemeDefault = "wood"
    def playerNames       = Map(P1 -> "Sente", P2 -> "Gote")
    def playerColors      = Map(P1 -> "black", P2 -> "white")
  }

  final case class Xiangqi() extends GameFamily {
    def id                = 4
    def name              = "Xiangqi"
    def key               = "xiangqi"
    def gameLogic         = GameLogic.FairySF()
    def aiEnabled         = false
    def defaultVariant    = Variant.FairySF(strategygames.fairysf.variant.Xiangqi)
    def variants          = Variant.all(GameLogic.FairySF()).filter(_.gameFamily == this)
    def displayPiece      = "RH"
    def pieceSetThemes    = List("2dhanzi", "ka")
    def pieceSetDefault   = "2dhanzi"
    def boardThemes       = List("grey", "green")
    def boardThemeDefault = "green"
    def playerNames       = Map(P1 -> "White", P2 -> "Black")
    def playerColors      = Map(P1 -> "white", P2 -> "black")
  }

  final case class Flipello() extends GameFamily {
    def id             = 5
    def name           = "Flipello"
    def key            = "flipello"
    def gameLogic      = GameLogic.FairySF()
    def aiEnabled      = false
    def defaultVariant = Variant.FairySF(strategygames.fairysf.variant.Flipello)
    def variants       = Variant.all(GameLogic.FairySF()).filter(_.gameFamily == this)
    def displayPiece   = "bP"
    def pieceSetThemes =
      List("fabirovsky_flipello", "check_yb_flipello", "classic_flipello", "marble_bw_flipello")
    def pieceSetDefault   = "fabirovsky_flipello"
    def boardThemes       = List("green", "green-marble", "wood")
    def boardThemeDefault = "green"
    def playerNames       = Map(P1 -> "Black", P2 -> "White")
    def playerColors      = Map(P1 -> "black", P2 -> "white")
  }

  def all: List[GameFamily] = List(
    Chess(),
    Draughts(),
    LinesOfAction(),
    Shogi(),
    Xiangqi(),
    Flipello()
  )

  // TODO: I'm sure there is a better scala way of doing this
  def apply(id: Int): GameFamily = id match {
    case 1 => Draughts()
    case 2 => LinesOfAction()
    case 3 => Shogi()
    case 4 => Xiangqi()
    case 5 => Flipello()
    case _ => Chess()
  }

}
