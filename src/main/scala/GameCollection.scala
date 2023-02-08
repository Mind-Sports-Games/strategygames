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

  final case class Samurai() extends GameLogic {
    def id   = 3
    def name = "Samurai"
  }

  final case class Togyzkumalak() extends GameLogic {
    def id   = 4
    def name = "Togyzkumalak"
  }

  def all: List[GameLogic] = List(Chess(), Draughts(), FairySF(), Samurai(), Togyzkumalak())

  // TODO: I'm sure there is a better scala way of doing this
  def apply(id: Int): GameLogic = id match {
    case 1 => Draughts()
    case 2 => FairySF()
    case 3 => Samurai()
    case 4 => Togyzkumalak()
    case _ => Chess()
  }
}

sealed abstract class GameFamily {
  def id: Int
  def name: String
  def key: String
  def gameLogic: GameLogic
  def hasFishnet: Boolean
  def hasAnalysisBoard: Boolean
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
    def id                = GameLogic.Chess().id
    def name              = GameLogic.Chess().name
    def key               = GameLogic.Chess().name.toLowerCase()
    def gameLogic         = GameLogic.Chess()
    def hasFishnet        = true
    def hasAnalysisBoard  = true
    def defaultVariant    = Variant.Chess(strategygames.chess.variant.Standard)
    def variants          = Variant.all(GameLogic.Chess()).filter(_.gameFamily == this)
    def displayPiece      = "wN"
    def pieceSetThemes    = List(
      "cburnett",
      "merida",
      "alpha",
      "california",
      "cardinal",
      "companion",
      "dubrovny",
      "fantasy",
      "fresca",
      "gioco",
      "governor",
      "kosal",
      "leipzig",
      "letter",
      "maestro",
      "pirouetti",
      "reillycraig",
      "shapes",
      "spatial",
      "staunty",
      "tatiana"
    )
    def pieceSetDefault   = "staunty"
    def boardThemes       = List(
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
    def id                = GameLogic.Draughts().id
    def name              = GameLogic.Draughts().name
    def key               = GameLogic.Draughts().name.toLowerCase()
    def gameLogic         = GameLogic.Draughts()
    def hasFishnet        = false
    def hasAnalysisBoard  = false
    def defaultVariant    = Variant.Draughts(strategygames.draughts.variant.Standard)
    def variants          = Variant.all(GameLogic.Draughts())
    def displayPiece      = "wK"
    def pieceSetThemes    = List("wide_crown", "fabirovsky", "check_yb")
    def pieceSetDefault   = "wide_crown"
    def boardThemes       = List(
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
    def id                = 2
    def name              = "Lines Of Action"
    def key               = "loa"
    def gameLogic         = GameLogic.Chess()
    def hasFishnet        = false
    def hasAnalysisBoard  = true
    def defaultVariant    = Variant.Chess(strategygames.chess.variant.LinesOfAction)
    def variants          = Variant.all(GameLogic.Chess()).filter(_.gameFamily == this)
    def displayPiece      = "bL"
    def pieceSetThemes    = List("fabirovsky_loa", "check_yb_loa", "wide")
    def pieceSetDefault   = "check_yb_loa"
    def boardThemes       = List(
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
    def hasFishnet        = true
    def hasAnalysisBoard  = true
    def defaultVariant    = Variant.FairySF(strategygames.fairysf.variant.Shogi)
    def variants          = Variant.all(GameLogic.FairySF()).filter(_.gameFamily == this)
    def displayPiece      = "0KE"
    def pieceSetThemes    = List("2kanji", "1kanji", "ctw")
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
    def hasFishnet        = true
    def hasAnalysisBoard  = true
    def defaultVariant    = Variant.FairySF(strategygames.fairysf.variant.Xiangqi)
    def variants          = Variant.all(GameLogic.FairySF()).filter(_.gameFamily == this)
    def displayPiece      = "RH"
    def pieceSetThemes    = List("2dhanzi", "ka")
    def pieceSetDefault   = "2dhanzi"
    def boardThemes       = List("grey", "green")
    def boardThemeDefault = "green"
    def playerNames       = Map(P1 -> "Red", P2 -> "Black")
    def playerColors      = Map(P1 -> "white", P2 -> "black")
  }

  final case class Flipello() extends GameFamily {
    def id                = 5
    def name              = "Flipello"
    def key               = "flipello"
    def gameLogic         = GameLogic.FairySF()
    def hasFishnet        = true
    def hasAnalysisBoard  = true
    def defaultVariant    = Variant.FairySF(strategygames.fairysf.variant.Flipello)
    def variants          = Variant.all(GameLogic.FairySF()).filter(_.gameFamily == this)
    def displayPiece      = "bP"
    def pieceSetThemes    =
      List("fabirovsky_flipello", "check_yb_flipello", "classic_flipello", "marble_bw_flipello")
    def pieceSetDefault   = "fabirovsky_flipello"
    def boardThemes       = List("green", "green-marble", "wood")
    def boardThemeDefault = "green"
    def playerNames       = Map(P1 -> "Black", P2 -> "White")
    def playerColors      = Map(P1 -> "black", P2 -> "white")
  }

  final case class Amazons() extends GameFamily {
    def id                = 8
    def name              = "Amazons"
    def key               = "amazons"
    def gameLogic         = GameLogic.FairySF()
    def hasFishnet        = true
    def hasAnalysisBoard  = true
    def defaultVariant    = Variant.FairySF(strategygames.fairysf.variant.Amazons)
    def variants          = Variant.all(GameLogic.FairySF()).filter(_.gameFamily == this)
    def displayPiece      = "wQ"
    def pieceSetThemes    = List("queen")
    def pieceSetDefault   = "queen"
    def boardThemes       = List("blue", "brown", "grey")
    def boardThemeDefault = "grey"
    def playerNames       = Map(P1 -> "White", P2 -> "Black")
    def playerColors      = Map(P1 -> "white", P2 -> "black")
  }

  final case class Oware() extends GameFamily {
    def id                = 6
    def name              = "Oware"
    def key               = "oware"
    def gameLogic         = GameLogic.Samurai()
    def hasFishnet        = false
    def hasAnalysisBoard  = true
    def defaultVariant    = Variant.Samurai(strategygames.samurai.variant.Oware)
    def variants          = Variant.all(GameLogic.Samurai()).filter(_.gameFamily == this)
    def displayPiece      = "display"
    def pieceSetThemes    =
      List(
        "green",
        "blue",
        "red",
        "grey",
        "green_seed",
        "green_numbers"
      )
    def pieceSetDefault   = "green"
    def boardThemes       = List("light-wood", "dark-wood")
    def boardThemeDefault = "light-wood"
    def playerNames       = Map(P1 -> "South", P2 -> "North")
    def playerColors      = Map(P1 -> "white", P2 -> "black")
  }

  final case class Togyzkumalak() extends GameFamily {
    def id                = 7
    def name              = "Togyzkumalak"
    def key               = "togyzkumalak"
    def gameLogic         = GameLogic.Togyzkumalak()
    def hasFishnet        = false
    def hasAnalysisBoard  = true
    def defaultVariant    = Variant.Togyzkumalak(strategygames.togyzkumalak.variant.Togyzkumalak)
    def variants          = Variant.all(GameLogic.Togyzkumalak()).filter(_.gameFamily == this)
    def displayPiece      = "display"
    def pieceSetThemes    =
      List(
        "black_gloss"
      )
    def pieceSetDefault   = "black_gloss"
    def boardThemes       = List("blue", "wood")
    def boardThemeDefault = "blue"
    def playerNames       = Map(P1 -> "Bastaushi", P2 -> "Kostaushi")
    def playerColors      = Map(P1 -> "white", P2 -> "black")
  }

  def all: List[GameFamily] = List(
    Chess(),
    Draughts(),
    LinesOfAction(),
    Shogi(),
    Xiangqi(),
    Flipello(),
    Amazons(),
    Oware(),
    Togyzkumalak()
  )

  // TODO: I'm sure there is a better scala way of doing this
  def apply(id: Int): GameFamily = id match {
    case 1 => Draughts()
    case 2 => LinesOfAction()
    case 3 => Shogi()
    case 4 => Xiangqi()
    case 5 => Flipello()
    case 6 => Oware()
    case 7 => Togyzkumalak()
    case 8 => Amazons()
    case _ => Chess()
  }

}

sealed abstract class GameGroup {
  def id: Int
  def name: String
  def key: String
  def variants: List[Variant]

  override def toString = s"GameGroup($name)"
}

object GameGroup {

  final case class Chess() extends GameGroup {
    def id       = 0
    def name     = "Chess"
    def key      = "chess"
    def variants = Variant.all(GameLogic.Chess()).filter(_.gameFamily.name == this.name)
  }

  final case class Draughts() extends GameGroup {
    def id       = 1
    def name     = "Draughts"
    def key      = "draughts"
    def variants = Variant.all(GameLogic.Draughts())

  }

  final case class LinesOfAction() extends GameGroup {
    def id       = 2
    def name     = "Lines of Action"
    def key      = "loa"
    def variants = Variant.all(GameLogic.Chess()).filter(_.gameFamily == GameFamily.LinesOfAction())
  }

  final case class FairySF() extends GameGroup {
    def id       = 3
    def name     = "Fairy Stockfish"
    def key      = "fairysf"
    def variants = Variant.all(GameLogic.FairySF())
  }

  final case class Shogi() extends GameGroup {
    def id       = 4
    def name     = "Shogi"
    def key      = "shogi"
    def variants = Variant.all(GameLogic.FairySF()).filter(_.gameFamily.name == this.name)
  }

  final case class Xiangqi() extends GameGroup {
    def id       = 5
    def name     = "Xiangqi"
    def key      = "xiangqi"
    def variants = Variant.all(GameLogic.FairySF()).filter(_.gameFamily.name == this.name)
  }

  final case class Flipello() extends GameGroup {
    def id       = 6
    def name     = "Flipello"
    def key      = "flipello"
    def variants = Variant.all(GameLogic.FairySF()).filter(_.gameFamily.name == this.name)
  }

  final case class Mancala() extends GameGroup {
    def id       = 7
    def name     = "Mancala"
    def key      = "mancala"
    def variants = Variant.all(GameLogic.Togyzkumalak()) ::: Variant.all(GameLogic.Samurai())
  }

  final case class Amazons() extends GameGroup {
    def id       = 8
    def name     = "Amazons"
    def key      = "amazons"
    def variants = Variant.all(GameLogic.FairySF()).filter(_.gameFamily.name == this.name)
  }

  def all: List[GameGroup] =
    List(
      Chess(),
      Draughts(),
      LinesOfAction(),
      FairySF(),
      Shogi(),
      Xiangqi(),
      Flipello(),
      Mancala(),
      Amazons()
    )

  def medley: List[GameGroup] =
    List(Chess(), Draughts(), LinesOfAction(), Shogi(), Xiangqi(), Flipello(), Mancala())

  // TODO: I'm sure there is a better scala way of doing this
  def apply(id: Int): GameGroup = id match {
    case 1 => Draughts()
    case 2 => LinesOfAction()
    case 3 => FairySF()
    case 4 => Shogi()
    case 5 => Xiangqi()
    case 6 => Flipello()
    case 7 => Mancala()
    case 8 => Amazons()
    case _ => Chess()
  }
}
