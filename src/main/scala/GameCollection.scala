package strategygames

import variant.Variant

//GameLogic is for the package in the strategygames codebase
//Used for routing the wrapper layer
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

  final case class Go() extends GameLogic {
    def id   = 5
    def name = "Go"
  }

  final case class Backgammon() extends GameLogic {
    def id   = 6
    def name = "Backgammon"
  }

  final case class Abalone() extends GameLogic {
    def id   = 7
    def name = "Abalone"
  }

  final case class Dameo() extends GameLogic {
    def id   = 8
    def name = "Dameo"
  }

  def all: List[GameLogic] =
    List(
      Chess(),
      Draughts(),
      FairySF(),
      Samurai(),
      Togyzkumalak(),
      Go(),
      Backgammon(),
      Abalone(),
      Dameo()
    )

  // TODO: I'm sure there is a better scala way of doing this
  def apply(id: Int): GameLogic = id match {
    case 1 => Draughts()
    case 2 => FairySF()
    case 3 => Samurai()
    case 4 => Togyzkumalak()
    case 5 => Go()
    case 6 => Backgammon()
    case 7 => Abalone()
    case 8 => Dameo()
    case _ => Chess()
  }
}

//GameFamily is for related games in one GameLogic
//Used for pieceset/boardthemes
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
  def playerFENChars: Map[Player, Char]

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
    def playerFENChars    = Map(P1 -> 'w', P2 -> 'b')
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
    def playerFENChars    = Map(P1 -> 'w', P2 -> 'b')
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
    def playerFENChars    = Map(P1 -> 'w', P2 -> 'b')
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
    def playerFENChars    = Map(P1 -> 'w', P2 -> 'b')
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
    def playerFENChars    = Map(P1 -> 'w', P2 -> 'b')
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
      List(
        "fabirovsky_flipello",
        "check_yb_flipello",
        "classic_flipello",
        "marble_bw_flipello",
        "stone_flipello",
        "disc_flipello"
      )
    def pieceSetDefault   = "disc_flipello"
    def boardThemes       = List("green", "green-marble", "wood")
    def boardThemeDefault = "green"
    def playerNames       = Map(P1 -> "Black", P2 -> "White")
    def playerColors      = Map(P1 -> "black", P2 -> "white")
    def playerFENChars    = Map(P1 -> 'w', P2 -> 'b')
  }

  final case class Amazons() extends GameFamily {
    def id                = 8
    def name              = "Amazons"
    def key               = "amazons"
    def gameLogic         = GameLogic.FairySF()
    def hasFishnet        = false
    def hasAnalysisBoard  = true
    def defaultVariant    = Variant.FairySF(strategygames.fairysf.variant.Amazons)
    def variants          = Variant.all(GameLogic.FairySF()).filter(_.gameFamily == this)
    def displayPiece      = "wQ"
    def pieceSetThemes    = List("arrow", "queen", "counter", "counter_arrow")
    def pieceSetDefault   = "arrow"
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
    def boardThemeDefault = "leather"
    def playerNames       = Map(P1 -> "White", P2 -> "Black")
    def playerColors      = Map(P1 -> "white", P2 -> "black")
    def playerFENChars    = Map(P1 -> 'w', P2 -> 'b')
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
    def playerFENChars    = Map(P1 -> 'S', P2 -> 'N')
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
    def playerFENChars    = Map(P1 -> 'S', P2 -> 'N')
  }

  final case class Go() extends GameFamily {
    def id                = 9
    def name              = "Go"
    def key               = "go"
    def gameLogic         = GameLogic.Go()
    def hasFishnet        = false
    def hasAnalysisBoard  = true
    def defaultVariant    = Variant.Go(strategygames.go.variant.Go19x19)
    def variants          = Variant.all(GameLogic.Go()).filter(_.gameFamily == this)
    def displayPiece      = "display"
    def pieceSetThemes    =
      List(
        "classic_stone",
        "cross"
      )
    def pieceSetDefault   = "classic_stone"
    def boardThemes       = List("light-wood", "dark-wood", "yellow-wood")
    def boardThemeDefault = "light-wood"
    def playerNames       = Map(P1 -> "Black", P2 -> "White")
    def playerColors      = Map(P1 -> "black", P2 -> "white")
    def playerFENChars    = Map(P1 -> 'b', P2 -> 'w')
  }

  final case class Backgammon() extends GameFamily {
    def id                = 10
    def name              = "Backgammon"
    def key               = "backgammon"
    def gameLogic         = GameLogic.Backgammon()
    def hasFishnet        = false
    def hasAnalysisBoard  = false
    def defaultVariant    = Variant.Backgammon(strategygames.backgammon.variant.Backgammon)
    def variants          = Variant.all(GameLogic.Backgammon()).filter(_.gameFamily == this)
    def displayPiece      = "display"
    def pieceSetThemes    = List("wooden", "classic", "contemporary")
    def pieceSetDefault   = "wooden"
    def boardThemes       = List("classic", "contemporary")
    def boardThemeDefault = "classic"
    def playerNames       = Map(P1 -> "White", P2 -> "Black")
    def playerColors      = Map(P1 -> "white", P2 -> "black")
    def playerFENChars    = Map(P1 -> 'w', P2 -> 'b')
  }

  final case class BreakthroughTroyka() extends GameFamily {
    def id                = 11
    def name              = "BreakthroughTroyka"
    def key               = "breakthroughtroyka"
    def gameLogic         = GameLogic.FairySF()
    def hasFishnet        = true
    def hasAnalysisBoard  = true
    def defaultVariant    = Variant.FairySF(strategygames.fairysf.variant.BreakthroughTroyka)
    def variants          = Variant.all(GameLogic.FairySF()).filter(_.gameFamily == this)
    def displayPiece      = "bP"
    def pieceSetThemes    = List("staunty", "checkers", "fabirovsky")
    def pieceSetDefault   = "staunty"
    def boardThemes       = List(
      "purple-diag",
      "maple"
    )
    def boardThemeDefault = "purple-diag"
    def playerNames       = Map(P1 -> "White", P2 -> "Black")
    def playerColors      = Map(P1 -> "white", P2 -> "black")
    def playerFENChars    = Map(P1 -> 'w', P2 -> 'b')
  }

  final case class Abalone() extends GameFamily {
    def id                = 12
    def name              = "Abalone"
    def key               = "abalone"
    def gameLogic         = GameLogic.Abalone()
    def hasFishnet        = false
    def hasAnalysisBoard  = true
    def defaultVariant    = Variant.Abalone(strategygames.abalone.variant.Abalone)
    def variants          = Variant.all(GameLogic.Abalone()).filter(_.gameFamily == this)
    def displayPiece      = "display"
    def pieceSetThemes    =
      List(
        "classic",
        "spatial",
        "newspaper",
        "fantasy"
      )
    def pieceSetDefault   = "classic"
    def boardThemes       = List(
      "classic",
      "dark",
      "yellow"
    )
    def boardThemeDefault = "classic"
    def playerNames       = Map(P1 -> "Black", P2 -> "White")
    def playerColors      = Map(P1 -> "black", P2 -> "white")
    def playerFENChars    = Map(P1 -> 'b', P2 -> 'w')
  }

  final case class Dameo() extends GameFamily {
    def id                = 13
    def name              = GameLogic.Dameo().name
    def key               = GameLogic.Dameo().name.toLowerCase()
    def gameLogic         = GameLogic.Dameo()
    def hasFishnet        = false
    def hasAnalysisBoard  = false
    def defaultVariant    = Variant.Dameo(strategygames.dameo.variant.Dameo)
    def variants          = Variant.all(GameLogic.Dameo())
    def displayPiece      = "wK"
    def pieceSetThemes    = List("wide_crown_dameo", "fabirovsky_dameo", "check_yb_dameo")
    def pieceSetDefault   = "wide_crown_dameo"
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
    def playerFENChars    = Map(P1 -> 'w', P2 -> 'b')
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
    Togyzkumalak(),
    Go(),
    Backgammon(),
    BreakthroughTroyka(),
    Abalone(),
    Dameo()
  )

  // TODO: I'm sure there is a better scala way of doing this
  def apply(id: Int): GameFamily = id match {
    case 1  => Draughts()
    case 2  => LinesOfAction()
    case 3  => Shogi()
    case 4  => Xiangqi()
    case 5  => Flipello()
    case 6  => Oware()
    case 7  => Togyzkumalak()
    case 8  => Amazons()
    case 9  => Go()
    case 10 => Backgammon()
    case 11 => BreakthroughTroyka()
    case 12 => Abalone()
    case 13 => Dameo()
    case _  => Chess()
  }

}

//GameGroups are for collections of related games
//A variant can be in multiple game groups!
//Used primarily for medley grouping atm
sealed abstract class GameGroup {
  def id: Int
  def name: String
  def key: String
  def variants: List[Variant]
  def medley: Boolean

  override def toString = s"GameGroup($name)"
}

object GameGroup {

  final case class Chess() extends GameGroup {
    def id       = 0
    def name     = "Chess"
    def key      = "chess"
    def variants = Variant.all(GameLogic.Chess()).filter(_.gameFamily.name == this.name)
    def medley   = true
  }

  final case class Draughts() extends GameGroup {
    def id       = 1
    def name     = "Draughts"
    def key      = "draughts"
    def variants = Variant.all(GameLogic.Draughts()) ::: Variant.all(GameLogic.Dameo())
    def medley   = true

  }

  final case class LinesOfAction() extends GameGroup {
    def id       = 2
    def name     = "Lines of Action"
    def key      = "loa"
    def variants = Variant.all(GameLogic.Chess()).filter(_.gameFamily == GameFamily.LinesOfAction())
    def medley   = true
  }

  final case class FairySF() extends GameGroup {
    def id       = 3
    def name     = "Fairy Stockfish"
    def key      = "fairysf"
    def variants = Variant.all(GameLogic.FairySF())
    def medley   = false
  }

  final case class Shogi() extends GameGroup {
    def id       = 4
    def name     = "Shogi"
    def key      = "shogi"
    def variants = Variant.all(GameLogic.FairySF()).filter(_.gameFamily.name == this.name)
    def medley   = true
  }

  final case class Xiangqi() extends GameGroup {
    def id       = 5
    def name     = "Xiangqi"
    def key      = "xiangqi"
    def variants = Variant.all(GameLogic.FairySF()).filter(_.gameFamily.name == this.name)
    def medley   = true
  }

  final case class Flipello() extends GameGroup {
    def id       = 6
    def name     = "Flipello"
    def key      = "flipello"
    def variants = Variant.all(GameLogic.FairySF()).filter(_.gameFamily.name == this.name)
    def medley   = true
  }

  final case class Mancala() extends GameGroup {
    def id       = 7
    def name     = "Mancala"
    def key      = "mancala"
    def variants = Variant.all(GameLogic.Togyzkumalak()) ::: Variant.all(GameLogic.Samurai())
    def medley   = true
  }

  final case class Amazons() extends GameGroup {
    def id       = 8
    def name     = "Amazons"
    def key      = "amazons"
    def variants = Variant.all(GameLogic.FairySF()).filter(_.gameFamily.name == this.name)
    def medley   = true
  }

  final case class Go() extends GameGroup {
    def id       = 9
    def name     = "Go"
    def key      = "go"
    def variants = Variant.all(GameLogic.Go()).filter(_.gameFamily.name == this.name)
    def medley   = true
  }

  final case class Backgammon() extends GameGroup {
    def id       = 10
    def name     = "Backgammon"
    def key      = "backgammon"
    def variants = Variant.all(GameLogic.Backgammon()).filter(_.gameFamily.name == this.name)
    def medley   = true
  }

  final case class BreakthroughTroyka() extends GameGroup {
    def id       = 11
    def name     = "BreakthroughTroyka"
    def key      = "breakthroughtroyka"
    def variants = Variant.all(GameLogic.FairySF()).filter(_.gameFamily.name == this.name)
    def medley   = true
  }

  final case class Abalone() extends GameGroup {
    def id       = 12
    def name     = "Abalone"
    def key      = "abalone"
    def variants = Variant.all(GameLogic.Abalone()).filter(_.gameFamily.name == this.name)
    def medley   = true
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
      Amazons(),
      Go(),
      Backgammon(),
      BreakthroughTroyka(),
      Abalone()
    )

  def medley: List[GameGroup] = all.filter(_.medley)

  // TODO: I'm sure there is a better scala way of doing this
  def apply(id: Int): GameGroup = id match {
    case 1  => Draughts()
    case 2  => LinesOfAction()
    case 3  => FairySF()
    case 4  => Shogi()
    case 5  => Xiangqi()
    case 6  => Flipello()
    case 7  => Mancala()
    case 8  => Amazons()
    case 9  => Go()
    case 10 => Backgammon()
    case 11 => BreakthroughTroyka()
    case 12 => Abalone()
    case _  => Chess()
  }
}
