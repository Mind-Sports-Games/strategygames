package strategygames.fiveinarow

import org.specs2.mutable.Specification

import strategygames.{
  Action => StratAction,
  Game => StratGame,
  GameFamily,
  GameGroup,
  GameLogic,
  MoveMetrics,
  Player,
  Replay => StratReplay,
  Role => StratRole
}
import strategygames.format.{ FEN => StratFEN, Forsyth => StratForsyth, Uci => StratUci }
import strategygames.format.pgn.{ Binary, Dumper }
import strategygames.variant.{ Variant => StratVariant }

class FiveInARowWrapperTest extends Specification {

  val lib                        = GameLogic.FiveInARow()
  val gf                         = GameFamily.FiveInARow()
  val stratVariant: StratVariant = StratVariant.FiveInARow(strategygames.fiveinarow.variant.Gomoku)

  private def applyAll(game: StratGame, ucis: String*): StratGame =
    ucis.foldLeft(game) { (g, str) =>
      val uci = StratUci.apply(lib, gf, str).getOrElse(sys.error(s"unreadable uci ${str}"))
      g.applyUci(uci, MoveMetrics()).fold(err => sys.error(s"${str} refused: ${err}"), _._1)
    }

  "the collection" should {

    "register the logic, family and group under one name" in {
      GameLogic(10) === lib
      GameFamily(15) === gf
      GameGroup(14) === GameGroup.FiveInARow()
      gf.key === "fiveinarow"
      GameGroup.FiveInARow().variants === List(stratVariant)
    }

    "find gomoku by key and as the family default" in {
      StratVariant(lib, "gomoku") === Some(stratVariant)
      gf.defaultVariant === stratVariant
      stratVariant.canOfferDraw must beTrue
      stratVariant.onlyDropsVariant must beTrue
    }
  }

  "the wrapper" should {

    "build a game with P1 to move" in {
      val g = StratGame.apply(lib, stratVariant)
      g.situation.board.variant.gameFamily === gf
      g.player === Player.P1
      g.situation.canSwap must beFalse
    }

    "list the two stone roles" in {
      StratRole.all(lib).map(_.forsyth) === List('B', 'W')
    }

    "hash a situation" in {
      strategygames.Hash(lib, StratGame.apply(lib, stratVariant).situation).length === 3
    }

    "read the initial fen" in {
      StratForsyth.initial(lib).value === strategygames.fiveinarow.format.Forsyth.initial.value
      StratFEN.apply(lib, StratForsyth.initial(lib).value).player === Some(Player.P1)
    }

    "parse each action string into a wrapped Uci" in {
      StratUci.apply(lib, gf, "B@h8") must beSome[StratUci]
      StratUci.apply(lib, gf, "W@o15") must beSome[StratUci]
      StratUci.apply(lib, gf, "swap") must beSome[StratUci]
      StratUci.apply(lib, gf, "swap2") must beSome[StratUci]
      StratUci.apply(lib, gf, "pass") must beNone
    }
  }

  "an opening played through the wrapper" should {

    val opened = applyAll(StratGame.apply(lib, stratVariant), "B@h8", "W@h9", "B@i8")

    "hand the choice to P2 with swap and swap2 on offer" in {
      opened.player === Player.P2
      opened.situation.canSwap must beTrue
      opened.situation.canSwap2 must beTrue
      opened.situation.actions.map(_.toUci.uci) must contain("swap", "swap2")
    }

    "swap and pass the turn" in {
      val (g, swap) = opened.swap().toOption.get
      g.player === Player.P1
      swap.toUci.uci === "swap"
      Dumper(lib, (swap: StratAction)) === "swap"
    }

    "swap2 without passing the turn, then end it after two drops" in {
      val (g, swap2) = opened.swap2().toOption.get
      g.player === Player.P2
      Dumper(lib, (swap2: StratAction)) === "swap2"
      val after      = applyAll(g, "W@j8", "B@j9")
      after.player === Player.P1
      after.situation.canSwap must beTrue
      after.actionStrs === Vector(Vector("B@h8", "W@h9", "B@i8"), Vector("swap2", "W@j8", "B@j9"))
    }
  }

  "a recorded game" should {

    val actions = Vector(
      Vector("B@h8", "W@h9", "B@i8"),
      Vector("swap2", "W@j8", "B@j9"),
      Vector("swap"),
      Vector("W@k8"),
      Vector("B@a1")
    )

    "round trip through the binary format" in {
      val bytes = Binary.writeActionStrs(gf, actions)
      bytes.isSuccess must beTrue
      Binary.readActionStrs(lib, bytes.get.toList).get === actions
    }

    "replay to the position the game reached" in {
      val played   = applyAll(StratGame.apply(lib, stratVariant), actions.flatten*)
      val replayed = StratReplay.gameFromUciStrings(lib, actions, Player.P2, None, stratVariant)
      replayed.toOption.map(g => StratForsyth.>>(lib, g).value) ===
        Some(StratForsyth.>>(lib, played).value)
    }
  }
}
