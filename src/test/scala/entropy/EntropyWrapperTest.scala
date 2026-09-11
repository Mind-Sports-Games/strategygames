package strategygames.entropy

import org.specs2.mutable.Specification

import strategygames.{
  Action => StratAction,
  Game => StratGame,
  GameFamily,
  GameLogic,
  Player,
  Role => StratRole
}
import strategygames.format.{ FEN => StratFEN, Forsyth => StratForsyth, Uci => StratUci }
import strategygames.format.pgn.{ Binary, Dumper }
import strategygames.variant.{ Variant => StratVariant }

// Entropy reached through the wrapper API rather than its own package. The wrapper
// dispatches on GameLogic, and a missing case there is a MatchError at runtime that no
// amount of testing inside the entropy package would catch.
class EntropyWrapperTest extends Specification {

  val lib                        = GameLogic.Entropy()
  val gf                         = GameFamily.Entropy()
  val stratVariant: StratVariant = StratVariant.Entropy(strategygames.entropy.variant.Entropy)

  "the wrapper" should {

    "build a game and report the entropy game family" in {
      val g = StratGame.apply(lib, stratVariant)
      g.situation.board.variant.gameFamily === gf
      g.player === Player.P1
    }

    "list roles, and find them by forsyth character" in {
      StratRole.all(lib).size === 7
      StratRole.allByForsyth(lib).get('k').map(_.name) === Some("Black")
      StratRole.storable(lib).size === 7
    }

    "make a piece from a forsyth character" in {
      strategygames.Piece.fromChar(lib, gf, 'R').map(_.role.name) === Some("Red")
    }

    "hash a situation without falling over" in {
      val g = StratGame.apply(lib, stratVariant)
      strategygames.Hash(lib, g.situation).length === 3
    }

    "give the initial fen and read it back" in {
      StratForsyth.initial(lib).value === "7/7/7/7/7/7/7[] w 0 0 1 1"
      StratFEN.apply(lib, "7/7/7/7/7/7/7[] w 0 0 1 1").player === Some(Player.P1)
    }

    "build a move uci from two wrapped positions" in {
      StratUci.Move
        .apply(lib, strategygames.Pos.Entropy(Pos.D4), strategygames.Pos.Entropy(Pos.E4), None)
        .uci === "d4e4"
    }

    "parse each action string into a wrapped Uci" in {
      StratUci.apply(lib, gf, "draw-r") must beSome[StratUci]
      StratUci.apply(lib, gf, "r@d4") must beSome[StratUci]
      StratUci.apply(lib, gf, "d4e4") must beSome[StratUci]
      StratUci.apply(lib, gf, "pass") must beSome[StratUci]
    }
  }

  "a turn played through the wrapper" should {

    "draw, drop, then pass" in {
      val start = StratGame.apply(lib, stratVariant)

      val drawn = start.drawCounter(StratRole.EntropyRole(Red))
      drawn.toOption must beSome[(StratGame, strategygames.DrawCounter)]

      val afterDraw = drawn.toOption.get._1
      // still Chaos's turn: the counter has been revealed but not yet placed
      afterDraw.player === Player.P1
      val dropped   = afterDraw.drop(StratRole.EntropyRole(Red), strategygames.Pos.Entropy(Pos.D4))
      dropped.toOption must beSome[(StratGame, strategygames.Drop)]

      val afterDrop = dropped.toOption.get._1
      afterDrop.player === Player.P2 // Order's turn
      afterDrop.situation.board.pieces.size === 1

      afterDrop.pass().toOption must beSome[(StratGame, strategygames.Pass)]
    }

    "offer no draw agreement, since entropy is decided on score" in {
      stratVariant.canOfferDraw must beFalse
    }
  }

  "the draw-counter uci factories" should {

    "build a drawn counter from its colour, for entropy only" in {
      StratUci.DrawCounter.fromStrings(lib, "r").map(_.uci) === Some("draw-r")
      StratUci.DrawCounter.fromStrings(lib, "?") === None
      StratUci.DrawCounter.fromStrings(GameLogic.Backgammon(), "r") === None
    }

    "build a draw request, for entropy only" in {
      StratUci.DoDrawCounter.apply(lib).map(_.uci) === Some("draw")
      StratUci.DoDrawCounter.apply(GameLogic.Backgammon()) === None
    }

    "resolve a draw request into whatever the bag gives up" in {
      val start = StratGame.apply(lib, stratVariant)
      val uci   = StratUci.DoDrawCounter.apply(lib).get

      val drawn = start.applyUci(uci, strategygames.MoveMetrics())
      drawn.toOption must beSome[(StratGame, StratAction)]

      // the request carries no colour; the game that comes back knows which one it drew
      drawn.toOption.get._2.toUci.uci must startWith("draw-")
    }

    "wrap a played draw back into a uci" in {
      val start = StratGame.apply(lib, stratVariant)
      val dc    = start.drawCounter(StratRole.EntropyRole(Red)).toOption.get._2
      StratUci.apply(lib, dc).uci === "draw-r"
    }
  }

  "the pgn dumper" should {

    "dump every entropy action type, including the draw" in {
      val start        = StratGame.apply(lib, stratVariant)
      val (g1, drawn)  = start.drawCounter(StratRole.EntropyRole(Red)).toOption.get
      val (g2, drop)   = g1.drop(StratRole.EntropyRole(Red), strategygames.Pos.Entropy(Pos.D4)).toOption.get
      val (_, passed)  = g2.pass().toOption.get

      Dumper(lib, drawn) === "draw-r"
      Dumper(lib, drop) === "r@d4"
      Dumper(lib, passed) === "pass"

      // and through the Action overload, which used to drop the draw on the floor
      Dumper(lib, (drawn: StratAction)) === "draw-r"
    }
  }

  "binary encoding" should {
    "round-trip every entropy action type" in {
      val actions = Vector(Vector("draw-r", "r@d4"), Vector("d4e4"), Vector("draw-k", "k@a1"), Vector("pass"))

      val bytes = Binary.writeActionStrs(gf, actions)
      bytes.isSuccess must beTrue

      val back = Binary.readActionStrs(lib, bytes.get.toList)
      back.isSuccess must beTrue
      back.get === actions
    }
  }
}
