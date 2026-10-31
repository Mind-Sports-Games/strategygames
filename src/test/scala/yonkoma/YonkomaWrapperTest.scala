package strategygames.yonkoma

import org.specs2.mutable.Specification

import strategygames.{
  Action => StratAction,
  Game => StratGame,
  GameFamily,
  GameLogic,
  Player,
  Role => StratRole,
  Situation => StratSituation
}
import strategygames.format.{ FEN => StratFEN, Forsyth => StratForsyth, Uci => StratUci }
import strategygames.format.pgn.{ Binary, Dumper, Reader => StratReader, Tag, Tags }
import strategygames.variant.{ Variant => StratVariant }

// Yonkoma reached through the wrapper API rather than its own package. The wrapper
// dispatches on GameLogic, and a missing case there is a MatchError at runtime that no
// amount of testing inside the yonkoma package would catch.
class YonkomaWrapperTest extends Specification {

  val lib                        = GameLogic.Yonkoma()
  val gf                         = GameFamily.Yonkoma()
  val stratVariant: StratVariant = StratVariant.Yonkoma(strategygames.yonkoma.variant.Yonkoma10x10)
  val strat12: StratVariant      = StratVariant.Yonkoma(strategygames.yonkoma.variant.Yonkoma12x12)

  "the game collection" should {

    "know the yonkoma logic, family and group by id" in {
      GameLogic(10) === lib
      GameFamily(15) === gf
      strategygames.GameGroup(14).variants.map(_.key) === List("yonkoma10x10", "yonkoma12x12")
      gf.defaultVariant === stratVariant
      gf.variants.size === 2
      gf.hasAnalysisBoard must beFalse
      gf.hasFishnet must beFalse
    }

    "list both variants with distinct ids, keys and perf ids" in {
      val vs = StratVariant.all(lib)
      vs.map(_.id) === List(1, 2)
      vs.map(_.key) === List("yonkoma10x10", "yonkoma12x12")
      vs.map(_.perfId) === List(1000, 1001)
      StratVariant.all.filterNot(_.gameLogic == lib).map(_.perfId).intersect(List(1000, 1001)) must beEmpty
      StratVariant.all.filterNot(_.gameLogic == lib).map(_.key).intersect(vs.map(_.key)) must beEmpty
      StratVariant.byKey(lib).get("yonkoma12x12") === Some(strat12)
      StratVariant.default(lib) === stratVariant
      StratVariant.libStandard(lib) === stratVariant
    }

    "describe the variants for the lobby" in {
      stratVariant.baseVariant must beTrue
      strat12.baseVariant must beFalse
      stratVariant.dropsVariant must beFalse
      stratVariant.canOfferDraw must beFalse
      stratVariant.hasAnalysisBoard must beFalse
      stratVariant.gameFamily === gf
      stratVariant.playerNames === Map(Player.P1 -> "Red", Player.P2 -> "Blue")
      stratVariant.startPlayer === Player.P1
      stratVariant.pieces.size === 4
    }
  }

  "the wrapper" should {

    "build a game and report the yonkoma game family" in {
      val g = StratGame.apply(lib, stratVariant)
      g.situation.board.variant.gameFamily === gf
      g.player === Player.P1
      g.situation.moves.size === 2
      g.situation.destinations.values.map(_.size).sum === 16
    }

    "list the one role, and find it by forsyth character" in {
      StratRole.all(lib).size === 1
      StratRole.allByForsyth(lib).get('c').map(_.name) === Some("Checker")
      StratRole.storable(lib) must beEmpty
    }

    "make a piece from a forsyth character" in {
      strategygames.Piece.fromChar(lib, gf, 'C').map(p => (p.player, p.role.name)) === Some(
        (Player.P1, "Checker")
      )
      strategygames.Piece.fromChar(lib, gf, 'c').map(_.player) === Some(Player.P2)
    }

    "hash a situation without falling over" in {
      val g = StratGame.apply(lib, stratVariant)
      strategygames.Hash(lib, g.situation).length === 3
    }

    "give the initial fen and read it back" in {
      StratForsyth.initial(lib).value === "c8C/10/10/10/10/10/10/10/10/C8c w 1"
      StratFEN.apply(lib, "c8C/10/10/10/10/10/10/10/10/C8c w 1").player === Some(Player.P1)
      StratFEN.apply(lib, "c8C/10/10/10/10/10/10/10/10/C8c w 1").initial must beTrue
      strat12.initialFen.initial must beTrue
      StratForsyth.<<@(lib, strat12, strat12.initialFen).map(_.board.pieces.size) === Some(4)
    }

    "build a move uci from two wrapped positions" in {
      StratUci.Move
        .apply(lib, strategygames.Pos.Yonkoma(Pos.A1), strategygames.Pos.Yonkoma(Pos.E5), None)
        .uci === "a1e5"
    }

    "parse each action string into a wrapped Uci" in {
      StratUci.apply(lib, gf, "a1e5") must beSome[StratUci]
      StratUci.apply(lib, gf, "j10a1") must beSome[StratUci]
      StratUci.apply(lib, gf, "pass") must beSome[StratUci]
      StratUci.Pass.apply(lib, gf).map(_.uci) === Some("pass")
      StratUci.Drop.fromStrings(lib, gf, "c", "a1") === None
    }

    "find positions by key and piotr" in {
      strategygames.Pos.fromKey(lib, "l12").map(_.key) === Some("l12")
      strategygames.Pos.fromKey(lib, "m1") === None
      strategygames.Pos.Yonkoma(Pos.A1).all.size === 144
    }
  }

  "a game played through the wrapper" should {

    "move, skip a stuck player and end on the crosscut" in {
      val start = StratGame.apply(lib, stratVariant)

      val moved = start.apply(strategygames.Pos.Yonkoma(Pos.J10), strategygames.Pos.Yonkoma(Pos.F6))
      moved.toOption must beSome[(StratGame, strategygames.Move)]
      val g1    = moved.toOption.get._1
      g1.player === Player.P2
      g1.plies === 1
      g1.turnCount === 1

      val stuck = List("j1e6", "a1e5", "a10d7", "e5d6").foldLeft(g1) { (g, str) =>
        g.applyUci(StratUci.apply(lib, gf, str).get, strategygames.MoveMetrics()).toOption.get._1
      }
      stuck.player === Player.P2
      stuck.situation.moves must beEmpty
      stuck.situation.passes.size === 1
      stuck.situation.forcedAction.map(_.toUci.uci) === Some("pass")
      stuck.situation.actions.size === 1

      val passed = stuck.pass().toOption.get._1
      passed.player === Player.P1
      passed.situation.forcedAction === None

      val over =
        passed.apply(strategygames.Pos.Yonkoma(Pos.F6), strategygames.Pos.Yonkoma(Pos.E7)).toOption.get._1
      over.situation.end must beTrue
      over.situation.winner === Some(Player.P2)
      over.situation.status === Some(strategygames.Status.VariantEnd)
      over.situation.playable(true) must beFalse
      over.actionStrs === Vector(
        Vector("j10f6"),
        Vector("j1e6"),
        Vector("a1e5"),
        Vector("a10d7"),
        Vector("e5d6"),
        Vector("pass"),
        Vector("f6e7")
      )
    }

    "refuse a pass while a move is available" in {
      StratGame.apply(lib, stratVariant).pass().toOption must beNone
    }

    "play on the 12x12 board as well" in {
      val g = List("l12g7", "l1g6", "a1f6", "a12f7").foldLeft(StratGame.apply(lib, strat12)) { (g, str) =>
        g.applyUci(StratUci.apply(lib, gf, str).get, strategygames.MoveMetrics()).toOption.get._1
      }
      g.situation.end must beTrue
      g.situation.winner === Some(Player.P1)
    }
  }

  "the replay" should {

    "rebuild a game from its action strings" in {
      val actionStrs = Vector(
        Vector("j10f6"),
        Vector("j1e6"),
        Vector("a1e5"),
        Vector("a10d7"),
        Vector("e5d6"),
        Vector("pass"),
        Vector("f6e7")
      )
      val ucis       = actionStrs.toList.flatMap(v => v).flatMap(a => StratUci.apply(lib, gf, a))
      ucis.size === 7
      val replay     = strategygames.Replay.apply(lib, ucis, None, stratVariant)
      replay.toOption must beSome[strategygames.Replay].like { case r =>
        r.state.situation.end must beTrue
        r.state.plies === 7
        r.chronoActions.size === 7
      }
      val situations = strategygames.Replay.situations(lib, actionStrs, None, stratVariant)
      situations.toOption.map(_.size) === Some(8)
    }

    "take the board size from the Variant tag" in {
      val actionStrs = Vector(Vector("l12g7"), Vector("l1g6"), Vector("a1f6"), Vector("a12f7"))
      val tags       = Tags(List(Tag(_.Variant, "Yonkoma 12x12")))
      val result     = StratReader.replayResultFromActionStrs(lib, actionStrs, identity, tags)
      result.toOption.flatMap(_.valid.toOption) must beSome[strategygames.Replay].like { case r =>
        r.state.situation.board.variant === strat12
        r.state.situation.end must beTrue
      }
    }

    "dump the ucis of a game" in {
      val actionStrs = Vector(Vector("a1e5"), Vector("a10e6"), Vector("j10f6"), Vector("j1f5"))
      strategygames.format.UciDump.apply(lib, actionStrs, None, stratVariant).toOption === Some(actionStrs)
    }

    "give the fens of every position in a game" in {
      val actionStrs = Vector(Vector("a1e5"), Vector("a10e6"))
      val boards     = strategygames.Replay.boards(lib, actionStrs, None, stratVariant)
      boards.toOption.map(_.size) === Some(3)
    }
  }

  "the pgn dumper" should {
    "dump a move and a pass" in {
      val start       = StratGame.apply(lib, stratVariant)
      val (g1, move)  =
        start.apply(strategygames.Pos.Yonkoma(Pos.J10), strategygames.Pos.Yonkoma(Pos.F6)).toOption.get
      Dumper(lib, move) === "j10f6"
      Dumper(lib, (move: StratAction)) === "j10f6"
      val stuck       = List("j1e6", "a1e5", "a10d7", "e5d6").foldLeft(g1) { (g, str) =>
        g.applyUci(StratUci.apply(lib, gf, str).get, strategygames.MoveMetrics()).toOption.get._1
      }
      val (_, passed) = stuck.pass().toOption.get
      Dumper(lib, passed) === "pass"
      Dumper(lib, (passed: StratAction)) === "pass"
    }
  }

  "binary encoding" should {
    "round-trip moves and passes through the wrapper" in {
      val actions = Vector(Vector("a1e5"), Vector("pass"), Vector("l12a11"))

      val bytes = Binary.writeActionStrs(gf, actions)
      bytes.isSuccess must beTrue

      val back = Binary.readActionStrs(lib, bytes.get.toList)
      back.isSuccess must beTrue
      back.get === actions
    }
  }

  "a situation" should {
    "wrap and unwrap" in {
      val s = StratSituation.apply(lib, stratVariant)
      s.toYonkoma.board.variant === strategygames.yonkoma.variant.Yonkoma10x10
      (StratSituation.wrap(s.toYonkoma): StratSituation) === s
      s.gameLogic === lib
      s.canDrop must beFalse
      s.takebackable must beTrue
    }
  }
}
