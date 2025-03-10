package format.sgf

import strategygames._
import strategygames.variant.Variant

import strategygames.format.sgf.Dumper
import strategygames.format.FEN

import org.specs2.matcher.ValidatedMatchers
import org.specs2.mutable.Specification

class DumperTest extends Specification with ValidatedMatchers {

  val Go19x19     = Variant.Go(strategygames.go.variant.Go19x19)
  val Go13x13     = Variant.Go(strategygames.go.variant.Go13x13)
  val Go9x9       = Variant.Go(strategygames.go.variant.Go9x9)
  val Backgammon  = Variant.Backgammon(strategygames.backgammon.variant.Backgammon)
  val Othello     = Variant.FairySF(strategygames.fairysf.variant.Flipello)
  val Amazons     = Variant.FairySF(strategygames.fairysf.variant.Amazons)
  val Shogi       = Variant.FairySF(strategygames.fairysf.variant.Shogi)
  val MiniShogi   = Variant.FairySF(strategygames.fairysf.variant.MiniShogi)
  val Xiangqi     = Variant.FairySF(strategygames.fairysf.variant.Xiangqi)
  val MiniXiangqi = Variant.FairySF(strategygames.fairysf.variant.MiniXiangqi)

  // not supporting loa yet as in chess game logic
  // val LOA           = Variant.Chess(strategygames.chess.variant.LinesOfAction)
  // val ScrambledEggs = Variant.Chess(strategygames.chess.variant.ScrambledEggs)

  "Go19x19 => s@b1 in actionStrsToOutput" should {
    "have an sgf output" in {
      val output                 = ";B[bs]"
      val actionStrs: ActionStrs = Vector(Vector("s@b1"))
      Dumper.apply(Go19x19, actionStrs) must_== output
    }
  }

  "Go19x19 => longer game has actionStrsToOutput" should {
    "have an sgf output" in {
      val output                 = ";B[bs]\n;W[cr]\n;B[dq]\n;W[ep]"
      val actionStrs: ActionStrs = Vector(Vector("s@b1"), Vector("s@c2"), Vector("s@d3"), Vector("s@e4"))
      Dumper.apply(Go19x19, actionStrs) must_== output
    }
  }

  "Go19x19 => longer game with handicap has actionStrsToOutput" should {
    "have an sgf output" in {
      val initialFen             = Some(
        FEN(
          Go19x19,
          "19/19/19/15S3/19/19/19/19/19/19/19/19/19/19/19/3S11S3/19/19/19[SSSSSSSSSSssssssssss] w - 0 75 0 0 75 0 1"
        )
      )
      val output                 = ";B[dp]\n;B[pd]\n;B[pp]\n;W[bs]\n;B[cr]\n;W[dq]\n;B[ep]"
      val actionStrs: ActionStrs = Vector(Vector("s@b1"), Vector("s@c2"), Vector("s@d3"), Vector("s@e4"))
      Dumper.apply(Go19x19, actionStrs, initialFen) must_== output
    }
  }

  "Go13x13 => game with handicap has actionStrsToOutput" should {
    "have an sgf output" in {
      val initialFen             = Some(
        FEN(
          Go13x13,
          "13/13/13/3S5S3/13/13/3S2S2S3/13/13/3S5S3/13/13/13[SSSSSSSSSSssssssssss] w - 0 75 0 0 75 0 1"
        )
      )
      val output                 = ";B[dd]\n;B[dg]\n;B[dj]\n;B[gg]\n;B[jd]\n;B[jg]\n;B[jj]\n;W[bm]\n;B[cl]\n;W[dk]\n;B[ej]"
      val actionStrs: ActionStrs = Vector(Vector("s@b1"), Vector("s@c2"), Vector("s@d3"), Vector("s@e4"))
      Dumper.apply(Go13x13, actionStrs, initialFen) must_== output
    }
  }

  "Go13x13 => game with handicap 0 but komi has actionStrsToOutput" should {
    "have an sgf output" in {
      val initialFen             = Some(
        FEN(
          Go13x13,
          "13/13/13/13/13/13/13/13/13/13/13/13/13[SSSSSSSSSSssssssssss] b - 0 40 0 0 40 0 1"
        )
      )
      val output                 = ";B[bm]\n;W[cl]\n;B[dk]\n;W[ej]"
      val actionStrs: ActionStrs = Vector(Vector("s@b1"), Vector("s@c2"), Vector("s@d3"), Vector("s@e4"))
      Dumper.apply(Go13x13, actionStrs, initialFen) must_== output
    }
  }

  "Go9x9 => longer game has actionStrsToOutput" should {
    "have an sgf output" in {
      val output                 = ";B[bi]\n;W[ch]\n;B[dg]\n;W[ef]"
      val actionStrs: ActionStrs = Vector(Vector("s@b1"), Vector("s@c2"), Vector("s@d3"), Vector("s@e4"))
      Dumper.apply(Go9x9, actionStrs) must_== output
    }
  }

  "Go9x9 => with passes in game has actionStrsToOutput" should {
    "have an sgf output" in {
      val output                 = ";B[bi]\n;W[ch]\n;B[]\n;W[dc]"
      val actionStrs: ActionStrs = Vector(Vector("s@b1"), Vector("s@c2"), Vector("pass"), Vector("s@d7"))
      Dumper.apply(Go9x9, actionStrs) must_== output
    }
  }

  "Go9x9 with initial fen handicap" should {
    "have an sgf output with just additioanl moves" in {
      val initialFen             =
        Some(FEN(Go9x9, "9/9/2S3S2/9/2S3S2/9/2S3S2/9/9[SSSSSSSSSSssssssssss] w - 0 865 0 0 55 0 1"))
      val output                 = ";B[cc]\n;B[ce]\n;B[cg]\n;B[gc]\n;B[ge]\n;B[gg]"
      val actionStrs: ActionStrs = Vector()

      Dumper.apply(Go9x9, actionStrs, initialFen) must_== output
    }
  }

  "Go9x9 with initial fen handicap" should {
    "have an sgf output with additionl moves" in {
      val initialFen             =
        Some(FEN(Go9x9, "9/9/2S3S2/9/2S3S2/9/2S3S2/9/9[SSSSSSSSSSssssssssss] w - 0 865 0 0 55 0 1"))
      val output                 = ";B[cc]\n;B[ce]\n;B[cg]\n;B[gc]\n;B[ge]\n;B[gg]\n;W[bi]\n;B[ch]\n;W[]\n;B[dc]"
      val actionStrs: ActionStrs = Vector(Vector("s@b1"), Vector("s@c2"), Vector("pass"), Vector("s@d7"))
      Dumper.apply(Go9x9, actionStrs, initialFen) must_== output
    }
  }

  // https://www.red-bean.com/sgf/backgammon.html
  "Backgammon drop=> s@l1 actionStrsToOutput" should {
    "have an sgf output" in {
      val output                 = ";W[yx]"
      val actionStrs: ActionStrs = Vector(Vector("s@l2"))
      Dumper.apply(Backgammon, actionStrs) must_== output
    }
  }

  "Backgammon lift=> ^h1 actionStrsToOutput" should {
    "have an sgf output" in {
      val output                 = ";W[ez]"
      val actionStrs: ActionStrs = Vector(Vector("^h1"))
      Dumper.apply(Backgammon, actionStrs) must_== output
    }
  }

  "Backgammon move=> ^h1 actionStrsToOutput" should {
    "have an sgf output" in {
      val output                 = ";W[mg]"
      val actionStrs: ActionStrs = Vector(Vector("a2f1"))
      Dumper.apply(Backgammon, actionStrs) must_== output
    }
  }

  "Backgammon diceroll=> 31 actionStrsToOutput" should {
    "have an sgf output" in {
      val output                 = ";W[31]"
      val actionStrs: ActionStrs = Vector(Vector("1/3"))
      Dumper.apply(Backgammon, actionStrs) must_== output
    }
  }

  "Backgammon part of game actionStrsToOutput" should {
    "have an sgf output" in {
      val output                 = ";W[31hefe]\n;B[66]"
      val actionStrs: ActionStrs =
        Vector(Vector("1/3", "e1h1", "g1h1", "endTurn"), Vector("6/6"))
      Dumper.apply(Backgammon, actionStrs) must_== output
    }
  }

  "Backgammon p2 starts actionStrsToOutput" should {
    "have an sgf output" in {
      val output                 = ";W[]\n;B[44aeaelplp]\n;W[43]"
      val actionStrs: ActionStrs =
        Vector(Vector("endTurn"), Vector("4/4", "l1h1", "l1h1", "a1d2", "a1d2", "endTurn"), Vector("3/4"))
      Dumper.apply(Backgammon, actionStrs) must_== output
    }
  }

  "Othello drop=> s@l1 actionStrsToOutput" should {
    "have an sgf output" in {
      val output                 = ";B[dc]"
      val actionStrs: ActionStrs = Vector(Vector("P@d6"))
      Dumper.apply(Othello, actionStrs) must_== output
    }
  }

  // https://playstrategy.dev/U8hL4fbB/
  "Othello longer game with passes actionStrsToOutput" should {
    "have an sgf output" in {
      val output                 =
        ";B[fe]\n;W[df]\n;B[cc]\n;W[dc]\n;B[ec]\n;W[bb]\n;B[cd]\n;W[ce]\n;B[aa]\n;W[cb]\n;B[ba]\n;W[db]\n;B[ca]\n;W[da]\n;B[ea]\n;W[ab]\n;B[ac]\n;W[ff]\n;B[bc]\n;W[fd]\n;B[eb]\n;W[fa]\n;B[ga]\n;W[]\n;B[fb]\n;W[]\n;B[fc]\n;W[gb]\n;B[gd]\n;W[hc]\n;B[ha]\n;W[]\n;B[ge]\n;W[gc]\n;B[ef]\n;W[dg]\n;B[hb]\n;W[hd]\n;B[he]\n;W[bd]\n;B[ad]\n;W[]\n;B[be]\n;W[]\n;B[fg]\n;W[gf]\n;B[eg]\n;W[gg]\n;B[hf]\n;W[]\n;B[fh]\n;W[hg]\n;B[hh]\n;W[]\n;B[gh]\n;W[]\n;B[eh]\n;W[]\n;B[dh]"
      val actionStrs: ActionStrs = Vector(
        Vector("P@f4"),
        Vector("P@d3"),
        Vector("P@c6"),
        Vector("P@d6"),
        Vector("P@e6"),
        Vector("P@b7"),
        Vector("P@c5"),
        Vector("P@c4"),
        Vector("P@a8"),
        Vector("P@c7"),
        Vector("P@b8"),
        Vector("P@d7"),
        Vector("P@c8"),
        Vector("P@d8"),
        Vector("P@e8"),
        Vector("P@a7"),
        Vector("P@a6"),
        Vector("P@f3"),
        Vector("P@b6"),
        Vector("P@f5"),
        Vector("P@e7"),
        Vector("P@f8"),
        Vector("P@g8"),
        Vector("d3d3"),
        Vector("P@f7"),
        Vector("d3d3"),
        Vector("P@f6"),
        Vector("P@g7"),
        Vector("P@g5"),
        Vector("P@h6"),
        Vector("P@h8"),
        Vector("d3d3"),
        Vector("P@g4"),
        Vector("P@g6"),
        Vector("P@e3"),
        Vector("P@d2"),
        Vector("P@h7"),
        Vector("P@h5"),
        Vector("P@h4"),
        Vector("P@b5"),
        Vector("P@a5"),
        Vector("d2d2"),
        Vector("P@b4"),
        Vector("d2d2"),
        Vector("P@f2"),
        Vector("P@g3"),
        Vector("P@e2"),
        Vector("P@g2"),
        Vector("P@h3"),
        Vector("d2d2"),
        Vector("P@f1"),
        Vector("P@h2"),
        Vector("P@h1"),
        Vector("d2d2"),
        Vector("P@g1"),
        Vector("d2d2"),
        Vector("P@e1"),
        Vector("d2d2"),
        Vector("P@d1")
      )
      Dumper.apply(Othello, actionStrs) must_== output
    }
  }

  // https://www.red-bean.com/sgf/amazons.html#types
  "Amazons full move => d1d6,P@g9 actionStrsToOutput" should {
    "have an sgf output" in {
      val output                 = ";W[djdegb]"
      val actionStrs: ActionStrs = Vector(Vector("d1d6", "P@g9"))
      Dumper.apply(Amazons, actionStrs) must_== output
    }
  }

  "Amazons moves => actionStrsToOutput" should {
    "have an sgf output" in {
      val output                 = ";W[djdegb]\n;B[gabfjf]\n;W[jgjj]"
      val actionStrs: ActionStrs = Vector(Vector("d1d6", "P@g9"), Vector("g10b5", "P@j5"), Vector("j4j1"))
      Dumper.apply(Amazons, actionStrs) must_== output
    }
  }

  "Xiangqi moves => actionStrsToOutput" should {
    "have an sgf output" in {
      val output                 = ";W[hheh]\n;B[bacc]\n;W[hjgh]"
      val actionStrs: ActionStrs = Vector(Vector("h3e3"), Vector("b10c8"), Vector("h1g3"))
      Dumper.apply(Xiangqi, actionStrs) must_== output
    }
  }

  "MiniXiangqi moves => actionStrsToOutput" should {
    "have an sgf output" in {
      val output                 = ";W[efee]\n;B[fafc]\n;W[eeed]\n;B[fcdc]\n;W[eddd]"
      val actionStrs: ActionStrs =
        Vector(Vector("e2e3"), Vector("f7f5"), Vector("e3e4"), Vector("f5d5"), Vector("e4d4"))
      Dumper.apply(MiniXiangqi, actionStrs) must_== output
    }
  }

  "Shogi moves => actionStrsToOutput" should {
    "have an sgf output" in {
      val output                 = ";B[fifh]\n;W[gcgd]\n;B[cgcf]\n;W[hbbh+]\n;B[cibh]\n;W[*Bee]\n;B[*Bhf]\n;W[eebh]"
      val actionStrs: ActionStrs =
        Vector(
          Vector("f1f2"),
          Vector("g7g6"),
          Vector("c3c4"),
          Vector("h8b2B"),
          Vector("c1b2"),
          Vector("B@e5"),
          Vector("B@h4"),
          Vector("e5b2")
        )
      Dumper.apply(Shogi, actionStrs) must_== output
    }
  }

  "Shogi moves fairy style promotion (+) => actionStrsToOutput" should {
    "have an sgf output" in {
      val output                 =
        ";B[hghf]\n;W[eafb]\n;B[hfhe]\n;W[fbgb]\n;B[hehd]\n;W[fafb]\n;B[gggf]\n;W[fbfa]\n;B[gfge]\n;W[bbfb]\n;B[hdhc+]"
      val actionStrs: ActionStrs =
        Vector(
          Vector("h3h4"),
          Vector("e9f8"),
          Vector("h4h5"),
          Vector("f8g8"),
          Vector("h5h6"),
          Vector("f9f8"),
          Vector("g3g4"),
          Vector("f8f9"),
          Vector("g4g5"),
          Vector("b8f8"),
          Vector("h6h7+")
        )
      Dumper.apply(Shogi, actionStrs) must_== output
    }
  }

  "MiniShogi moves => actionStrsToOutput" should {
    "have an sgf output" in {
      val output                 =
        ";B[debc]\n;W[aaad]\n;B[aead]\n;W[*Pdd]\n;B[bcda+]\n;W[eada]\n;B[*Raa]\n;W[ddde+]\n;B[eede]"
      val actionStrs: ActionStrs =
        Vector(
          Vector("d1b3"),
          Vector("a5a2"),
          Vector("a1a2"),
          Vector("P@d2"),
          Vector("b3d5B"),
          Vector("e5d5"),
          Vector("R@a5"),
          Vector("d2d1P"),
          Vector("e1d1")
        )
      Dumper.apply(MiniShogi, actionStrs) must_== output
    }
  }

  // "Lines of Action moves => actionStrsToOutput" should {
  //   "have an sgf output" in {
  //     val output                 = ";B[dhbf]\n;W[afcd]\n;B[cacd]\n;W[hdfb]\n;B[gagc]"
  //     val actionStrs: ActionStrs =
  //       Vector(Vector("d1b3"), Vector("a3c5"), Vector("c8c5"), Vector("h5f7"), Vector("g8g6"))
  //     Dumper.apply(LOA, actionStrs) must_== output
  //   }
  // }

  // "Scrambled Eggs moves => actionStrsToOutput" should {
  //   "have an sgf output" in {
  //     val output                 = ";B[bhbf]\n;W[hdfd]\n;B[gagc]\n;W[hfec]\n;B[abcd]\n;W[badc]\n;B[addd]\n;W[acfc]\n;B[fhfd]"
  //     val actionStrs: ActionStrs =
  //       Vector(
  //         Vector("b1b3"),
  //         Vector("h5f5"),
  //         Vector("g8g6"),
  //         Vector("h3e6"),
  //         Vector("a7c5"),
  //         Vector("b8d6"),
  //         Vector("a5d5"),
  //         Vector("a6f6"),
  //         Vector("f1f5")
  //       )
  //     Dumper.apply(ScrambledEggs, actionStrs) must_== output
  //   }
  // }

}
