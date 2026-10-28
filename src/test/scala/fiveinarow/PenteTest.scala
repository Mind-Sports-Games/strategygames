package strategygames.fiveinarow

import scala.util.Random

import org.specs2.mutable.Specification

import strategygames.{
  GameFamily,
  GameLogic,
  History => StratHistory,
  Player,
  Score,
  Situation => StratSituation,
  Status
}
import strategygames.fiveinarow.format.{ FEN, Forsyth, Uci }
import strategygames.fiveinarow.format.pgn.Binary
import strategygames.fiveinarow.variant.{ Gomoku, Pente }
import strategygames.variant.{ Variant => StratVariant }

class PenteTest extends Specification with FiveInARowTestSupport {

  private def pente(stones: (String, Role)*): Board = boardIn(Pente)(stones*)

  private def stones(role: Role)(keys: String*): Seq[(String, Role)] = keys.map(_ -> role)

  private def dropOn(board: Board, key: String): Situation = {
    val s = situationOf(board)
    s.drop(s.nextColour, Pos.fromKey(key).get).fold(err => sys.error(err), _.situationAfter)
  }

  private def keys(board: Board): Set[String] = board.pieces.keySet.map(_.key)

  private def penteStart: Game = Game(Pente)

  // the spare black stone on s19 makes it black's turn to close the sandwich on d1
  private lazy val rowCaptured: Situation =
    dropOn(pente(("a1", BlackStone), ("s19", BlackStone), ("b1", WhiteStone), ("c1", WhiteStone)), "d1")

  "the pente variant" should {

    "be found by key, on a 19x19 board, scoring captures" in {
      variant.Variant("pente") === Some(Pente)
      Pente.boardSize === Board.Dim19x19
      Pente.perfIcon === ''
      Pente.hasGameScore must beTrue
      Gomoku.hasGameScore must beFalse
      StratVariant.FiveInARow(Pente).hasGameScore must beTrue
      StratVariant.FiveInARow(Pente).gameFamily === GameFamily.FiveInARow()
    }

    "offer every point of the 19x19 board" in {
      Situation(Pente).drops.map(_.size) === Some(361)
      Situation(Gomoku).drops.map(_.size) === Some(225)
    }

    "accept a drop on s19, which gomoku refuses" in {
      play(penteStart, "B@s19").situation.board.pieces.keySet.map(_.key) === Set("s19")
      refused(start, "B@s19") must beTrue
      refused(start, "B@p1") must beTrue
    }

    "open with the same swap2 rules as gomoku" in {
      val g = play(penteStart, "B@j10", "W@j11", "B@k10", "swap2", "W@l10", "B@l11")
      g.situation.openingStep === OpeningStep.FinalChoice
      g.situation.canSwap must beTrue
      g.player === Player.P1
    }
  }

  "a capture" should {

    "remove exactly two stones sandwiched along a row" in {
      val after = rowCaptured
      keys(after.board) === Set("a1", "s19", "d1")
      after.board.score === Score(2, 0)
    }

    "work along columns and both diagonals, all at once" in {
      val board = pente(
        stones(BlackStone)("m10", "j13", "m13", "a1", "c1", "e1") ++
          stones(WhiteStone)("k10", "l10", "j11", "j12", "k11", "l12")*
      )
      val after = dropOn(board, "j10")
      keys(after.board) === Set("m10", "j13", "m13", "a1", "c1", "e1", "j10")
      after.board.score === Score(6, 0)
    }

    "take a pair on a falling diagonal, looking back up it from the drop" in {
      val board = pente(stones(BlackStone)("a19", "s1") ++ stones(WhiteStone)("b18", "c17")*)
      val after = dropOn(board, "d16")
      keys(after.board) === Set("a19", "s1", "d16")
      after.board.score === Score(2, 0)
    }

    "not take a pair of the capturing colour's own stones" in {
      val board = pente(stones(BlackStone)("a1", "b1", "c1") ++ stones(WhiteStone)("a5", "c5")*)
      val after = dropOn(board, "e5")
      after.board.pieces.size === 6
      after.board.score === Score(0, 0)
    }

    "not take three in a row" in {
      val board = pente(stones(BlackStone)("a1", "a5", "c5") ++ stones(WhiteStone)("b1", "c1", "d1")*)
      val after = dropOn(board, "e1")
      after.board.pieces.size === 7
      after.board.score === Score(0, 0)
    }

    "not take a single stone" in {
      val after = dropOn(pente(("a1", BlackStone), ("b1", WhiteStone)), "c1")
      after.board.pieces.size === 3
      after.board.score === Score(0, 0)
    }

    "not happen to a pair that drops into the sandwich itself" in {
      val after = dropOn(pente(("a1", BlackStone), ("d1", BlackStone), ("b1", WhiteStone)), "c1")
      keys(after.board) === Set("a1", "b1", "c1", "d1")
      after.board.score === Score(0, 0)
    }

    "not happen in gomoku" in {
      val after = dropOn(boardOf(("a1", BlackStone), ("b1", WhiteStone), ("c1", WhiteStone)), "d1")
      after.board.pieces.size === 4
      after.board.score === Score(0, 0)
    }

    "leave the turn order intact, counting captured stones as dropped" in {
      val after = rowCaptured
      after.nextColour === WhiteStone
      after.player === Player.P2
    }

    "let a point emptied by a capture be played again" in {
      val after = rowCaptured
      after.drops.exists(_.exists(_.key == "b1")) must beTrue
    }
  }

  "captures in the opening" should {

    // P1 opens h8 black, h9 white, h7 black; P2's swap2 white on h6 then sandwiches h8 and h7
    val captured = play(penteStart, "B@h8", "W@h9", "B@h7", "swap2", "W@h6")

    "be taken during the swap2 drops, and credited to the colour that made them" in {
      keys(captured.situation.board) === Set("h9", "h6")
      captured.situation.board.score === Score(0, 2)
      captured.situation.openingStep === OpeningStep.Swap2Drops
      captured.situation.nextColour === BlackStone
      captured.player === Player.P2
    }

    "still reach the final choice after the fifth stone dropped" in {
      val g = play(captured, "B@a1")
      g.situation.openingStep === OpeningStep.FinalChoice
      g.situation.blackSeat === Player.P2
      g.situation.board.score === Score(2, 0)
      g.player === Player.P1
    }
  }

  "winning" should {

    // nine spare black stones balance the eight white stones black has already captured
    val spares = stones(BlackStone)("s1", "s3", "s5", "s7", "s9", "s11", "s13", "s15", "s17")

    def withScore(board: Board, score: Score): Board = board.updateHistory(_.copy(score = score))

    val eightTaken = withScore(
      pente((spares ++ Seq(("a1", BlackStone), ("b1", WhiteStone), ("c1", WhiteStone)))*),
      Score(8, 0)
    )

    "go to the seat that captures its tenth stone" in {
      situationOf(eightTaken).end must beFalse
      val after = dropOn(eightTaken, "d1")
      after.board.score === Score(10, 0)
      after.end must beTrue
      after.winner === Some(Player.P1)
      after.status === Some(Status.TenCaptures)
      after.drops must beNone
    }

    "follow the colour that captured after a swap" in {
      situationOf(dropOn(eightTaken, "d1").board.swapColours).winner === Some(Player.P2)
    }

    "still go to five or more in a row" in {
      val s = situationOf(pente(stones(WhiteStone)("o15", "p15", "q15", "r15", "s15", "n15")*))
      s.winner === Some(Player.P2)
      s.end must beTrue
    }

    "not come from fewer than ten captured" in {
      situationOf(withScore(pente(), Score(8, 8))).end must beFalse
    }
  }

  "a pente fen" should {

    "start from an empty 19x19 board with both scores at zero" in {
      Pente.initialFen.value === List.fill(19)("19").mkString("/") + " b 1 o 0 0 1"
      Forsyth.>>(penteStart).value === Pente.initialFen.value
      Gomoku.initialFen === Forsyth.initial
    }

    "record each seat's captures" in {
      val after = rowCaptured
      Forsyth.>>(after).value.split(' ').drop(1).mkString(" ") === "w 1 - 2 0 1"
    }

    "round trip with the score, the seat and the turn" in {
      val g    = play(penteStart, "B@h8", "W@h9", "B@h7", "swap2", "W@h6", "B@a1", "W@s19")
      val read = Forsyth.<<@(Pente, Forsyth.>>(g)).get
      read.board.pieces === g.situation.board.pieces
      read.board.score === g.situation.board.score
      read.blackSeat === g.situation.blackSeat
      read.player === g.player
      Forsyth.<<(Forsyth.>>(g)).map(_.board.variant) === Some(Pente)
    }

    "not be read as gomoku, nor a gomoku fen as pente" in {
      Forsyth.<<@(Gomoku, Pente.initialFen) must beNone
      Forsyth.<<@(Pente, Gomoku.initialFen) must beNone
      Forsyth.<<(Gomoku.initialFen).map(_.board.variant) === Some(Gomoku)
    }

    "be recognised as an initial fen by the wrapper" in {
      strategygames.format.FEN.FiveInARow(Pente.initialFen).initial must beTrue
    }

    "refuse a next colour the stones and captures contradict" in {
      val empty18 = List.fill(18)("19").mkString("/")
      Forsyth.<<(FEN(s"$empty18/BW17 b 1 - 0 0 1")).isDefined must beTrue
      Forsyth.<<(FEN(s"$empty18/BW17 w 1 - 0 0 1")) must beNone
      // black has captured two white stones, so one black and two white have been dropped
      Forsyth.<<(FEN(s"$empty18/B18 w 1 - 2 0 1")).isDefined must beTrue
      Forsyth.<<(FEN(s"$empty18/B18 b 1 - 2 0 1")) must beNone
    }
  }

  "the wrapper" should {

    "report each seat's captures as the history score" in {
      val after = rowCaptured
      (StratSituation.FiveInARow(after): StratSituation).board.history.score === Score(2, 0)
      StratHistory.FiveInARow(after.board.history).score === Score(2, 0)
    }
  }

  "the binary format" should {

    "round trip drops beyond the 256th point" in {
      val record =
        Vector(Vector("B@s19", "W@a1", "B@j10"), Vector("swap2", "W@r19", "B@s18"), Vector("W@n14"))
      Binary.writeActionStrs(record).toOption.flatMap(b => Binary.readActionStrs(b.toList).toOption) ===
        Some(record)
    }

    "keep every point distinct" in {
      val ucis = Pos.all.map(p => s"B@${p.key}")
      Binary.writeMoves(ucis).toOption.flatMap(b => Binary.readActionStrs(b.toList, ucis.size).toOption) ===
        Some(ucis.map(Vector(_)).toVector)
    }
  }

  "random games of pente" should {

    def pick[A](rng: Random, as: Seq[A]): A = as.drop(rng.nextInt(as.size)).head

    def act(rng: Random, g: Game): Game = {
      val s      = g.situation
      val drops  = s.dropsAsDrops
      val nearby = drops.filter(d => (d.pos.file.index - 9).abs <= 3 && (d.pos.rank.index - 9).abs <= 3)
      rng.nextInt(6) match {
        case 0 if s.canSwap  => g.apply(s.swap().getOrElse(sys.error("refused a legal swap")))
        case 1 if s.canSwap2 => g.apply(s.swap2().getOrElse(sys.error("refused a legal swap2")))
        case _               => g.apply(pick(rng, if (nearby.nonEmpty) nearby else drops))
      }
    }

    def played(seed: Int): Game =
      Iterator
        .iterate(penteStart)(g => if (g.situation.end) g else act(new Random(seed * 1000 + g.plies), g))
        .take(400)
        .find(_.situation.end)
        .getOrElse(sys.error(s"the random game on seed ${seed} never ended"))

    val games = (1 to 30).map(seed => played(20261012 + seed))

    "always end on a line of five, ten captures or a full board" in {
      games.forall(
        _.situation.status.exists(Set(Status.LineOfFive, Status.TenCaptures, Status.Draw))
      ) must beTrue
    }

    "make captures along the way" in {
      games.exists(_.situation.board.score.nonEmpty) must beTrue
    }

    "keep the stones dropped in step with the plies" in {
      games.forall { g =>
        val swaps = g.actionStrs.flatMap(t => t).count(a => a == "swap" || a == "swap2")
        g.situation.board.droppedCount == g.plies - swaps
      } must beTrue
    }

    "replay from the record to the same position" in {
      games.forall { g =>
        Replay
          .gameFromUciStrings(g.actionStrs.toList.flatMap(t => t), None, Pente)
          .toOption
          .map(r => Forsyth.>>(r).value)
          .contains(Forsyth.>>(g).value)
      } must beTrue
    }

    "regroup the record into the same turns from its binary form" in {
      games.forall { g =>
        Binary
          .writeActionStrs(g.actionStrs)
          .toOption
          .flatMap(bytes => Binary.readActionStrs(bytes.toList).toOption)
          .exists(_ == g.actionStrs)
      } must beTrue
    }

    "parse every action they played" in {
      games.forall(_.actionStrs.flatMap(t => t).forall(a => Uci(a).isDefined)) must beTrue
    }
  }

  "the game logic" should {
    "still be five in a row" in {
      StratVariant.FiveInARow(Pente).gameLogic === GameLogic.FiveInARow()
    }
  }
}
