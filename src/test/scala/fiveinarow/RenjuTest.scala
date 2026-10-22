package strategygames.fiveinarow

import scala.util.Random

import org.specs2.mutable.Specification

import strategygames.{
  Game => StratGame,
  GameFamily,
  GameLogic,
  GameMessage,
  MoveMetrics,
  Player,
  Situation => StratSituation,
  Status
}
import strategygames.format.{ Forsyth => StratForsyth, Uci => StratUci }
import strategygames.fiveinarow.format.{ Forsyth, Uci }
import strategygames.fiveinarow.format.pgn.Binary
import strategygames.fiveinarow.variant.{ Gomoku, Renju }
import strategygames.variant.{ Variant => StratVariant }

class RenjuTest extends Specification with FiveInARowTestSupport {

  private def renjuStart: Game = Game(Renju)

  private def blacks(keys: String*): Seq[(String, Role)] = keys.map(_ -> BlackStone)

  // white stones spaced along the edges, enough to make it black's turn without lines of their own
  private val spareWhites: List[String] =
    List(
      "a1",
      "c1",
      "e1",
      "g1",
      "i1",
      "k1",
      "m1",
      "o1",
      "a15",
      "c15",
      "e15",
      "g15",
      "i15",
      "k15",
      "m15",
      "o15"
    )

  private def blackToMove(keys: String*)(whites: String*): Board = {
    val spares = spareWhites.filterNot(whites.contains).take(keys.size - whites.size)
    boardIn(Renju)((blacks(keys*) ++ (whites ++ spares).map(_ -> WhiteStone))*)
  }

  private def forbidden(board: Board, key: String): Boolean =
    Forbidden.isForbidden(board, Pos.fromKey(key).get)

  private def dropOn(board: Board, key: String): Situation = {
    val s = situationOf(board)
    s.drop(s.nextColour, Pos.fromKey(key).get).fold(err => sys.error(err), _.situationAfter)
  }

  private def drops(game: Game): Set[String] = game.situation.drops.getOrElse(Nil).map(_.key).toSet

  // a 4-stone opening symmetric about the h file
  private def fourOnTheHFile: Game =
    play(renjuStart, "B@h8", "W@h9", "B@h10", "W@h11")

  "the renju variant" should {

    "be found by key, on 15x15, without a score" in {
      variant.Variant("renju") === Some(Renju)
      Renju.boardSize === Board.Dim15x15
      Renju.perfIcon === ''
      Renju.hasGameScore must beFalse
      StratVariant.FiveInARow(Renju).gameFamily === GameFamily.FiveInARow()
      StratVariant.FiveInARow(Renju).gameLogic === GameLogic.FiveInARow()
    }

    "start from the empty board in the placing step" in {
      Renju.initialFen.value === List.fill(15)("15").mkString("/") + " b 1 p 1"
      Forsyth.>>(renjuStart).value === Renju.initialFen.value
    }
  }

  "the taraguchi opening" should {

    "put the first stone on the centre" in {
      drops(renjuStart) === Set("h8")
      refused(renjuStart, "B@g8") must beTrue
      renjuStart.situation.canSwap must beFalse
    }

    "widen the square about the centre by one for each of the first five stones" in {
      val one   = play(renjuStart, "B@h8")
      val two   = play(one, "W@h9")
      val three = play(two, "B@i9")
      val four  = play(three, "W@g7")
      drops(one).size === 8
      drops(one) must contain("g7", "i9")
      drops(two).size === 23
      drops(three).size === 46
      drops(four).size === 77
      refused(one, "W@h10") must beTrue
      refused(four, "B@m8") must beTrue
      refused(four, "B@l8") must beFalse
    }

    "offer the player to move a swap after each of the first five stones" in {
      val one = play(renjuStart, "B@h8")
      one.situation.openingStep === OpeningStep.SwapOffered
      one.player === Player.P2
      one.situation.canSwap must beTrue
    }

    "hand the next stone to the other seat after a swap, and offer no second swap" in {
      val swapped = play(renjuStart, "B@h8", "swap")
      swapped.situation.blackSeat === Player.P2
      swapped.player === Player.P1
      swapped.situation.nextColour === WhiteStone
      swapped.situation.canSwap must beFalse
      swapped.situation.openingStep === OpeningStep.Placing
      play(swapped, "W@h9").situation.canSwap must beTrue
    }

    "take the swap after the fourth stone before black chooses" in {
      val four    = play(renjuStart, "B@h8", "W@h9", "B@i9", "W@g7")
      four.situation.canSwap must beTrue
      four.situation.canOffer10 must beTrue
      val swapped = play(four, "swap")
      swapped.situation.canSwap must beFalse
      swapped.situation.canOffer10 must beTrue
      swapped.player === Player.P2
    }

    "on choice 1, offer a swap after the fifth and free the sixth" in {
      val five = play(renjuStart, "B@h8", "W@h9", "B@i9", "W@g7", "B@j10")
      five.situation.canSwap must beTrue
      five.situation.canOffer10 must beFalse
      drops(five).size === 225 - 5
      val six  = play(five, "W@a1")
      six.situation.openingStep === OpeningStep.Play
      six.situation.canSwap must beFalse
    }

    "not offer ten fifth stones at any other point" in {
      play(renjuStart, "B@h8").situation.canOffer10 must beFalse
      refused(play(renjuStart, "B@h8", "W@h9", "B@i9"), "offer10") must beTrue
    }
  }

  "offering ten fifth stones" should {

    val offering = play(fourOnTheHFile, "offer10")

    "keep the turn with black, who drops anywhere" in {
      offering.situation.openingStep === OpeningStep.Offering
      offering.player === Player.P1
      refused(offering, "swap") must beTrue
      drops(offering) must contain("a1")
    }

    "leave the offered stones off the board" in {
      val g = play(offering, "B@a1")
      g.situation.board.pieces.size === 4
      g.situation.board.offered.map(_.key) === List("a1")
      g.player === Player.P1
      g.situation.nextColour === BlackStone
    }

    "refuse a stone symmetrical to one already offered" in {
      val g = play(offering, "B@g5")
      refused(g, "B@i5") must beTrue
      refused(g, "B@g5") must beTrue
      refused(g, "B@h5") must beFalse
      drops(g) must not(contain("i5"))
    }

    "allow mirror-image stones when the position has no symmetry" in {
      val asymmetric = play(renjuStart, "B@h8", "W@h9", "B@i9", "W@g7", "offer10", "B@g5")
      refused(asymmetric, "B@i5") must beFalse
    }

    val ten     = List("a1", "b1", "c1", "d1", "e1", "f1", "g1", "a2", "b2", "c2").map(k => s"B@$k")
    val offered = play(offering, ten*)

    "pass to white to pick once ten are offered" in {
      offered.situation.openingStep === OpeningStep.Picking
      offered.player === Player.P2
      offered.situation.drops must beSome(List.empty[Pos])
      offered.situation.selectSquaresActions.size === 10
      offered.actionStrs.last === ("offer10" +: ten).toVector
    }

    "keep the picked stone as black's fifth, clear the rest, and leave white to play the sixth" in {
      val picked = play(offered, "ss:c2")
      picked.situation.board.offered must beEmpty
      picked.situation.board.pieces.size === 5
      picked.situation.board(Pos.fromKey("c2").get).map(_.role) === Some(BlackStone)
      picked.player === Player.P2
      picked.situation.nextColour === WhiteStone
      picked.situation.canSwap must beFalse
      drops(picked).size === 225 - 5
      val six    = play(picked, "W@o15")
      six.situation.openingStep === OpeningStep.Play
      six.player === Player.P1
      six.actionStrs.last === Vector("ss:c2", "W@o15")
    }

    "refuse a pick that was not offered, or more than one" in {
      refused(offered, "ss:h1") must beTrue
      refused(offered, "ss:a1,b1") must beTrue
    }

    "round trip the offered stones through the fen" in {
      val fen  = Forsyth.>>(play(offering, "B@a1", "B@c2"))
      fen.value.split(' ').head.split('/').last === "b14"
      fen.value.split(' ').drop(1).take(3).mkString(" ") === "b 1 x"
      val read = Forsyth.<<@(Renju, fen).get
      read.board.offered.map(_.key).toSet === Set("a1", "c2")
      read.player === Player.P1
      Forsyth.<<@(Renju, Forsyth.>>(offered)).get.player === Player.P2
      Forsyth.<<(Forsyth.>>(offered)).map(_.board.variant) === Some(Renju)
      Forsyth.<<(Forsyth.>>(offered)).map(_.player) === Some(Player.P2)
      Forsyth.<<(Gomoku.initialFen).map(_.board.variant) === Some(Gomoku)
    }
  }

  "a forbidden move" should {

    "be a double three" in {
      forbidden(blackToMove("f8", "g8", "h6", "h7")(), "h8") must beTrue
    }

    "be a double three with a split three" in {
      forbidden(blackToMove("f8", "i8", "h6", "h7")(), "h8") must beTrue
    }

    "be a double four, closed fours included" in {
      forbidden(blackToMove("e8", "f8", "g8", "h4", "h5", "h6")("d8"), "h8") must beTrue
    }

    "be a double four in a single line" in {
      forbidden(blackToMove("c8", "e8", "g8", "i8")(), "f8") must beTrue
    }

    "be an overline" in {
      forbidden(blackToMove("d8", "e8", "f8", "g8", "i8")(), "h8") must beTrue
    }

    "not be a four-three" in {
      forbidden(blackToMove("e8", "f8", "g8", "h6", "h7")(), "h8") must beFalse
    }

    "not be a single three or a single four" in {
      forbidden(blackToMove("f8", "g8")(), "h8") must beFalse
      forbidden(blackToMove("e8", "f8", "g8")(), "h8") must beFalse
    }

    "not count a three that white has closed at both ends" in {
      forbidden(blackToMove("f8", "g8", "h6", "h7")("h4", "h9"), "h8") must beFalse
    }

    "not count a three whose straight four would itself be forbidden" in {
      val overlinePoints = List("e5", "e6", "e7", "e9", "e10", "e11", "i5", "i6", "i7", "i9", "i10", "i11")
      val board          = blackToMove(("f8" :: "g8" :: "h6" :: "h7" :: overlinePoints)*)()
      forbidden(board, "e8") must beTrue
      forbidden(board, "i8") must beTrue
      forbidden(board, "h8") must beFalse
    }

    "give way to exactly five, whatever else the stone makes" in {
      val board = blackToMove("d8", "e8", "f8", "g8", "h6", "h7", "i9", "j10")()
      forbidden(board, "h8") must beFalse
      dropOn(board, "h8").winner === Some(Player.P1)
    }

    "only ever apply to black" in {
      val board = boardIn(Renju)((blacks("a1") ++ Seq("f8", "g8", "h6", "h7").map(_ -> WhiteStone))*)
      Renju.isForbidden(board, WhiteStone, Pos.fromKey("h8").get) must beFalse
    }

    "be legal, and lose black the game on the spot" in {
      val after = dropOn(blackToMove("f8", "g8", "h6", "h7")(), "h8")
      after.end must beTrue
      after.winner === Some(Player.P2)
      after.status === Some(Status.ForbiddenMove)
      after.board.history.forbiddenDrop.map(_.key) === Some("h8")
    }

    "go to whichever seat holds white" in {
      val after = dropOn(blackToMove("f8", "g8", "h6", "h7")().swapColours, "h8")
      after.winner === Some(Player.P1)
    }

    "not exist in gomoku" in {
      val board = boardIn(Gomoku)(
        (blacks("f8", "g8", "h6", "h7") ++ spareWhites.take(4).map(_ -> WhiteStone))*
      )
      dropOn(board, "h8").end must beFalse
    }
  }

  "the forbidden move warning" should {

    "list the points where black's drop would lose, and raise the game message" in {
      val s = situationOf(blackToMove("f8", "g8", "h6", "h7")())
      s.forbiddenDrops.map(_.key) must contain("h8")
      s.gameMessage === Some(GameMessage.ForbiddenMoveWarning)
      (StratSituation.FiveInARow(s): StratSituation).gameMessage === Some(GameMessage.ForbiddenMoveWarning)
    }

    "stay quiet with no forbidden point, on white's turn, and in gomoku" in {
      situationOf(blackToMove("f8", "g8")()).gameMessage must beNone
      val whiteToMove = boardIn(Renju)(blacks("f8", "g8", "h6", "h7", "a1")*)
      situationOf(whiteToMove).gameMessage must beNone
      situationOf(
        boardIn(Gomoku)((blacks("f8", "g8", "h6", "h7") ++ spareWhites.take(4).map(_ -> WhiteStone))*)
      ).gameMessage must beNone
    }
  }

  "winning in renju" should {

    "not come from a black overline" in {
      val s = situationOf(boardIn(Renju)(blacks("a8", "b8", "c8", "d8", "e8", "f8")*))
      Renju.colourWithLine(s.board) must beNone
    }

    "come from a white overline" in {
      val s = situationOf(boardIn(Renju)(Seq("a8", "b8", "c8", "d8", "e8", "f8").map(_ -> WhiteStone)*))
      s.winner === Some(Player.P2)
    }

    "come from exactly five black" in {
      situationOf(boardIn(Renju)(blacks("a8", "b8", "c8", "d8", "e8")*)).winner === Some(Player.P1)
    }
  }

  "the wrapper" should {

    val lib = GameLogic.FiveInARow()
    val gf  = GameFamily.FiveInARow()

    def applyAll(game: StratGame, ucis: String*): StratGame =
      ucis.foldLeft(game) { (g, str) =>
        val uci = StratUci.apply(lib, gf, str).getOrElse(sys.error(s"unreadable uci ${str}"))
        g.applyUci(uci, MoveMetrics()).fold(err => sys.error(s"${str} refused: ${err}"), _._1)
      }

    val record = Vector(
      Vector("B@h8"),
      Vector("W@h9"),
      Vector("B@h10"),
      Vector("W@h11"),
      Vector("offer10", "B@a1", "B@b1", "B@c1", "B@d1", "B@e1", "B@f1", "B@g1", "B@a2", "B@b2", "B@c2"),
      Vector("ss:c2", "W@o15"),
      Vector("B@a15")
    )

    "offer offer10 and the picks among the actions" in {
      val four    = applyAll(StratGame(lib, StratVariant.FiveInARow(Renju)), record.take(4).flatMap(t => t)*)
      four.situation.canOffer10 must beTrue
      four.situation.actions.map(_.toUci.uci) must contain("offer10")
      val offered = applyAll(four, record(4)*)
      offered.situation.actions
        .map(_.toUci.uci)
        .toSet === record(4).drop(1).map(k => s"ss:${k.drop(2)}").toSet
      offered.situation.canDrop must beFalse
    }

    "play a recorded game into the same turns" in {
      val g = applyAll(StratGame(lib, StratVariant.FiveInARow(Renju)), record.flatMap(t => t)*)
      g.actionStrs === record
    }

    "round trip the record through the binary format" in {
      Binary.writeActionStrs(record).toOption.flatMap(b => Binary.readActionStrs(b.toList).toOption) ===
        Some(record)
    }

    "replay the record to the same position" in {
      val g = applyAll(StratGame(lib, StratVariant.FiveInARow(Renju)), record.flatMap(t => t)*)
      Replay
        .gameFromUciStrings(record.flatMap(t => t).toList, None, Renju)
        .toOption
        .map(Forsyth.>>(_).value) ===
        Some(StratForsyth.>>(lib, g).value)
    }

    "read offer10 and picks as uci" in {
      Uci("offer10") === Some(Uci.Offer10())
      Uci("ss:k10") === Some(Uci.SelectSquares(List(Pos.fromKey("k10").get)))
      Uci.piotr(Uci.SelectSquares(List(Pos.fromKey("k10").get)).piotr) ===
        Some(Uci.SelectSquares(List(Pos.fromKey("k10").get)))
    }
  }

  "random games of renju" should {

    def pick[A](rng: Random, as: Seq[A]): A = as.drop(rng.nextInt(as.size)).head

    def act(rng: Random, g: Game): Game = {
      val s = g.situation
      if (s.canSelectSquares) g.apply(pick(rng, s.selectSquaresActions))
      else {
        val drops  = s.dropsAsDrops
        val nearby = drops.filter(d => (d.pos.file.index - 7).abs <= 3 && (d.pos.rank.index - 7).abs <= 3)
        rng.nextInt(6) match {
          case 0 if s.canSwap    => g.apply(s.swap().getOrElse(sys.error("refused a legal swap")))
          case 1 if s.canOffer10 => g.apply(s.offer10().getOrElse(sys.error("refused a legal offer10")))
          case _                 => g.apply(pick(rng, if (nearby.nonEmpty) nearby else drops))
        }
      }
    }

    def played(seed: Int): Game =
      Iterator
        .iterate(renjuStart)(g => if (g.situation.end) g else act(new Random(seed * 1000 + g.plies), g))
        .take(300)
        .find(_.situation.end)
        .getOrElse(sys.error(s"the random game on seed ${seed} never ended"))

    val games = (1 to 30).map(seed => played(20261013 + seed))

    "always end on a line of five, a forbidden move or a full board" in {
      games.forall(
        _.situation.status.exists(Set(Status.LineOfFive, Status.ForbiddenMove, Status.Draw))
      ) must beTrue
    }

    "include swaps and offer10s among them" in {
      val all = games.flatMap(_.actionStrs.flatMap(t => t))
      all must contain("swap")
      all must contain("offer10")
    }

    "replay from the record to the same position" in {
      games.forall { g =>
        Replay
          .gameFromUciStrings(g.actionStrs.toList.flatMap(t => t), None, Renju)
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
  }
}
