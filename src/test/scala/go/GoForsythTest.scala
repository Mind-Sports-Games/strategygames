package strategygames.go

import org.specs2.mutable.Specification

import strategygames.go.format.{ FEN, Forsyth, Uci }
import strategygames.go.variant.{ Go13x13, Go19x19, Go9x9, Variant }

class GoForsythTest extends Specification with GoRulesTestSupport {

  private val emptyNineByNineRows = "9/9/9/9/9/9/9/9/9"
  private val nineByNineOpening   = "b - 0 55 0 0 55 0 1"

  private def nineByNine(rows: String = emptyNineByNineRows, tail: String = nineByNineOpening): FEN =
    FEN(s"${rows}${pocket} ${tail}")

  private def handicapStart(handicap: Int): FEN =
    FEN(s"${Go9x9.boardFenFromHandicap(handicap)}${pocket} w - ${handicap * 10} 55 0 0 55 0 1")

  private val handicapStarts = (1 to 9).toList.map(handicapStart)

  private val fourStoneHandicapStart = handicapStart(4)

  private val refusedFens = List(
    FEN("badfen"),
    FEN(s"${emptyNineByNineRows}${pocket} b - 0 55 0 0 55"),
    FEN(s"10/10/10/10/10/10/10/10/10/10${pocket} b - 0 55 0 0 55 0 1"),
    nineByNine(rows = "9/9/9/9/9/9/9/9/8"),
    nineByNine(rows = "9/9/9/9/9/9/9/9/9S"),
    nineByNine(rows = "9/9/9/9/9/9/9/9/8X"),
    nineByNine(tail = "x - 0 55 0 0 55 0 1"),
    nineByNine(tail = "ba - 0 55 0 0 55 0 1"),
    nineByNine(tail = "S - 0 55 0 0 55 0 1"),
    nineByNine(tail = "1 - 0 55 0 0 55 0 1"),
    nineByNine(tail = "b z9 0 55 0 0 55 0 1"),
    nineByNine(tail = "b a10 0 55 0 0 55 0 1"),
    nineByNine(tail = "b - 0 55 0 0 komi 0 1")
  )

  private def refused(fen: FEN) =
    (Forsyth.validate(fen) === false) and (Forsyth.<<@(Go9x9, fen).isEmpty === true)

  "a malformed go fen" should {
    "be refused when it is not a fen at all" in
      refused(FEN("badfen"))
    "be refused when it has too few fields" in
      refused(FEN(s"${emptyNineByNineRows}${pocket} b - 0 55 0 0 55"))
    "be refused when the row count is not a go board size" in
      refused(FEN(s"10/10/10/10/10/10/10/10/10/10${pocket} b - 0 55 0 0 55 0 1"))
    "be refused when a row does not fill the board" in
      refused(nineByNine(rows = "9/9/9/9/9/9/9/9/8"))
    "be refused when a row overflows the board" in
      refused(nineByNine(rows = "9/9/9/9/9/9/9/9/9S"))
    "be refused when a row names an unknown stone" in
      refused(nineByNine(rows = "9/9/9/9/9/9/9/9/8X"))
    "be refused when the turn field names no player" in
      refused(nineByNine(tail = "x - 0 55 0 0 55 0 1"))
    "be refused when the turn field carries more than the turn symbol" in
      refused(nineByNine(tail = "ba - 0 55 0 0 55 0 1"))
    "be refused when the turn field names a player the way a board row would" in
      refused(nineByNine(tail = "S - 0 55 0 0 55 0 1"))
    "be refused when the turn field names a player by number" in
      refused(nineByNine(tail = "1 - 0 55 0 0 55 0 1"))
    "be refused when the ko point names a file the board has not" in
      refused(nineByNine(tail = "b z9 0 55 0 0 55 0 1"))
    "be refused when the ko point names a rank the board has not" in
      refused(nineByNine(tail = "b a10 0 55 0 0 55 0 1"))
    "be refused when a numeric field is not a number" in
      refused(nineByNine(tail = "b - 0 55 0 0 komi 0 1"))
  }

  "an overflowing board row" should {
    "never be truncated into a playable position" in
      Forsyth.<<@(Go9x9, nineByNine(rows = "9/9/9/9/9/9/9/9/9S")) === None
  }

  "a board part that disagrees with the variant it is read as" should {
    "be refused rather than read into a board of the other size" in {
      (Forsyth.<<@(Go9x9, Go19x19.initialFen) === None) and
        (Forsyth.<<@(Go19x19, Go9x9.initialFen) === None)
    }
    "never leave a stone on a point the variant's board size has not" in {
      val nineteenRanks = FEN(
        s"S18/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19${pocket} b - 10 75 0 0 75 0 1"
      )
      (nineteenRanks.pieces.keys.toList === List(pointAt("a19"))) and
        (Forsyth.<<@(Go9x9, nineteenRanks) === None)
    }
  }

  "a fen read without being told which variant to read it as" should {
    val tenRanks = FEN(s"10/10/10/10/10/10/10/10/10/10${pocket} b - 0 55 0 0 55 0 1")
    "take the variant its row count names" in {
      (Forsyth.<<(Go13x13.initialFen).map(_.board.variant) === Some(Go13x13)) and
        (Forsyth.<<<(Go19x19.initialFen).map(_.situation.board.variant) === Some(Go19x19))
    }
    "fail loudly when its row count is no go board size" in {
      (tenRanks.variant must throwA[RuntimeException]) and
        (Forsyth.<<(tenRanks) must throwA[RuntimeException]) and
        (Forsyth.<<<(tenRanks) must throwA[RuntimeException])
    }
  }

  "a well formed go fen" should {
    "be accepted for the initial position of every variant" in
      forall(Variant.all)(variant => Forsyth.validate(variant.initialFen) === true)
    "be accepted for every 9x9 handicap board" in
      forall(handicapStarts)(fen => Forsyth.validate(fen) === true)
    "be accepted with a ko point the board has" in
      Forsyth.validate(nineByNine(tail = "b c3 0 55 0 0 55 0 1")) === true
    "keep its ko point off the list of legal drops" in {
      val situation = situationFrom(nineByNine(tail = "b c3 0 55 0 0 55 0 1"))
      (situation.board.ko === Some(pointAt("c3"))) and
        (dropKeysOf(situation) must not(contain("c3")))
    }
  }

  "the legacy nine field fen" should {
    val legacy = FEN(s"${emptyNineByNineRows}${pocket} b - 0 55 0 0 55 1")
    "read into a situation" in
      Forsyth.<<@(Go9x9, legacy).isEmpty === false
    "re emit as the ten field form" in
      Forsyth.<<@(Go9x9, legacy).map(fenOf) === Some(Go9x9.initialFen)
    "stay outside what the ten field validator accepts" in
      Forsyth.validate(legacy) === false
  }

  "the two gates on a go fen" should {
    "reach the same verdict on every turn symbol" in
      forall(List("b", "w", "B", "W", "ba", "wb", "S", "s", "N", "1", "2", "x", "-", "")) { turn =>
        val fen = nineByNine(tail = s"${turn} - 0 55 0 0 55 0 1")
        Forsyth.validate(fen) === Forsyth.<<@(Go9x9, fen).isDefined
      }
    "reach the same verdict on every ten field refusal" in
      refusedFens.filter(fen => Forsyth.validate(fen) != Forsyth.<<@(Go9x9, fen).isDefined) === Nil
  }

  "the stones a fen names" should {
    "be no stones at all for every variant's initial position" in
      forall(Variant.all)(variant =>
        Forsyth.<<@(variant, variant.initialFen).map(_.board.pieces) === Some(Map.empty[Pos, Piece])
      )
    "be the star points a handicap fen lays out, named one by one" in {
      val blackStone = Piece(P1, Stone)
      (Forsyth.<<@(Go9x9, fourStoneHandicapStart).map(_.board.pieces) === Some(
        Map(
          pointAt("c3") -> blackStone,
          pointAt("g3") -> blackStone,
          pointAt("c7") -> blackStone,
          pointAt("g7") -> blackStone
        )
      )) and
        (Forsyth.<<@(Go9x9, handicapStart(1)).map(_.board.pieces) === Some(
          Map(pointAt("c7") -> blackStone)
        )) and
        (Forsyth.<<@(Go9x9, handicapStart(9)).map(_.board.pieces.size) === Some(9))
    }
    "read a mid game position with two digit ranks the same way" in {
      val fen = FEN(
        s"S17s/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19${pocket} b - 10 85 0 0 75 0 2"
      )
      (Forsyth.<<@(Go19x19, fen).map(_.board.pieces) === Some(fen.pieces)) and
        (fen.pieces.keys.toList.sortBy(_.index) === List(pointAt("a19"), pointAt("s19")))
    }
  }

  "the board rows written from a piece map" should {
    "reproduce the board field they were read from" in
      forall(handicapStarts)(fen => Forsyth.boardRows(Go9x9, fen.pieces) === fen.board)
    "carry no pocket, unlike the board part of a fen" in
      Forsyth.boardPart(Board.init(Go13x13)) ===
      s"${Forsyth.boardRows(Go13x13, Go13x13.initialFen.pieces)}${pocket}"
  }

  "the board state a go board exports on its own" should {
    // NOTE: ten fields, turn and full move included, where every other logic exports board-only state.
    // Kept that way on purpose — the string is a wire format lila's callers index by field position.
    // `go/format/Forsyth.exportBoard` says why, and carries the TODO for narrowing it.
    "name the player the ply count hands the move to, and the full move it stands at" in
      Forsyth.exportBoard(Board.init(Go9x9)) === s"${emptyNineByNineRows}${pocket} b - 0 55 0 0 55 0 1"
    "be every field of the game's own fen" in {
      val played = playing(Go9x9, List("d4", "f4", "e6", "pass"))
      Forsyth.exportBoard(played.situation.board) === Forsyth.>>(played).value
    }
    "keep telling a settled board apart from an unsettled one" in {
      val awaitingSelection = playing(Go9x9, List("d4", "f4", "e6", "pass", "pass"))
      val settled           = playingOn(awaitingSelection, List("ss:"))
      Forsyth.exportBoard(awaitingSelection.situation.board) !==
        Forsyth.exportBoard(settled.situation.board)
    }
  }

  "removing dead stones" should {
    "lift the named stones and keep every other field of the fen" in
      Forsyth
        .removeDeadStones(Go9x9, fourStoneHandicapStart, List(pointAt("c7"), pointAt("g3")))
        .value === s"9/9/6S2/9/9/9/2S6/9/9${pocket} w - 40 55 0 0 55 0 1"
    "lift them from every 9x9 handicap board, touching nothing but the board field" in
      forall(handicapStarts) { fen =>
        val lifted = Forsyth.removeDeadStones(Go9x9, fen, List(pointAt("c7"), pointAt("g3")))
        (lifted.pieces === fen.pieces -- List(pointAt("c7"), pointAt("g3"))) and
          (lifted.value.split(' ').drop(1).toList === fen.value.split(' ').drop(1).toList)
      }
    "leave the fen alone when nothing is named" in
      Forsyth.removeDeadStones(Go9x9, fourStoneHandicapStart, Nil) === fourStoneHandicapStart
  }

  "the initial fen of a go variant" should {
    "come from the variant itself" in
      forall(Variant.all)(variant => Variant(variant.key).map(_.initialFen) === Some(variant.initialFen))
  }

  "a replay started from a handicap fen" should {
    val replayed = Replay(List(Uci("s@a1")).flatten, Some(fourStoneHandicapStart), Go9x9)
    "keep the handicap position as the replay's setup" in
      replayed.toOption.map(replay => fenOf(replay.setup)) ===
      Some(fenOf(situationFrom(fourStoneHandicapStart)))
    "keep the handicap stones on the board it plays on" in {
      (replayed.toOption.map(_.setup.situation.board.pieces.size) === Some(4)) and
        (replayed.toOption.map(_.state.situation.board.pieces.size) === Some(5))
    }
  }

  "the go board invariant" should {
    "hold for the starting board of every variant" in
      forall(Variant.all)(variant => Board.init(variant).valid(true) === true)
    "hold for a handicap board" in
      situationFrom(fourStoneHandicapStart).board.valid(true) === true
    "refuse a stone on a point the board size has not" in
      Board
        .init(Go9x9)
        .copy(pieces = Map(pointAt("s19") -> Piece(P1, Stone)))
        .valid(true) === false
    "hold for every position of a played 19x19 game" in {
      val played = playing(Go19x19, List("a1", "s19", "b2", "r18", "pass", "pass"))
      played.situation.board.valid(true) === true
    }
  }
}
