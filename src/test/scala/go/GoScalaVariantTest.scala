package strategygames.go

import org.specs2.mutable.Specification

import strategygames.{ GameFamily, GameLogic, Player, Score, Status }
import strategygames.go.format.{ FEN, Forsyth, Uci }
import strategygames.go.variant.{ Go13x13, Go19x19, Go9x9, Variant }

class GoScalaVariantTest extends Specification {

  private def playing(game: Game, ucis: List[String]): Game =
    ucis.foldLeft(game) { (played, uci) =>
      Uci(uci)
        .map(played.apply(_).map { case (next, _) => next })
        .getOrElse(sys.error(s"unreadable uci: ${uci}"))
        .valueOr(error => sys.error(s"cannot play ${uci}: ${error}"))
    }

  private val scriptedNineByNine = List(
    "s@g3",
    "s@c7",
    "s@f2",
    "s@e5",
    "s@e1",
    "s@d4",
    "s@h4",
    "s@i1",
    "s@i5",
    "s@c5",
    "s@d5",
    "s@d6"
  )

  "the canonical go variants" should {
    "be the only registered go variants" in {
      (Variant.all.size === 3) and
        (Variant.byKey.get("go9x9") === Some(Go9x9)) and
        (Variant.byKey.get("go13x13") === Some(Go13x13)) and
        (Variant.byKey.get("go19x19") === Some(Go19x19)) and
        (Variant.byId.get(1) === Some(Go9x9)) and
        (Variant.byId.get(2) === Some(Go13x13)) and
        (Variant.byId.get(4) === Some(Go19x19)) and
        (Variant.byId.get(5) === None) and
        (Variant.byId.get(6) === None) and
        (Variant.byId.get(7) === None) and
        (Variant.all.map(v => v.key -> v.perfId).toMap === Map(
          "go9x9"   -> 500,
          "go13x13" -> 501,
          "go19x19" -> 502
        ))
    }

    "stay the default and the opening sensible ones" in {
      (Variant.default === Go19x19) and
        (Variant.openingSensibleVariants === Set(Go9x9, Go13x13, Go19x19))
    }

    "be reachable from the strategygames wrapper" in {
      val wrapped = strategygames.variant.Variant.all(GameLogic.Go())
      (wrapped.map(_.key) === List("go19x19", "go13x13", "go9x9")) and
        (GameFamily.Go().variants.size === 3) and
        (strategygames.variant.Variant(GameLogic.Go(), "go19x19Joansala") === None)
    }

    "open on an empty board" in {
      forall(List(Go9x9, Go13x13, Go19x19))(v =>
        Forsyth.<<@(v, v.initialFen).map(_.board.pieces.isEmpty) === Some(true)
      )
    }

    "carry their setup traits" in {
      (Go9x9.komi === 5.5) and
        (Go13x13.komi === 7.5) and
        (Go19x19.komi === 7.5)
    }
  }

  "an initial scala go position" should {
    "emit the initial fen of its variant" in {
      List(Go9x9, Go13x13, Go19x19).map(v => Forsyth.>>(Game(v)))
        === List(Go9x9.initialFen, Go13x13.initialFen, Go19x19.initialFen)
    }

    "offer every point as a drop, and a pass beside them" in {
      List(Go9x9, Go13x13, Go19x19).map { v =>
        val situation = Game(v).situation
        (v.validDrops(situation).size, situation.pass().isValid, situation.board.pieces.size)
      } === List((81, true, 0), (169, true, 0), (361, true, 0))
    }

    "not have ended" in {
      val situation = Game(Go9x9).situation
      (situation.end === false) and
        (situation.isRepetition === false) and
        (situation.status === None) and
        (situation.player === Player.P1) and
        (Forsyth.>>(situation).value.split(' ')(1) === "b")
    }
  }

  "a scripted go9x9 game" should {
    val passed = playing(Game(Go9x9), scriptedNineByNine ++ List("pass", "pass"))
    val ended  = playing(passed, List("ss:i1"))

    "capture the black stone at d5 when white closes it in" in {
      (passed.situation.board.pieces.size === 11) and
        (passed.situation.board.pieces.get(Pos.D5) === None) and
        (passed.situation.board.pieces.get(Pos.D6) === Some(Piece(P2, Stone)))
    }

    "await dead stone selection after two passes" in {
      (passed.situation.canSelectSquares === true) and
        (passed.situation.end === false) and
        (passed.situation.board.deadStonesSelected === false) and
        (Forsyth.>>(passed).value
          === "9/9/2s6/3s5/2s1s3S/3s3S1/6S2/5S3/4S3s[SSSSSSSSSSssssssssss] b - 50 125 0 1 55 2 8")
    }

    "end as a variant end once the dead stone is removed" in {
      (ended.situation.end === true) and
        (ended.situation.status === Some(Status.VariantEnd)) and
        (ended.situation.winner === Some(Player.P1)) and
        (ended.situation.board.pieces.size === 10) and
        (Go9x9.areaScore(ended.situation.board) === Score(150, 115)) and
        (Forsyth.>>(ended).value
          === "9/9/2s6/3s5/2s1s3S/3s3S1/6S2/5S3/4S4[SSSSSSSSSSssssssssss] w - 150 115 0 1 55 3 8")
    }

    "offer no further actions once ended" in {
      (ended.situation.drops === None) and
        (ended.situation.canDrop === false)
    }

    "export a board fen that reloads into the same position" in {
      val exported = Forsyth.>>(ended)
      val reloaded = Forsyth.<<@(Go9x9, exported)
      (exported.value
        === "9/9/2s6/3s5/2s1s3S/3s3S1/6S2/5S3/4S4[SSSSSSSSSSssssssssss] w - 150 115 0 1 55 3 8") and
        (reloaded.map(s => Forsyth.>>(s)) === Some(exported)) and
        (reloaded.map(_.end) === Some(true)) and
        (reloaded.map(_.winner) === Some(Some(Player.P1)))
    }
  }

  "a key naming a square the board does not have" should {
    val awaitingSelection = playing(Game(Go9x9), scriptedNineByNine ++ List("pass", "pass"))

    def selecting(uci: String) = playing(awaitingSelection, List(uci))

    "be ignored in a dead stone selection" in {
      (selecting("ss:n4").situation.board.pieces.get(Pos.E5) === Some(Piece(P2, Stone))) and
        (Forsyth.>>(selecting("ss:n4")) === Forsyth.>>(selecting("ss:")))
    }

    "not stop the on board keys beside it from being lifted" in {
      (Forsyth.>>(selecting("ss:i1,n4")) === Forsyth.>>(selecting("ss:i1"))) and
        (selecting("ss:i1,n4").situation.board.pieces.get(Pos.I1) === None)
    }

    "be refused outright as a drop, rather than played somewhere else" in {
      Game(Go9x9).situation.drop(Role.defaultRole, Pos.N4).isInvalid === true
    }
  }

  "an action that is neither a pass, a selection, nor a placement" should {
    "be refused rather than aliased onto a point of the board" in {
      (Uci("garbage") must throwAn[Exception]) and
        (Replay.gameFromUciStrings(
          Vector(Vector("garbage")),
          Player.P2,
          None,
          Go9x9
        ) must throwAn[Exception])
    }
  }

  "a go9x9 game from a handicap position" should {
    val handicapFen = Go9x9.fenFromSetupConfig(4, 55)
    val game        = Game(Some(Go9x9), Some(handicapFen))

    "place the handicap stones and let p2 start" in {
      (handicapFen.value === "9/9/2S3S2/9/9/9/2S3S2/9/9[SSSSSSSSSSssssssssss] w - 40 55 0 0 55 0 1") and
        (handicapFen.handicap === Some(4)) and
        (game.situation.player === Player.P2) and
        (game.situation.board.pieces.size === 4)
    }

    "score the whole board to the handicapped player" in {
      Forsyth.>>(game).value
        === "9/9/2S3S2/9/9/9/2S3S2/9/9[SSSSSSSSSSssssssssss] w - 810 55 0 0 55 0 1"
    }

    "keep the handicap stones on the board after a drop" in {
      val played = playing(game, List("s@a1"))
      Forsyth.>>(played).value
        === "9/9/2S3S2/9/9/9/2S3S2/9/s8[SSSSSSSSSSssssssssss] b - 40 65 0 0 55 0 2"
    }
  }

  "a scripted go19x19 game" should {
    val moves  = List("s@a19", "s@s19", "s@a18", "s@s1", "pass", "pass")
    val passed = playing(Game(Go19x19), moves)
    val ended  = playing(passed, List("ss:"))

    "read and play two digit ranks" in {
      (passed.situation.board.pieces.get(Pos.A19) === Some(Piece(P1, Stone))) and
        (passed.situation.board.pieces.get(Pos.S19) === Some(Piece(P2, Stone))) and
        (passed.situation.canSelectSquares === true)
    }

    "end with a select squares that removes nothing" in {
      (ended.situation.end === true) and
        (ended.situation.board.pieces.size === 4) and
        (Forsyth.>>(ended).value.split(' ').last === "4") and
        (ended.situation.winner === Some(Player.P2))
    }
  }

  "a scripted go13x13 game" should {
    val played = playing(Game(Go13x13), List("s@e1", "s@d2", "s@e3", "s@h5", "s@k11"))

    "write a board of thirteen ranks, each thirteen points wide" in {
      Forsyth.>>(played).value ===
        "13/13/10S2/13/13/13/13/13/7s5/13/4S8/3s9/4S8[SSSSSSSSSSssssssssss] w - 30 95 0 0 75 0 3"
    }

    "hold each stone on the point its key names, and no other" in {
      played.situation.board.pieces === Map(
        Pos.E1  -> Piece(P1, Stone),
        Pos.D2  -> Piece(P2, Stone),
        Pos.E3  -> Piece(P1, Stone),
        Pos.H5  -> Piece(P2, Stone),
        Pos.K11 -> Piece(P1, Stone)
      )
    }

    "reload from the fen it wrote into the very same position" in {
      val reloaded = Forsyth.<<@(Go13x13, Forsyth.>>(played))
      (reloaded.map(_.board.pieces) === Some(played.situation.board.pieces)) and
        (reloaded.map(s => Forsyth.>>(s).value) === Some(Forsyth.>>(played).value))
    }

    "give black every point of an otherwise empty thirteen by thirteen board" in {
      val loneStone = Forsyth.>>(playing(Game(Go13x13), List("s@g7")))
      (loneStone.player1Score === 1690) and (loneStone.player2Score === 75)
    }
  }

  "a scala go position loaded from a mid game fen" should {
    val fen       = FEN(
      "9/9/2s6/3s5/2s1s3S/3s3S1/6S2/5S3/4S3s[SSSSSSSSSSssssssssss] b - 50 125 0 1 55 0 8"
    )
    val situation = Forsyth.<<@(Go9x9, fen)

    "keep its own variant, which size inference also names" in {
      (situation.map(_.board.variant) === Some(Go9x9)) and
        (fen.variant === Some(Go9x9))
    }

    "round trip through forsyth" in {
      situation.map(s => Forsyth.>>(s).value) === Some(fen.value)
    }
  }

  "a replayed go9x9 game" should {
    def replaying(ucis: List[String]) =
      Replay
        .situationsFromUci(ucis.flatMap(Uci(_)), Some(Go9x9.initialFen), Go9x9)
        .toOption

    "reach the position the played game reached" in {
      val actions  = scriptedNineByNine ++ List("pass", "pass", "ss:i1")
      val replayed = replaying(actions)
      (replayed.map(_.size) === Some(actions.size + 1)) and
        (replayed.map(_.last.end) === Some(true)) and
        (replayed.map(situations => Forsyth.>>(situations.last))
          === Some(Forsyth.>>(playing(Game(Go9x9), actions))))
    }

    "end on a fourth pass without any dead stone selection" in {
      val replayed = replaying(scriptedNineByNine ++ List("pass", "pass", "pass", "pass"))
      (replayed.map(_.last.end) === Some(true)) and
        (replayed.map(_.last.board.pieces.size) === Some(11))
    }
  }

}
