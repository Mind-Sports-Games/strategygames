package strategygames.go

import org.specs2.mutable.Specification

import strategygames.{ GameFamily, GameLogic, Player, Status }
import strategygames.go.format.{ FEN, Forsyth, Uci }
import strategygames.go.variant.{
  Go13x13,
  Go13x13Joansala,
  Go19x19,
  Go19x19Joansala,
  Go9x9,
  Go9x9Joansala,
  Variant
}

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
    "be registered alongside the parked joansala ones" in {
      (Variant.all.size === 6) and
        (Variant.byKey.get("go9x9Joansala") === Some(Go9x9Joansala)) and
        (Variant.byKey.get("go13x13Joansala") === Some(Go13x13Joansala)) and
        (Variant.byKey.get("go19x19Joansala") === Some(Go19x19Joansala)) and
        (Variant.byId.get(5) === Some(Go9x9Joansala)) and
        (Variant.byId.get(6) === Some(Go13x13Joansala)) and
        (Variant.byId.get(7) === Some(Go19x19Joansala)) and
        (Variant.all.map(_.id).distinct.size === 6) and
        (Variant.all.map(v => v.key -> v.perfId).toMap === Map(
          "go9x9"           -> 500,
          "go13x13"         -> 501,
          "go19x19"         -> 502,
          "go9x9Joansala"   -> 503,
          "go13x13Joansala" -> 504,
          "go19x19Joansala" -> 505
        ))
    }

    "stay the default and the opening sensible ones" in {
      (Variant.default === Go19x19) and
        (Variant.openingSensibleVariants === Set(Go9x9, Go13x13, Go19x19))
    }

    "be reachable from the strategygames wrapper" in {
      val wrapped = strategygames.variant.Variant.all(GameLogic.Go())
      (wrapped.filter(_.key.endsWith("Joansala")).map(_.key)
        === List("go19x19Joansala", "go13x13Joansala", "go9x9Joansala")) and
        (GameFamily.Go().variants.size === 6) and
        (strategygames.variant.Variant(GameLogic.Go(), "go19x19Joansala").map(_.toGo)
          === Some(Go19x19Joansala))
    }

    "run on the pure scala engine, unlike the parked joansala ones" in {
      (Api.positionFromVariant(Go9x9).isInstanceOf[ScalaPosition] === true) and
        (Api.positionFromVariant(Go13x13).isInstanceOf[ScalaPosition] === true) and
        (Api.positionFromVariant(Go19x19).isInstanceOf[ScalaPosition] === true) and
        (Api.positionFromVariant(Go19x19Joansala).isInstanceOf[ScalaPosition] === false) and
        (Api
          .positionFromVariantNameAndFEN("go9x9", Go9x9.initialFen.value)
          .isInstanceOf[ScalaPosition] === true) and
        (Forsyth
          .<<@(Go13x13, Go13x13.initialFen)
          .map(_.board.apiPosition.isInstanceOf[ScalaPosition]) === Some(true))
    }

    "share the setup of the parked joansala variant of the same size" in {
      (Go9x9Joansala.initialFen === Go9x9.initialFen) and
        (Go13x13Joansala.initialFen === Go13x13.initialFen) and
        (Go19x19Joansala.initialFen === Go19x19.initialFen) and
        (Go9x9Joansala.komi === 5.5) and
        (Go13x13Joansala.komi === 7.5) and
        (Go19x19Joansala.komi === 7.5) and
        (Go9x9Joansala.boardFenFromHandicap(9) === Go9x9.boardFenFromHandicap(9)) and
        (Go13x13Joansala.boardFenFromHandicap(9) === Go13x13.boardFenFromHandicap(9)) and
        (Go19x19Joansala.boardFenFromHandicap(9) === Go19x19.boardFenFromHandicap(9)) and
        (Go9x9Joansala.fenFromSetupConfig(4, 55) === Go9x9.fenFromSetupConfig(4, 55))
    }
  }

  "an initial scala go position" should {
    "emit the initial fen of its variant" in {
      List(Go9x9, Go13x13, Go19x19).map(v => Api.positionFromVariant(v).fen)
        === List(Go9x9.initialFen, Go13x13.initialFen, Go19x19.initialFen)
    }

    "offer every point as a drop, plus a pass" in {
      List(Go9x9, Go13x13, Go19x19).map { v =>
        val position = Api.positionFromVariant(v)
        (position.legalDrops.size, position.legalActions.size, position.pieceMap.size)
      } === List((81, 82, 0), (169, 170, 0), (361, 362, 0))
    }

    "not have ended" in {
      val position = Api.positionFromVariant(Go9x9)
      (position.gameEnd === false) and
        (position.isRepetition === false) and
        (position.gameResult === GameResult.Ongoing()) and
        (position.turn === "b") and
        (position.playerTurn === 1)
    }
  }

  "a scripted go9x9 game" should {
    val passed = playing(Game(Go9x9), scriptedNineByNine ++ List("pass", "pass"))
    val ended  = playing(passed, List("ss:i1"))

    "capture the black stone at d5 when white closes it in" in {
      (passed.situation.board.apiPosition.pieceMap.size === 11) and
        (passed.situation.board.apiPosition.pieceMap.get(Pos.D5) === None) and
        (passed.situation.board.apiPosition.pieceMap.get(Pos.D6) === Some(Piece(P2, Stone)))
    }

    "await dead stone selection after two passes" in {
      (passed.situation.canSelectSquares === true) and
        (passed.situation.end === false) and
        (passed.situation.board.apiPosition.gameEnd === false) and
        (passed.situation.board.apiPosition.fen.value
          === "9/9/2s6/3s5/2s1s3S/3s3S1/6S2/5S3/4S3s[SSSSSSSSSSssssssssss] b - 50 125 0 1 55 2 8")
    }

    "end as a variant end once the dead stone is removed" in {
      (ended.situation.end === true) and
        (ended.situation.status === Some(Status.VariantEnd)) and
        (ended.situation.winner === Some(Player.P1)) and
        (ended.situation.board.apiPosition.pieceMap.size === 10) and
        (ended.situation.board.apiPosition.p1Score === 15.0) and
        (ended.situation.board.apiPosition.p2Score === 11.5) and
        (ended.situation.board.apiPosition.gameOutcome === 1000) and
        (ended.situation.board.apiPosition.gameResult === GameResult.VariantEnd()) and
        (ended.situation.board.apiPosition.fen.value
          === "9/9/2s6/3s5/2s1s3S/3s3S1/6S2/5S3/4S4[SSSSSSSSSSssssssssss] b - 150 115 0 1 55 3 8")
    }

    "offer no further actions once ended" in {
      (ended.situation.drops === None) and
        (ended.situation.board.apiPosition.legalActions.size === 0)
    }

    "export a board fen that reloads into the same position" in {
      val exported = Forsyth.>>(ended)
      val reloaded = Forsyth.<<@(Go9x9, exported)
      (exported.value
        === "9/9/2s6/3s5/2s1s3S/3s3S1/6S2/5S3/4S4[SSSSSSSSSSssssssssss] w - 150 115 0 1 55 3 8") and
        (reloaded.map(_.board.apiPosition.fen) === Some(exported)) and
        (reloaded.map(_.end) === Some(true)) and
        (reloaded.map(_.winner) === Some(Some(Player.P1)))
    }
  }

  "a key naming a square the board does not have" should {
    val awaitingSelection = scriptedNineByNine ++ List("pass", "pass")

    def selecting(uci: String) =
      Api.positionFromVariantAndMoves(Go9x9, awaitingSelection ++ List(uci))

    "be ignored in a dead stone selection, as the joansala engine ignores it" in {
      val liftingNothing = selecting("ss:")
      val liftingN4      = selecting("ss:n4")
      (Api.uciToMove("s@n4", Go9x9) === Api.uciToMove("s@e5", Go9x9)) and
        (liftingN4.pieceMap.get(Pos.E5) === Some(Piece(P2, Stone))) and
        (liftingN4.fen === liftingNothing.fen)
    }

    "not stop the on board keys beside it from being lifted" in {
      (selecting("ss:i1,n4").fen === selecting("ss:i1").fen) and
        (selecting("ss:i1,n4").pieceMap.get(Pos.I1) === None)
    }

    "be refused outright as a drop, rather than played somewhere else" in {
      Api.positionFromVariantAndMoves(Go9x9, List("s@n4")) must throwAn[Exception]
    }
  }

  "an action that is neither a pass, a selection, nor a placement" should {
    "be refused rather than aliased onto a point of the board" in {
      Api.positionFromVariantAndMoves(Go9x9, List("garbage")) must throwAn[Exception]
    }
  }

  "a dead stone key that is not a coordinate at all" should {
    "be refused rather than dropped in silence" in {
      Api.positionFromVariantAndMoves(
        Go9x9,
        scriptedNineByNine ++ List("pass", "pass", "ss:zz")
      ) must throwAn[Exception]
    }
  }

  "a go9x9 game from a handicap position" should {
    val handicapFen = Go9x9.fenFromSetupConfig(4, 55)
    val game        = Game(Some(Go9x9), Some(handicapFen))

    "place the handicap stones and let p2 start" in {
      (handicapFen.value === "9/9/2S3S2/9/9/9/2S3S2/9/9[SSSSSSSSSSssssssssss] w - 40 55 0 0 55 0 1") and
        (handicapFen.handicap === Some(4)) and
        (game.situation.player === Player.P2) and
        (game.situation.board.apiPosition.pieceMap.size === 4) and
        (game.situation.board.apiPosition.turn === "w")
    }

    "score the whole board to the handicapped player" in {
      game.situation.board.apiPosition.fen.value
        === "9/9/2S3S2/9/9/9/2S3S2/9/9[SSSSSSSSSSssssssssss] w - 810 55 0 0 55 0 1"
    }

    "keep the handicap fen as the starting fen after a drop" in {
      val played = playing(game, List("s@a1"))
      (played.situation.board.apiPosition.initialFen === handicapFen) and
        (played.situation.board.apiPosition.fen.value
          === "9/9/2S3S2/9/9/9/2S3S2/9/s8[SSSSSSSSSSssssssssss] b - 40 65 0 0 55 0 2")
    }
  }

  "the scala engine and the parked joansala engine" should {
    "agree on the fen of a drop only go13x13 game" in {
      val moves = List("s@d4", "s@k10", "s@k4", "s@d10")
      playing(Game(Go13x13), moves).situation.board.apiPosition.fen
        === playing(Game(Go13x13Joansala), moves).situation.board.apiPosition.fen
    }

    "agree on the fen of a drop only go19x19 game" in {
      val moves = List("s@d4", "s@q16", "s@q4", "s@d16")
      playing(Game(Go19x19), moves).situation.board.apiPosition.fen
        === playing(Game(Go19x19Joansala), moves).situation.board.apiPosition.fen
    }
  }

  "a scripted go19x19 game" should {
    val moves  = List("s@a19", "s@s19", "s@a18", "s@s1", "pass", "pass")
    val passed = playing(Game(Go19x19), moves)
    val ended  = playing(passed, List("ss:"))

    "read and play two digit ranks" in {
      (passed.situation.board.apiPosition.pieceMap.get(Pos.A19) === Some(Piece(P1, Stone))) and
        (passed.situation.board.apiPosition.pieceMap.get(Pos.S19) === Some(Piece(P2, Stone))) and
        (passed.situation.canSelectSquares === true)
    }

    "end with a select squares that removes nothing" in {
      (ended.situation.end === true) and
        (ended.situation.board.apiPosition.pieceMap.size === 4) and
        (ended.situation.board.apiPosition.fen.value.split(' ').last === "4") and
        (ended.situation.winner === Some(Player.P2))
    }
  }

  "a scala go position loaded from a mid game fen" should {
    val fen       = FEN(
      "9/9/2s6/3s5/2s1s3S/3s3S1/6S2/5S3/4S3s[SSSSSSSSSSssssssssss] b - 50 125 0 1 55 0 8"
    )
    val situation = Forsyth.<<@(Go9x9, fen)

    "keep its own variant, which size inference now also names" in {
      (situation.map(_.board.variant) === Some(Go9x9)) and
        (fen.variant === Go9x9) and
        (Forsyth.<<@(Go9x9Joansala, fen).map(_.board.variant) === Some(Go9x9Joansala))
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
        (replayed.map(_.last.board.apiPosition.fen)
          === Some(playing(Game(Go9x9), actions).situation.board.apiPosition.fen))
    }

    "end on a fourth pass without any dead stone selection" in {
      val replayed = replaying(scriptedNineByNine ++ List("pass", "pass", "pass", "pass"))
      (replayed.map(_.last.end) === Some(true)) and
        (replayed.map(_.last.board.apiPosition.pieceMap.size) === Some(11))
    }
  }

}
