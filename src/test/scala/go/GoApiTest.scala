package strategygames.go

import org.specs2.matcher.ValidatedMatchers
import org.specs2.mutable.Specification

class GoApiTest extends Specification with ValidatedMatchers {

  "Go initial fen" should {
    val fen = variant.Go19x19.initialFen.value
    "be valid" in {
      Api.validateFEN(fen) must_== true
    }
    // println(fen.matches(Api.fenRegex))

    val fen2 =
      "19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/1SSss14[SSSSSSSSSSssssssssss] b - 0 65 65 1"
    "true due to allowing >1 for multiple empty spaces" in {
      Api.validateFEN(fen2) must_== true
    }
    // println(fen2.matches(Api.fenRegex))

    val fen4 =
      "19/19/19/19/19/19/19/19/19/19/19/19/SsSsSsSsSsSsSsSsSsSs/19/19/19/19/19/19[SSSSSSSSSSssssssssss] w - 0 65 65 1"
    // println(fen4.matches(Api.fenRegex))
    "false due to more stone in row than width" in {
      Api.validateFEN(fen4) must_== false
    }

    val game = Api.position(variant.Go19x19)
    "match that of the initial game board fen" in {
      game.fen.value.split(" ").take(3) must_== fen.split(" ").take(3)
    }
  }

  "Go situation legal drops" should {
    val game = Api.position(variant.Go19x19)
    "19*19 legal drops " in {
      game.legalDrops.size must_== 19 * 19 // 0 to 360
    }
  }

  "Go situation legal drops" should {
    val game    = Api.position(variant.Go19x19)
    val newGame = game.makeMoves(List(1, 2, 3, 40, 21).map(Api.moveToUci(_, variant.Go19x19)))
    "lots legal drops" in {
      newGame.legalDrops.size must_== 19 * 19 - 5
    }
  }

  "Go Move to Uci " should {
    val moves = List[Int](0, 1, 2, 3, 4, 5, 60, 70, 80, 90, 100, 110)
    "possible moves convert to uci (0->a1 etc)" in {
      moves.map(m => Api.moveToUci(m, variant.Go19x19)) must_== List(
        "S@a1",
        "S@b1",
        "S@c1",
        "S@d1",
        "S@e1",
        "S@f1",
        "S@d4",
        "S@n4",
        "S@e5",
        "S@o5",
        "S@f6",
        "S@p6"
      )
    }
  }

  "Go Uci to Move " should {
    println("test 2")
    val uci = List[String](
      "S@a1",
      "S@b1",
      "S@c1",
      "S@d1",
      "S@e1",
      "S@f1",
      "S@d4",
      "S@n4",
      "S@e5",
      "S@o5",
      "S@f6",
      "S@p6"
    )
    "possible moves convert from uci (a1->0 etc)" in {
      uci.map(m => Api.uciToMove(m, variant.Go19x19)) must_== List[Int](0, 1, 2, 3, 4, 5, 60, 70, 80, 90, 100,
        110)
    }
  }

  "Piece map of Go setup" should {
    val game               = Api.position(variant.Go19x19)
    val pieceMap: PieceMap = game.pieceMap
    "0 starting pieces " in {
      pieceMap.size must_== 0
    }
  }

  "convertPieceMapFromFen" should {
    val game    = Api.position(variant.Go19x19)
    val newGame = game.makeMoves(List(0, 60, 2, 1, 20).map(Api.moveToUci(_, variant.Go19x19)))

    val pieceMap                 = newGame.pieceMap
    // println(pieceMap)
    // println(game.toBoard)
    // println(game.fenString)
    val pieceAtA1: Option[Piece] = pieceMap.get(Pos.A1)
    val pieceAtD4: Option[Piece] = pieceMap.get(Pos.D4)
    val pieceAtB1: Option[Piece] = pieceMap.get(Pos.B1)
    "P1 Stone at pos A1" in {
      pieceAtA1 must_== Some(Piece(P1, Stone))
    }
    "P2 Stone at pos D4" in {
      pieceAtD4 must_== Some(Piece(P2, Stone))
    }
    "No/captured Stones at pos B1" in {
      pieceAtB1 must_== None
    }
    "4 stones on board" in {
      pieceMap.size must_== 4
    }
  }

  "convertPieceMapFromFen" should {
    val game    = Api.position(variant.Go19x19)
    val newGame = game.makeMoves(List(13, 360).map(Api.moveToUci(_, variant.Go19x19)))

    val pieceMap                  = newGame.pieceMap
    // println(pieceMap)
    // println(game.toBoard)
    // println(game.fenString)
    val pieceAtN1: Option[Piece]  = pieceMap.get(Pos.N1)
    val pieceAtS19: Option[Piece] = pieceMap.get(Pos.S19)
    "P1 Stone at pos N1" in {
      pieceAtN1 must_== Some(Piece(P1, Stone))
    }
    "P2 Stone at pos S19" in {
      pieceAtS19 must_== Some(Piece(P2, Stone))
    }
    "2 stones on board" in {
      pieceMap.size must_== 2
    }
  }

  "Piece map of Go game" should {
    val game                     = Api.position(variant.Go19x19)
    val newGame                  = game.makeMoves(List(0, 6, 3, 7, 4).map(Api.moveToUci(_, variant.Go19x19)))
    val position                 = Api.positionFromFen(newGame.fen.value)
    val fen                      = newGame.fen
    val pieceMap: PieceMap       = position.pieceMap
    val pieceAtD1: Option[Piece] = pieceMap.get(Pos.D1)
    "fen after a few moves" in {
      fen.value must_== "19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/S2SS1ss11[SSSSSSSSSSssssssssss] w - 30 85 65 3"
    }
    "P1 Stone at pos D1" in {
      pieceAtD1 must_== Some(Piece(P1, Stone))
    }
    "5 current pieces " in {
      pieceMap.size must_== 5
    }
  }

  "fen after 1 moves of Go game" should {
    val game     = Api.position(variant.Go19x19)
    val newGame1 = game.makeMoves(List(0).map(Api.moveToUci(_, variant.Go19x19)))
    val fen1     = newGame1.fen
    "should be white to play after 1 ply" in {
      fen1.value must_== "19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/S18[SSSSSSSSSSssssssssss] w - 3610 65 65 1"
    }
  }

  "fen after 2 moves of Go game" should {
    val game     = Api.position(variant.Go19x19)
    val newGame2 = game.makeMovesWithPrevious(
      List(10).map(Api.moveToUci(_, variant.Go19x19)),
      List(0).map(m => Api.moveToUci(m, variant.Go19x19))
    )
    val fen2     = newGame2.fen
    "should be black to play after 2 ply" in {
      fen2.value must_== "19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/S9s8[SSSSSSSSSSssssssssss] b - 10 75 65 2"
    }
  }

  "fen after 3 moves of Go game" should {
    val game     = Api.position(variant.Go19x19)
    val newGame3 =
      game.makeMovesWithPrevious(
        List(19).map(Api.moveToUci(_, variant.Go19x19)),
        List(0, 10).map(m => Api.moveToUci(m, variant.Go19x19))
      )
    val fen3     = newGame3.fen
    "should be white to play after 3 ply" in {
      fen3.value must_== "19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/S18/S9s8[SSSSSSSSSSssssssssss] w - 20 75 65 2"
    }
  }

  "fen after 4 moves of Go game" should {
    val game     = Api.position(variant.Go19x19)
    val newGame4 =
      game.makeMovesWithPrevious(
        List(38).map(Api.moveToUci(_, variant.Go19x19)),
        List(0, 10, 19).map(m => Api.moveToUci(m, variant.Go19x19))
      )
    val fen4     = newGame4.fen
    "should be black to play after 4 ply" in {
      fen4.value must_== "19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/s18/S18/S9s8[SSSSSSSSSSssssssssss] b - 20 85 65 3"
    }
  }

  // todo need different example?
  // "go game with a ko point" should {
  //   val game    = Api.position(variant.Go19x19)
  //   val newGame = game.makeMoves(List(2, 59, 20, 39, 22, 41, 40, 21).map(Api.moveToUci(_, variant.Go19x19)))
  //   val fen     = newGame.fenString
  //   // println(newGame.toBoard)
  //   // println(fen)
  //   "show up in fen" in {
  //     fen.split(" ").lift(2) must_== Some(40)
  //   }
  // }

  "go game with a ko point" should {
    val game    = Api.position(variant.Go19x19)
    val newGame = game.makeMoves(List(2, 59, 20, 39, 22, 41, 40, 21).map(Api.moveToUci(_, variant.Go19x19)))
    "not allow move to recapture" in {
      newGame.legalDrops.contains(40) must_== false
    }
  }

  "goBoardFromFen" should {
    val game    = Api.position(variant.Go19x19)
    val newGame = game.makeMoves(List(0, 7, 4, 1).map(Api.moveToUci(_, variant.Go19x19)))
    val fen     = newGame.fen
    val goBoard = Api.goBoardFromFen(fen)
    "fen is 19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/Ss2S2s11[SSSSSSSSSSssssssssss] b - 20 85 65 3" in {
      fen.value must_== "19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/Ss2S2s11[SSSSSSSSSSssssssssss] b - 20 85 65 3"
    }
    "go board diagram matches the fen " in {
      goBoard.toDiagram must_== "19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/XO2X2O11 b -"
    }
  }

  "positionFromVariantAndMoves" should {
    val uciMoves = List("S@a1", "S@e1") // 0,4
    val pos      = Api.positionFromVariantAndMoves(variant.Go19x19, uciMoves)
    val fen      = pos.fen
    "fen is 19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/S3s14[SSSSSSSSSSssssssssss] b - 10 75 65 2" in {
      fen.value must_== "19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/S3s14[SSSSSSSSSSssssssssss] b - 10 75 65 2"
    }
    "360 legal drops" in {
      pos.legalDrops.size must_== 359
    }
  }

  "positionFromVariantAndMoves" should {
    val uciMoves = List("S@a15", "S@e11")
    val pos      = Api.positionFromVariantAndMoves(variant.Go19x19, uciMoves)
    val fen      = pos.fen
    "fen is 19/19/19/19/S18/19/19/19/4s14/19/19/19/19/19/19/19/19/19/19[SSSSSSSSSSssssssssss] b - 10 75 65 2" in {
      fen.value must_== "19/19/19/19/S18/19/19/19/4s14/19/19/19/19/19/19/19/19/19/19[SSSSSSSSSSssssssssss] b - 10 75 65 2"
    }
    "360 legal drops" in {
      pos.legalDrops.size must_== 359
    }
  }

  "positionFromVariantNameAndFEN" should {
    val fen =
      "19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/S3s14[SSSSSSSSSSssssssssss] b - 10 75 65 2"
    val pos = Api.positionFromVariantNameAndFEN("go19x19", fen)
    "fen from new pos" in {
      pos.fen.value must_== fen
    }
  }

  "pieceMapFromFen" should {
    val fen      =
      "19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/S3s14[SSSSSSSSSSssssssssss] b - 10 75 65 2"
    val pieceMap = Api.pieceMapFromFen("go19x19", fen)
    "2 pieces" in {
      pieceMap.size must_== 2
    }
    "pieces are at a1 and e1" in {
      pieceMap.keys.toList.contains(Pos.A1) must_== true
      pieceMap.keys.toList.contains(Pos.E1) must_== true
    }
  }

  "game result " should {
    val fen      =
      "19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/S3s14[SSSSSSSSSSssssssssss] b - 10 75 65 2"
    val position = Api.positionFromFen(fen)
    "is still ongoing during game" in {
      position.gameEnd must_== false
      position.gameOutcome must_== -1000
      position.p1Score must_== 1.0
      position.p2Score must_== 7.5
      position.gameResult must_== GameResult.Ongoing()
    }
  }

  "game result" should {
    val fen      =
      "19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/S3s14[SSSSSSSSSSssssssssss] b - 10 75 65 2"
    val position = Api.positionFromFen(fen)
    position.makeMoves(List("pass", "pass", "ss:"))
    "is not a draw if both players pass early due to komi" in {
      position.gameResult must_!= GameResult.Draw()
    }
  }

  "game result" should {
    val fen      =
      "SSSSSS13/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/S3s14[SSSSSSSSSSssssssssss] b - 70 70 60 2"
    val position = Api.positionFromFen(fen)
    position.makeMoves(List("pass", "pass", "ss:"))
    "is a draw if both players pass in given position" in {
      position.gameResult must_== GameResult.Draw()
      position.gameOutcome must_== 0
    }
  }

  "game result" should {
    val fen      =
      "sssssssssssssssssss/sssssssssssssssssss/19/19/19/19/19/19/19/19/19/19/19/19/19/SSSSSSSSSSSSSSSSSSS/19/SSSSSSSSSSSSSSSSSSS/19[SSSSSSSSSSssssssssss] b - 0 65 65 39"
    val position = Api.positionFromFen(fen)
    val newGame  = position.makeMoves(List("pass", "pass", "ss:"))
    "be a win for player 1 as more territory " in {
      newGame.gameResult must_== GameResult.VariantEnd()
      newGame.gameOutcome must_== 1000
    }

  }

  "game result" should {
    val fen      =
      "19/sssssssssssssssssss/19/sssssssssssssssssss/19/19/19/19/19/19/19/19/19/19/19/19/19/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS[SSSSSSSSSSssssssssss] b - 0 65 65 39"
    val position = Api.positionFromFen(fen)
    val newGame  = position.makeMoves(List("pass", "pass", "ss:"))
    "be a win for player 2 as more territory " in {
      newGame.gameResult must_== GameResult.VariantEnd()
      newGame.gameOutcome must_== -1000
    }

  }

  "game result" should {
    val fen      =
      "2SSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/S1S1S1S1S1S1S1S1S1S/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/S1S1S1S1S1S1S1S1S1S[SSSSSSSSSSssssssssss] w - 3610 65 65 239"
    val position = Api.positionFromFen(fen)
    "be a winning for p1 but ongoing as moves can be played " in {
      position.gameEnd must_== false
      position.gameOutcome must_== 1000
      position.gameResult must_== GameResult.Ongoing()
      position.legalDrops.size must_== 2
      position.p1Score must_== 361.0
      position.p2Score must_== 6.5
    }

  }

  "game result" should {
    val fen      =
      "s1s1s1s1s1s1s1s1s1s/sssssssssssssssssss/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/S1S1S1S1S1S1S1S1S1S/SSSSSSSSSSSSSSSSSSS/S1S1S1S1S1S1S1S1S1S/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/19[SSSSSSSSSSssssssssss] w - 0 65 65 239"
    val position = Api.positionFromFen(fen)
    "be a win as for p1 but ongoing as moves can be played " in {
      position.gameEnd must_== false
      position.gameOutcome must_== 1000
      position.gameResult must_== GameResult.Ongoing()
      position.legalDrops.size must_== 28
      position.p1Score must_== 323.0
      position.p2Score must_== 44.5
    }

  }

  "game result" should {
    val fen      =
      "s1s1s1s1s1s1s1s1s1s/sssssssssssssssssss/1SSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/S1S1S1S1S1S1S1S1S1S/SSSSSSSSSSSSSSSSSSS/S1S1S1S1S1S1S1S1S1S/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/19[SSSSSSSSSSssssssssss] b - 0 65 65 239"
    val position = Api.positionFromFen(fen)
    "not count neutral spaces in scoring " in {
      position.gameEnd must_== false
      position.gameOutcome must_== 1000
      position.gameResult must_== GameResult.Ongoing()
      position.legalDrops.size must_== 38
      position.p1Score must_== 322.0
      position.p2Score must_== 44.5
    }
  }

  // go 9x9 and 13x13 tests for comparison
  "Go situation legal actions" should {
    val game = Api.position(variant.Go9x9)
    "9*9 legal drops and a pass" in {
      game.legalActions.size must_== 9 * 9 + 1 // 0 to 80
    }
  }

  "Go initial fen" should {
    val fen9  = variant.Go9x9.initialFen.value
    val fen13 = variant.Go13x13.initialFen.value
    "be valid for 9x9" in {
      Api.validateFEN(fen9) must_== true
    }
    "be valid for 13x13" in {
      Api.validateFEN(fen13) must_== true
    }
  }

  "Piece map of 9x9 Go game" should {
    val game                     = Api.position(variant.Go9x9)
    val newGame                  = game.makeMoves(List(0, 16, 30, 59, 4).map(Api.moveToUci(_, variant.Go9x9)))
    val position                 = Api.positionFromFen(newGame.fen.value)
    val fen                      = newGame.fen
    val pieceMap: PieceMap       = position.pieceMap
    val pieceAtE1: Option[Piece] = pieceMap.get(Pos.E1)
    val pieceAtF7: Option[Piece] = pieceMap.get(Pos.F7) // 59
    "fen after a few moves" in {
      fen.value must_== "9/9/5s3/9/9/3S5/9/7s1/S3S4[SSSSSSSSSSssssssssss] w - 30 85 65 3"
    }
    "P1 Stone at pos E1" in {
      pieceAtE1 must_== Some(Piece(P1, Stone))
    }
    "P2 Stone at pos F7" in {
      pieceAtF7 must_== Some(Piece(P2, Stone))
    }
    "5 current pieces " in {
      pieceMap.size must_== 5
    }
  }

  "Api.positonFromFen for 13x13" should {
    val game           = Api.position(variant.Go13x13)
    val newGame        = game.makeMoves(List(4, 16, 30, 59, 140).map(Api.moveToUci(_, variant.Go13x13)))
    val fen            = newGame.fen
    val createPosition = Api.positionFromFen(newGame.fen.value)
    val createdFen     = createPosition.fen
    "have a matching fen to original game" in {
      fen must_== createdFen
    }
  }

  "Api.positonFromFen for 19x19" should {
    val game           = Api.position(variant.Go19x19)
    val newGame        = game.makeMoves(List(4, 16, 30, 59, 140).map(Api.moveToUci(_, variant.Go19x19)))
    val fen            = newGame.fen
    val createPosition = Api.positionFromFen(newGame.fen.value)
    val createdFen     = createPosition.fen

    "have a matching fen to original game" in {
      fen must_== createdFen
    }
  }

  "Api.positonFromFen for 9x9" should {
    val game           = Api.position(variant.Go9x9)
    val newGame        = game.makeMoves(List(4, 16, 30, 59, 70).map(Api.moveToUci(_, variant.Go9x9)))
    val fen            = newGame.fen
    val createPosition = Api.positionFromFen(newGame.fen.value)
    val createdFen     = createPosition.fen
    "have a matching fen to original game" in {
      fen must_== createdFen
    }
  }

  "Piece map of 13x13 Go game" should {
    val game                              = Api.position(variant.Go13x13)
    val newGame                           = game.makeMoves(List(4, 16, 30, 59, 140).map(Api.moveToUci(_, variant.Go13x13)))
    val fen                               = newGame.fen
    val pieceMap                          = newGame.pieceMap
    val createPosition                    = Api.positionFromFen(newGame.fen.value)
    val pieceMapFromPositionApi: PieceMap = createPosition.pieceMap

    val pieceAtE1: Option[Piece]  = pieceMap.get(Pos.E1)  // 4
    val pieceAtH5: Option[Piece]  = pieceMap.get(Pos.H5)  // 59
    val pieceAtD2: Option[Piece]  = pieceMap.get(Pos.D2)  // 16
    val pieceAtE3: Option[Piece]  = pieceMap.get(Pos.E3)  // 30
    val pieceAtK11: Option[Piece] = pieceMap.get(Pos.K11) // 140
    "fen after a few moves" in {
      fen.value must_== "13/13/10S2/13/13/13/13/13/7s5/13/4S8/3s9/4S8[SSSSSSSSSSssssssssss] w - 30 85 65 3"
    }
    "P1 Stone at pos E1" in {
      pieceAtE1 must_== Some(Piece(P1, Stone))
    }
    "P2 Stone at pos H5" in {
      pieceAtH5 must_== Some(Piece(P2, Stone))
    }
    "P2 Stone at pos D2" in {
      pieceAtD2 must_== Some(Piece(P2, Stone))
    }
    "P1 Stone at pos E3" in {
      pieceAtE3 must_== Some(Piece(P1, Stone))
    }
    "P1 Stone at pos K11" in {
      pieceAtK11 must_== Some(Piece(P1, Stone))
    }
    "5 current pieces " in {
      pieceMap.size must_== 5
    }
    "piece map must be same from game and new creation of that game" in {
      pieceMap must_== pieceMapFromPositionApi
    }
  }

  "position from variant and fen" should {
    val fen      = "9/9/2S3S2/9/9/9/9/9/9[SSSSSSSSSSssssssssss] w - 810 45 45 1"
    val position = Api.positionFromVariantNameAndFEN(variant.Go9x9.key, fen)
    "start a game from the given position" in {
      position.playerTurn must_== -1
    }
    "have a the same fen value" in {
      position.fen.value must_== fen
    }
    "have the piecemap of size 2" in {
      position.pieceMap.size must_== 2
    }

    val p2 = position.makeMovesWithPrevious(List(3).map(Api.moveToUci(_, variant.Go9x9)), List())
    "new position has 3 pieces" in {
      p2.pieceMap.size must_== 3
    }
    "new position has fen of 3 pieces" in {
      p2.fen.value must_== "9/9/2S3S2/9/9/9/9/9/3s5[SSSSSSSSSSssssssssss] b - 20 55 45 1"
    }

  }

  "convert board string from piecemap" should {
    val fen         = "13/13/10S2/13/13/13/13/13/7s5/13/4S8/3s9/4S8[SSSSSSSSSSssssssssss] w - 30 85 65 3"
    val pieceMap    = Api.pieceMapFromFen(variant.Go13x13.key, fen)
    val boardString = Api.writeBoardFenFromPieceMap(pieceMap, variant.Go13x13)

    "board string should match initial part of fen" in {
      boardString must_== strategygames.go.format.FEN(fen).board
    }

  }

  "removing deadstones from fen" should {
    val fen        = "13/13/10S2/13/13/13/13/13/7s5/13/4S8/3s9/4S8[SSSSSSSSSSssssssssss] w - 30 85 65 3"
    val deadStones = List(Pos.E1, Pos.H5)
    val newFen     = Api.removeDeadStones(deadStones, fen, variant.Go13x13)
    "have new fen with fewer stones" in {
      newFen must_== "13/13/10S2/13/13/13/13/13/13/13/4S8/3s9/13[SSSSSSSSSSssssssssss] w - 30 85 65 3"
    }
  }

}
