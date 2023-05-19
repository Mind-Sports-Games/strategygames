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

    val fen2 = "19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/1SSss14[SSSSSSSSSSssssssssss] b - 0 0 1"
    "true due to allowing >1 for multiple empty spaces" in {
      Api.validateFEN(fen2) must_== true
    }
    // println(fen2.matches(Api.fenRegex))

    val fen4 =
      "19/19/19/19/19/19/19/19/19/19/19/19/SsSsSsSsSsSsSsSsSsSs/19/19/19/19/19/19[SSSSSSSSSSssssssssss] w - 0 0 1"
    // println(fen4.matches(Api.fenRegex))
    "false due to more stone in row than width" in {
      Api.validateFEN(fen4) must_== false
    }

    val game = Api.position
    "match that of the initial game board fen" in {
      game.fen.value.split(" ").take(3) must_== fen.split(" ").take(3)
    }
  }

  "Go situation legal moves" should {
    val game = Api.position
    "19*19 legal moves and a pass" in {
      game.legalMoves.size must_== 19 * 19 // 0 to 360
    }
  }

  "Go situation legal moves" should {
    val game    = Api.position
    val newGame = game.makeMoves(List(1, 2, 3, 40, 21))
    "lots legal moves" in {
      newGame.legalMoves.size must_== 19 * 19 - 5
    }
  }

  "Go Move to Uci " should {
    val moves = List[Int](0, 1, 2, 3, 4, 5, 60, 70, 80, 90, 100, 110)
    "possible moves convert to uci (0->a1 etc)" in {
      moves.map(m => Api.moveToUci(m)) must_== List(
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
      uci.map(m => Api.uciToMove(m)) must_== List[Int](0, 1, 2, 3, 4, 5, 60, 70, 80, 90, 100, 110)
    }
  }

  "Piece map of Go setup" should {
    val game               = Api.position
    val pieceMap: PieceMap = game.pieceMap
    "0 starting pieces " in {
      pieceMap.size must_== 0
    }
  }

  "convertPieceMapFromFen" should {
    val game    = Api.position
    val newGame = game.makeMoves(List(0, 60, 2, 1, 20))

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
    val game    = Api.position
    val newGame = game.makeMoves(List(13, 360))

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
    val game                     = Api.position
    val newGame                  = game.makeMoves(List(0, 6, 3, 7, 4))
    val position                 = Api.positionFromFen(newGame.fen.value)
    val fen                      = newGame.fen
    val pieceMap: PieceMap       = position.pieceMap
    val pieceAtD1: Option[Piece] = pieceMap.get(Pos.D1)
    "fen after a few moves" in {
      fen.value must_== "19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/S2SS1ss11[SSSSSSSSSSssssssssss] w - 0 6 3"
    }
    "P1 Stone at pos D1" in {
      pieceAtD1 must_== Some(Piece(P1, Stone))
    }
    "5 current pieces " in {
      pieceMap.size must_== 5
    }
  }

  "fen after 1 moves of Go game" should {
    val game     = Api.position
    val newGame1 = game.makeMoves(List(0))
    val fen1     = newGame1.fen
    "should be white to play after 1 ply" in {
      fen1.value must_== "19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/S18[SSSSSSSSSSssssssssss] w - 0 6 1"
    }
  }

  "fen after 2 moves of Go game" should {
    val game     = Api.position
    val newGame2 = game.makeMovesWithPrevious(List(10), List(0).map(Api.moveToUci))
    val fen2     = newGame2.fen
    "should be black to play after 2 ply" in {
      fen2.value must_== "19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/S9s8[SSSSSSSSSSssssssssss] b - 0 6 2"
    }
  }

  "fen after 3 moves of Go game" should {
    val game     = Api.position
    val newGame3 = game.makeMovesWithPrevious(List(19), List(0, 10).map(Api.moveToUci))
    val fen3     = newGame3.fen
    "should be white to play after 3 ply" in {
      fen3.value must_== "19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/S18/S9s8[SSSSSSSSSSssssssssss] w - 0 6 2"
    }
  }

  "fen after 4 moves of Go game" should {
    val game     = Api.position
    val newGame4 = game.makeMovesWithPrevious(List(38), List(0, 10, 19).map(Api.moveToUci))
    val fen4     = newGame4.fen
    "should be black to play after 4 ply" in {
      fen4.value must_== "19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/s18/S18/S9s8[SSSSSSSSSSssssssssss] b - 0 6 3"
    }
  }

  // todo need different example?
  // "go game with a ko point" should {
  //   val game    = Api.position
  //   val newGame = game.makeMoves(List(2, 59, 20, 39, 22, 41, 40, 21))
  //   val fen     = newGame.fenString
  //   // println(newGame.toBoard)
  //   // println(fen)
  //   "show up in fen" in {
  //     fen.split(" ").lift(2) must_== Some(40)
  //   }
  // }

  "go game with a ko point" should {
    val game    = Api.position
    val newGame = game.makeMoves(List(2, 59, 20, 39, 22, 41, 40, 21))
    "not allow move to recapture" in {
      newGame.legalMoves.contains(40) must_== false
    }
  }

  "goBoardFromFen" should {
    val game    = Api.position
    val newGame = game.makeMoves(List(0, 7, 4, 1))
    val fen     = newGame.fen
    val goBoard = Api.goBoardFromFen(fen.value)
    "fen is 19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/Ss2S2s11[SSSSSSSSSSssssssssss] b - 0 6 3" in {
      fen.value must_== "19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/Ss2S2s11[SSSSSSSSSSssssssssss] b - 0 6 3"
    }
    "go board diagram matches the fen " in {
      goBoard.toDiagram must_== fen.engineFen
    }
  }

  "positionFromVariantAndMoves" should {
    val uciMoves = List("S@a1", "S@e1") // 0,4
    val pos      = Api.positionFromVariantAndMoves(variant.Go19x19, uciMoves)
    val fen      = pos.fen
    "fen is 19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/S3s14[SSSSSSSSSSssssssssss] b - 0 6 2" in {
      fen.value must_== "19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/S3s14[SSSSSSSSSSssssssssss] b - 0 6 2"
    }
    "360 legal moves" in {
      pos.legalMoves.size must_== 359
    }
  }

  "positionFromVariantAndMoves" should {
    val uciMoves = List("S@a15", "S@e11")
    val pos      = Api.positionFromVariantAndMoves(variant.Go19x19, uciMoves)
    val fen      = pos.fen
    "fen is 19/19/19/19/S18/19/19/19/4s14/19/19/19/19/19/19/19/19/19/19[SSSSSSSSSSssssssssss] b - 0 6 2" in {
      fen.value must_== "19/19/19/19/S18/19/19/19/4s14/19/19/19/19/19/19/19/19/19/19[SSSSSSSSSSssssssssss] b - 0 6 2"
    }
    "360 legal moves" in {
      pos.legalMoves.size must_== 359
    }
  }

  "positionFromVariantNameAndFEN" should {
    val fen = "19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/S3s14[SSSSSSSSSSssssssssss] b - 0 6 2"
    val pos = Api.positionFromVariantNameAndFEN("go19x19", fen)
    "fen from new pos" in {
      pos.fen.value must_== fen
    }
  }

  "pieceMapFromFen" should {
    val fen      = "19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/S3s14[SSSSSSSSSSssssssssss] b - 0 6 2"
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
    val fen      = "19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/S3s14[SSSSSSSSSSssssssssss] b - 0 6 2"
    val position = Api.positionFromFen(fen)
    "is still ongoing during game" in {
      position.gameResult must_== GameResult.Ongoing()
    }
  }

  "game result" should {
    val fen      = "19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/S3s14[SSSSSSSSSSssssssssss] b - 0 6 2"
    val position = Api.positionFromFen(fen)
    position.makeMoves(List(361, 361))
    "is draw if both players pass early" in {
      position.gameResult must_== GameResult.Draw()
    }
  }

  "game result" should {
    val fen      =
      "sssssssssssssssssss/sssssssssssssssssss/19/19/19/19/19/19/19/19/19/19/19/19/19/SSSSSSSSSSSSSSSSSSS/19/SSSSSSSSSSSSSSSSSSS/19[SSSSSSSSSSssssssssss] b - 0 6 39"
    val position = Api.positionFromFen(fen)
    val newGame  = position.makeMoves(List(361, 361))
    "be a win for player 1 as more territory " in {
      newGame.gameResult must_== GameResult.VariantEnd()
      newGame.gameOutcome must_== 1000
    }

  }

  "game result" should {
    val fen      =
      "19/sssssssssssssssssss/19/sssssssssssssssssss/19/19/19/19/19/19/19/19/19/19/19/19/19/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS[SSSSSSSSSSssssssssss] b - 0 6 39"
    val position = Api.positionFromFen(fen)
    val newGame  = position.makeMoves(List(361, 361))
    "be a win for player 2 as more territory " in {
      newGame.gameResult must_== GameResult.VariantEnd()
      newGame.gameOutcome must_== -1000
    }

  }

  "game result" should {
    val fen      =
      "2SSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/S1S1S1S1S1S1S1S1S1S/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/S1S1S1S1S1S1S1S1S1S[SSSSSSSSSSssssssssss] w - 0 6 239"
    val position = Api.positionFromFen(fen)
    "be a win as its clearly over but moves can be played " in {
      position.gameEnd must_== false
      position.gameOutcome must_== 1000
      position.gameResult must_== GameResult.VariantEnd()
      position.legalMoves.size must_== 2
      position.gameScore must_== 361
    }

  }

}
