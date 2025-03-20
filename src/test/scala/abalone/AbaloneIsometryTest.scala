package strategygames.abalone

import strategygames.format.{FEN => StratFen, Forsyth => StratForsyth, Uci => StratUci}
import strategygames.variant.{Variant => StratVariant}

class AbaloneIsometryTest extends strategygames.chess.ChessTest with IAbaloneTest {
  val gameFamily   = variant.Abalone.gameFamily
  val lib          = gameFamily.gameLogic
  val stratVariant = StratVariant(lib, variant.Abalone.key).get

  /*
   *     · · · · ·
   *    · · · · · ·
   *   · · · · · · ·
   *  · · · · · · 1 ·
   * · · · · · · 1 1 ·
   *  · · · · · · · ·
   *   · · · · · · ·
   *    · · · · · ·
   *     · · · · ·
   */
  "downRight side move of 2 marbles moving right" in {
    _testEveryMoveLoadFenIsometry(
      lib,
      StratFen(lib, format.FEN("5/6/7/8/6SS1/6S1/7/6/5 0 0 b 0 0").value),
      stratVariant
    )(
      List(
        "f8e9"
      ).map(uciStr => StratUci(lib, gameFamily, uciStr).get)
    ) must beValid.like(gameData => {
      val fen1 = StratForsyth.>>(lib, gameData.game)
      val fen2 = StratForsyth.>>(lib, gameData.fenGame)
      fen1 must_== fen2
      fen1.value must_== "5/6/7/8/6S1S/7S/7/6/5 0 0 w 1 1"
      /*
       *     · · · · ·
       *    · · · · · ·
       *   · · · · · · ·
       *  · · · · · · - 1
       * · · · · · · 1 - 1
       *  · · · · · · · ·
       *   · · · · · · ·
       *    · · · · · ·
       *     · · · · ·
       */
    })
  }

  /*
   *     · · · · ·
   *    · · · · · ·
   *   · · · · · · ·
   *  · · · · · · 1 ·
   * · · · · · 1 1 · ·
   *  · · · · · · · ·
   *   · · · · · · ·
   *    · · · · · ·
   *     · · · · ·
   */
  "upLeft side move of 2 marbles moving upLeft" in {
    _testEveryMoveLoadFenIsometry(
      lib,
      StratFen(lib, format.FEN("5/6/7/8/5SS2/6S1/7/6/5 0 0 b 0 0").value),
      stratVariant
    )(
      List(
        "e7f6"
      ).map(uciStr => StratUci(lib, gameFamily, uciStr).get)
    ) must beValid.like(gameData => {
      val fen1 = StratForsyth.>>(lib, gameData.game)
      val fen2 = StratForsyth.>>(lib, gameData.fenGame)
      fen1 must_== fen2
      fen1.value must_== "5/6/7/8/9/4SSS1/7/6/5 0 0 w 1 1"
      /*
       *     · · · · ·
       *    · · · · · ·
       *   · · · · · · ·
       *  · · · · 1 1 1 ·
       * · · · · · - - · ·
       *  · · · · · · · ·
       *   · · · · · · ·
       *    · · · · · ·
       *     · · · · ·
       */
    })
  }

  /*
   *     · · · · ·
   *    · · · · · ·
   *   2 2 · · · · ·
   *  1 1 1 · · · · ·
   * · · · · · · · · ·
   *  · · · · · · · ·
   *   · · · · · · ·
   *    · · · · · ·
   *     · · · · ·
   */
  "a few downRight side moves for both players" in {
    _testEveryMoveLoadFenIsometry(
      lib,
      StratFen(lib, format.FEN("5/6/7/8/9/SSS5/ss5/6/5 0 0 b 0 0").value),
      stratVariant
    )(
      List(
        "f2e4",
        "g3f4",
        "e2d4",
        "f3e4",
        "d2c4",
        "e3d4",
        "c2b4",
        "d3c4",
        "b2a4",
        "c3b4"
      ).map(uciStr => StratUci(lib, gameFamily, uciStr).get)
    ) must beValid.like(gameData => {
      val fen1 = StratForsyth.>>(lib, gameData.game)
      val fen2 = StratForsyth.>>(lib, gameData.fenGame)
      println(fen1)
      fen1 must_== fen2
      fen1.value must_== "1SSS1/2ss2/7/8/9/8/7/6/5 0 0 b 10 6"
      /*
       *     · · · · ·
       *    · · · · · ·
       *   · · · · · · ·
       *  · · · · · · · ·
       * · · · · · · · · ·
       *  · · · · · · · ·
       *   · · · · · · ·
       *    · · 2 2 · ·
       *     · 1 1 1 ·
       */
    })
  }
//
//  "downleft side move of 2 with 1 possible path" in {
//    /*
//            _ _ _ _ _
//           _ _ _ _ _ _
//          _ _ _ _ _ _ _
//         _ _ _ _ _ _ s _
//        _ _ _ _ _ _ s s _
//         _ _ _ _ _ _ _ _
//          _ _ _ _ _ _ _
//           _ _ _ _ _ _
//            _ _ _ _ _
//     */
//    _testEveryMoveLoadFenIsometry(
//      lib,
//      StratFen(lib, format.FEN("5/6/7/6S1/6SS1/8/7/6/5 0 0 b 0 0").value),
//      stratVariant
//    )(
//      List(
//        "h6f5"
//      ).map(uciStr => StratUci(lib, gameFamily, uciStr).get)
//    ) must beValid.like(gameData => {
//      val fen1 = StratForsyth.>>(lib, gameData.game)
//      val fen2 = StratForsyth.>>(lib, gameData.fenGame)
//      fen1 must_== fen2
//      fen1.value must_== "5/6/7/5S2/5S1S1/8/7/6/5 0 0 w 1 1"
//      /*
//            _ _ _ _ _
//           _ _ _ _ _ _
//          _ _ _ _ _ _ _
//         _ _ _ _ _ s _ _
//        _ _ _ _ _ s _ s _
//         _ _ _ _ _ _ _ _
//          _ _ _ _ _ _ _
//           _ _ _ _ _ _
//            _ _ _ _ _
//       */
//    })
//  }
//
//  "downleft side move with 2 paths - case 1 - lineDir being left" in {
//    /*
//            _ _ _ _ _
//           _ _ _ _ _ _
//          _ _ _ _ _ _ _
//         _ _ _ _ _ s s _
//        _ _ _ _ _ _ _ s _
//         _ _ _ _ _ _ _ _
//          _ _ _ _ _ _ _
//           _ _ _ _ _ _
//            _ _ _ _ _
//     */
//    _testEveryMoveLoadFenIsometry(
//      lib,
//      StratFen(lib, format.FEN("5/6/7/5SS1/7S1/8/7/6/5 0 0 b 0 0").value),
//      stratVariant
//    )(
//      List(
//        "h6f5"
//      ).map(uciStr => StratUci(lib, gameFamily, uciStr).get)
//    ) must beValid.like(gameData => {
//      val fen1 = StratForsyth.>>(lib, gameData.game)
//      val fen2 = StratForsyth.>>(lib, gameData.fenGame)
//      fen1 must_== fen2
//      fen1.value must_== "5/6/7/8/5SSS1/8/7/6/5 0 0 w 1 1"
//      /*
//            _ _ _ _ _
//           _ _ _ _ _ _
//          _ _ _ _ _ _ _
//         _ _ _ _ _ _ _ _
//        _ _ _ _ _ s s s _
//         _ _ _ _ _ _ _ _
//          _ _ _ _ _ _ _
//           _ _ _ _ _ _
//            _ _ _ _ _
//       */
//    })
//  }
//
//  "downleft side move with 2 paths - case 2 - lineDir being downRight" in {
//    /*
//            _ _ _ _ _
//           _ _ _ _ _ _
//          _ _ _ _ _ _ _
//         _ _ _ _ _ s s _
//        _ _ _ _ _ _ _ s _
//         _ _ _ _ _ _ _ _
//          _ _ _ _ _ _ _
//           _ _ _ _ _ _
//            _ _ _ _ _
//     */
//    _testEveryMoveLoadFenIsometry(
//      lib,
//      StratFen(lib, format.FEN("5/6/7/5SS1/7S1/8/7/6/5 0 0 b 0 0").value),
//      stratVariant
//    )(
//      List(
//        "h6g4"
//      ).map(uciStr => StratUci(lib, gameFamily, uciStr).get)
//    ) must beValid.like(gameData => {
//      val fen1 = StratForsyth.>>(lib, gameData.game)
//      val fen2 = StratForsyth.>>(lib, gameData.fenGame)
//      fen1 must_== fen2
//      fen1.value must_== "5/6/7/5S2/6S2/6S1/7/6/5 0 0 w 1 1"
//      /*
//            _ _ _ _ _
//           _ _ _ _ _ _
//          _ _ _ _ _ _ _
//         _ _ _ _ _ s _ _
//        _ _ _ _ _ _ s _ _
//         _ _ _ _ _ _ s _
//          _ _ _ _ _ _ _
//           _ _ _ _ _ _
//            _ _ _ _ _
//       */
//    })
//  }
//
//  "downleft side move with 2 paths - case 1 - lineDir being downLeft and marbles moving left" in {
//    /*
//            _ _ _ _ _
//           _ _ _ _ _ _
//          _ _ _ _ _ _ _
//         _ _ _ _ _ s _ _
//        _ _ _ _ _ s _ _ _
//         _ _ _ _ s _ _ _
//          _ _ _ _ _ _ _
//           _ _ _ _ _ _
//            _ _ _ _ _
//     */
//    _testEveryMoveLoadFenIsometry(
//      lib,
//      StratFen(lib, format.FEN("5/6/7/5S2/5S3/4S3/7/6/5 0 0 b 0 0").value),
//      stratVariant
//    )(
//      List(
//        "g6d4"
//      ).map(uciStr => StratUci(lib, gameFamily, uciStr).get)
//    ) must beValid.like(gameData => {
//      val fen1 = StratForsyth.>>(lib, gameData.game)
//      val fen2 = StratForsyth.>>(lib, gameData.fenGame)
//      fen1 must_== fen2
//      fen1.value must_== "5/6/7/4S3/4S4/3S4/7/6/5 0 0 w 1 1"
//      /*
//            _ _ _ _ _
//           _ _ _ _ _ _
//          _ _ _ _ _ _ _
//         _ _ _ _ s _ _ _
//        _ _ _ _ s _ _ _ _
//         _ _ _ s _ _ _ _
//          _ _ _ _ _ _ _
//           _ _ _ _ _ _
//            _ _ _ _ _
//       */
//    })
//  }
//
//  "downleft side move with 2 paths - case 2 - lineDir being downLeft and marbles moving downRight" in {
//    /*
//            _ _ _ _ _
//           _ _ _ _ _ _
//          _ _ _ _ _ _ _
//         _ _ _ _ _ s _ _
//        _ _ _ _ _ s _ _ _
//         _ _ _ _ s _ _ _
//          _ _ _ _ _ _ _
//           _ _ _ _ _ _
//            _ _ _ _ _
//     */
//    _testEveryMoveLoadFenIsometry(
//      lib,
//      StratFen(lib, format.FEN("5/6/7/5S2/5S3/4S3/7/6/5 0 0 b 0 0").value),
//      stratVariant
//    )(
//      List(
//        "g6e3"
//      ).map(uciStr => StratUci(lib, gameFamily, uciStr).get)
//    ) must beValid.like(gameData => {
//      val fen1 = StratForsyth.>>(lib, gameData.game)
//      val fen2 = StratForsyth.>>(lib, gameData.fenGame)
//      fen1 must_== fen2
//      fen1.value must_== "5/6/7/8/6S2/5S2/4S2/6/5 0 0 w 1 1"
//      /*
//            _ _ _ _ _
//           _ _ _ _ _ _
//          _ _ _ _ _ _ _
//         _ _ _ _ _ _ _ _
//        _ _ _ _ _ _ s _ _
//         _ _ _ _ _ s _ _
//          _ _ _ _ s _ _
//           _ _ _ _ _ _
//            _ _ _ _ _
//       */
//    })
//  }
//
//  "upRight side move with 2 paths - case 1 - lineDir being upRight and marbles moving right" in {
//    /*
//            _ _ _ _ _
//           _ _ _ _ _ _
//          _ _ _ _ _ _ _
//         _ _ _ _ _ s _ _
//        _ _ _ _ _ s _ _ _
//         _ _ _ _ s _ _ _
//          _ _ _ _ _ _ _
//           _ _ _ _ _ _
//            _ _ _ _ _
//     */
//    _testEveryMoveLoadFenIsometry(
//      lib,
//      StratFen(lib, format.FEN("5/6/7/5S2/5S3/4S3/7/6/5 0 0 b 0 0").value),
//      stratVariant
//    )(
//      List(
//        "e4h6"
//      ).map(uciStr => StratUci(lib, gameFamily, uciStr).get)
//    ) must beValid.like(gameData => {
//      val fen1 = StratForsyth.>>(lib, gameData.game)
//      val fen2 = StratForsyth.>>(lib, gameData.fenGame)
//      fen1 must_== fen2
//      fen1.value must_== "5/6/7/6S1/6S2/5S2/7/6/5 0 0 w 1 1"
//      /*
//            _ _ _ _ _
//           _ _ _ _ _ _
//          _ _ _ _ _ _ _
//         _ _ _ _ _ _ s _
//        _ _ _ _ _ _ s _ _
//         _ _ _ _ _ s _ _
//          _ _ _ _ _ _ _
//           _ _ _ _ _ _
//            _ _ _ _ _
//       */
//    })
//  }
//
//  "upRight side move with 2 paths - case 2 - lineDir being upRight and marbles moving upLeft" in {
//    /*
//            _ _ _ _ _
//           _ _ _ _ _ _
//          _ _ _ _ _ _ _
//         _ _ _ _ _ s _ _
//        _ _ _ _ _ s _ _ _
//         _ _ _ _ s _ _ _
//          _ _ _ _ _ _ _
//           _ _ _ _ _ _
//            _ _ _ _ _
//     */
//    _testEveryMoveLoadFenIsometry(
//      lib,
//      StratFen(lib, format.FEN("5/6/7/5S2/5S3/4S3/7/6/5 0 0 b 0 0").value),
//      stratVariant
//    )(
//      List(
//        "e4g7"
//      ).map(uciStr => StratUci(lib, gameFamily, uciStr).get)
//    ) must beValid.like(gameData => {
//      val fen1 = StratForsyth.>>(lib, gameData.game)
//      val fen2 = StratForsyth.>>(lib, gameData.fenGame)
//      fen1 must_== fen2
//      fen1.value must_== "5/6/4S2/4S3/4S4/8/7/6/5 0 0 w 1 1"
//      /*
//            _ _ _ _ _
//           _ _ _ _ _ _
//          _ _ _ _ s _ _
//         _ _ _ _ s _ _ _
//        _ _ _ _ s _ _ _ _
//         _ _ _ _ _ _ _ _
//          _ _ _ _ _ _ _
//           _ _ _ _ _ _
//            _ _ _ _ _
//       */
//    })
//  }
//
//  "downLeft and upRight side moves with several paths" in {
//    /*
//            _ _ _ _ _
//           _ _ 1 _ _ _
//          _ _ _ 1 _ _ _
//         _ _ _ _ 1 1 1 _
//        _ _ _ _ _ _ _ _ _
//         _ _ _ _ _ _ _ _
//          _ _ 2 2 _ _ _
//           _ _ _ 2 _ _
//            _ _ _ _ _
//     */
//    _testEveryMoveLoadFenIsometry(
//      lib,
//      StratFen(lib, format.FEN("5/2S3/3S3/4SSS1/9/8/2ss3/3s2/5 0 0 b 0 0").value),
//      stratVariant
//    )(
//      List(
//        "f6i7",
//        "d3c1", // 1
//        "i7f6",
//        "c1d3", // 2
//        "f6g9",
//        "d3b2"
//      ).map(uciStr => StratUci(lib, gameFamily, uciStr).get)
//    ) must beValid.like(gameData => {
//      val fen1 = StratForsyth.>>(lib, gameData.game)
//      val fen2 = StratForsyth.>>(lib, gameData.fenGame)
//      fen1 must_== fen2
//      // fen1.value must_== "5/2s3/3ssss/8/9/8/2S4/2S3/2S2 0 0 b 2 2" // 1
//      // fen1.value must_== "5/2s3/3s3/4sss1/9/8/2SS3/3S2/5 0 0 b 4 3" // 2
//      fen1.value must_== "2S2/3S2/4S2/5SS1/9/8/7/1sss2/5 0 0 b 6 4"
//      /*
//            _ _ 1 _ _
//           _ _ _ 1 _ _
//          _ _ _ _ 1 _ _
//         _ _ _ _ _ 1 1 _
//        _ _ _ _ _ _ _ _ _
//         _ _ _ _ _ _ _ _
//          _ _ _ _ _ _ _
//           _ S S S _ _
//            _ _ _ _ _
//       */
//    })
//  }
//
//  "pushing 2v1 upRight to the edge then 3v1 left to another edge" in {
//    /*
//            _ 0 * * _
//           _ _ _ _ * _
//          _ _ _ _ 0 _ _
//         _ _ _ _ 0 _ _ _
//        _ _ _ _ _ _ _ _ _
//         _ _ _ _ _ _ _ _
//          _ _ _ _ _ _ _
//           _ _ _ _ _ _
//            _ _ _ _ _
//     */
//    _testEveryMoveLoadFenIsometry(
//      lib,
//      StratFen(lib, format.FEN("1Sss1/4s1/4S2/4S3/9/8/7/6/5 0 0 b 0 0").value),
//      stratVariant
//    )(
//      List(
//        "f6h8",
//        "i9f9"
//      ).map(uciStr => StratUci(lib, gameFamily, uciStr).get)
//    ) must beValid.like(gameData => {
//      val fen1 = StratForsyth.>>(lib, gameData.game)
//      val fen2 = StratForsyth.>>(lib, gameData.fenGame)
//      fen1 must_== fen2
//      fen1.value must_== "Ssss1/4S1/4S2/8/9/8/7/6/5 0 0 b 2 2"
//
//    })
//  }
//
//  "pushing 3v2 to have a 3v3" in {
//    /*
//            _ _ _ _ _
//           _ _ _ _ _ _
//          _ _ _ _ _ _ _
//         _ _ _ _ _ _ _ _
//     * _ * * 0 0 0 _ _
//         _ _ _ _ _ _ _ _
//          _ _ _ _ _ _ _
//           _ _ _ _ _ _
//            _ _ _ _ _
//     */
//    _testEveryMoveLoadFenIsometry(
//      lib,
//      StratFen(lib, format.FEN("5/6/7/8/s1ssSSS2/8/7/6/5 0 0 b 0 0").value),
//      stratVariant
//    )(
//      List(
//        "g5d5"
//      ).map(uciStr => StratUci(lib, gameFamily, uciStr).get)
//    ) must beValid.like(gameData => {
//      val fen1 = StratForsyth.>>(lib, gameData.game)
//      val fen2 = StratForsyth.>>(lib, gameData.fenGame)
//      fen1 must_== fen2
//      fen1.value must_== "5/6/7/8/sssSSS3/8/7/6/5 0 0 w 1 1"
//
//    })
//  }
//
//  "pushing out 3v2, 3v1, 2v1" in {
//    /*
//            _ _ _ _ _
//           _ _ _ _ _ _
//          _ _ _ _ _ _ _
//         _ _ _ _ _ _ _ _
//     * * 0 0 0 _ _ _ _
//         _ _ _ _ _ _ _ _
//          _ _ _ _ _ _ _
//           _ _ _ _ _ _
//     * * * * 0
//     */
//    _testEveryMoveLoadFenIsometry(
//      lib,
//      StratFen(lib, format.FEN("5/6/7/8/ssSSS4/8/7/6/ssssS 0 0 b 0 0").value),
//      stratVariant
//    )(
//      List(
//        "e5b5",
//        "b1e1",
//        "c5a5"
//      ).map(uciStr => StratUci(lib, gameFamily, uciStr).get)
//    ) must beValid.like(gameData => {
//      val fen1 = StratForsyth.>>(lib, gameData.game)
//      val fen2 = StratForsyth.>>(lib, gameData.fenGame)
//      fen1 must_== fen2
//      fen1.value must_== "5/6/7/8/SS1S5/8/7/6/s1sss 2 1 w 0 2"
//
//    })
//  }
//
//  "pushing 2v1 then 3v2" in {
//    /*
//            _ _ _ _ _
//           _ _ _ _ _ _
//          _ _ _ _ _ _ _
//         _ _ _ _ _ _ _ _
//     * * _ * 0 0 _ _ _
//         _ _ _ _ _ _ _ _
//          _ _ _ _ _ _ _
//           _ _ _ _ _ _
//            _ _ _ _ _
//     */
//    _testEveryMoveLoadFenIsometry(
//      lib,
//      StratFen(lib, format.FEN("5/6/7/8/ss1sSS3/8/7/6/5 0 0 b 0 0").value),
//      stratVariant
//    )(
//      List(
//        "f5d5",
//        "a5d5"
//      ).map(uciStr => StratUci(lib, gameFamily, uciStr).get)
//    ) must beValid.like(gameData => {
//      val fen1 = StratForsyth.>>(lib, gameData.game)
//      val fen2 = StratForsyth.>>(lib, gameData.fenGame)
//      fen1 must_== fen2
//      fen1.value must_== "5/6/7/8/1sssSS3/8/7/6/5 0 0 b 2 2"
//
//    })
//  }
//
//  "several pushing cases with some created on the fly from a more populated situation" in {
//    /*
//            _ _ _ * *
//           _ _ _ 0 * *
//          _ _ _ _ 0 0 _
//         0 _ _ _ _ _ 0 *
//        _ 0 _ 0 0 0 * _ 0
//         _ 0 _ _ _ * * _
//          _ _ _ _ _ * _
//           _ * _ 0 0 _
//     * _ * 0 0
//     */
//    _testEveryMoveLoadFenIsometry(
//      lib,
//      StratFen(lib, format.FEN("3ss/3Sss/4SS1/S5Ss/1S1SSSs1S/1S3ss1/5s1/1s1SS1/s1sSS 0 0 b 0 0").value),
//      stratVariant
//    )(
//      List(
//        "d5g5",
//        "h5e2",
//        "e1c1",
//        "b1b3",                  // 00
//        "b6b3", /* 01 */ "a1c3", // 02
//        "b5b2",
//        "f3d1",
//        "b4b1",
//        "i9i7",
//        "e5h5",
//        "i8i5",
//        "f5i5",
//        "i7i5",
//        "g5i5",
//        "d1f3",
//        "h5h8",
//        "h9g9",
//        "g7g9"
//      ).map(uciStr => StratUci(lib, gameFamily, uciStr).get)
//    ) must beValid.like(gameData => {
//      val fen1 = StratForsyth.>>(lib, gameData.game)
//      val fen2 = StratForsyth.>>(lib, gameData.fenGame)
//      fen1 must_== fen2
//      // fen1.value must_== "3SS/3sSS/4ss1/s5sS/1s2sss1s/1s3SS1/1S3S1/1S1sS1/S1ss1 0 1 b 2 3" // 00
//      // fen1.value must_== "3SS/3sSS/4ss1/6sS/1s2sss1s/1s3SS1/1s3S1/1S1sS1/SSss1 0 1 w 3 3" // 01
//      // fen1.value must_== "3SS/3sSS/4ss1/6sS/1s2sss1s/1s3SS1/1sS2S1/1S1sS1/1Sss1 0 1 b 4 4" // 02
//      fen1.value must_== "2S2/3SS1/5S1/6Ss/8S/5ss1/1Ss2s1/1S1Ss1/1SS2 6 4 w 0 10"
//      /*
//            _ _ 0 _ _   0 0 0
//           _ _ _ 0 0 _   0 _
//          _ _ _ _ _ 0 _   _
//         _ _ _ _ _ _ 0 *
//        _ _ _ _ _ _ _ _ 0
//         _ _ _ _ _ * * _
//          _ 0 * _ _ * _   *
//           _ 0 _ 0 * _   * *
//            _ 0 0 _ _   * * *
//       */
//    })
//  }
//
//  "playing a few side moves and a push out" in {
//    /*
//            1 _ 1 _ _
//           _ 1 _ 1 _ _
//          _ 2 2 2 1 _ _
//         _ _ _ _ _ 1 1 _
//        _ _ 1 _ _ _ _ _ _
//         _ _ _ _ _ _ _ _
//          _ _ _ _ _ _ _
//           _ _ _ _ _ _
//            _ _ _ _ _
//     */
//    _testEveryMoveLoadFenIsometry(
//      lib,
//      StratFen(lib, format.FEN("S1S2/1S1S2/1sssS2/5SS1/2S6/8/7/6/5 0 0 b 0 0").value),
//      stratVariant
//    )(
//      List(
//        "c5c6",
//        "d7f6",                   // 1
//        "g6i7", /* 2 */ "f6e7",   // 3
//        "e9e7",
//        "e6c6",                   // 4
//        "e7d8", /* 5 */ "d6b6",   // 6
//        "i7g6",
//        "f7e7",                   // 7
//        "g9f6", /* 7.5 */ "e7d6", // 8
//        "d8d6",
//        "b6d6",                   // 9
//        "g6d6",
//        "b6c5",
//        "f8g6"
//      ).map(uciStr => StratUci(lib, gameFamily, uciStr).get)
//    ) must beValid.like(gameData => {
//      val fen1 = StratForsyth.>>(lib, gameData.game)
//      val fen2 = StratForsyth.>>(lib, gameData.fenGame)
//      fen1 must_== fen2
//      // fen1.value must_== "s1s2/1s1s2/4s2/1sSSSss1/9/8/7/6/5 0 0 b 2 2" // 1
//      // fen1.value must_== "s1s2/1s1s2/4sss/1sSSS3/9/8/7/6/5 0 0 w 3 2" // 2
//      // fen1.value must_== "s1s2/1s1s2/2SSsss/1sS5/9/8/7/6/5 0 0 b 4 3" // 3
//      // fen1.value must_== "2s2/1s1s2/2sSsss/sSS5/9/8/7/6/5 0 0 b 6 4" // 4
//      // fen1.value must_== "2s2/s2s2/1s1Ssss/sSS5/9/8/7/6/5 0 0 w 7 4" // 5
//      // fen1.value must_== "2s2/s2s2/1s1Ssss/SS6/9/8/7/6/5 0 1 b 0 5" // 6
//      // fen1.value must_== "2s2/s2s2/1sS1s2/SS3ss1/9/8/7/6/5 0 1 b 2 6" // 7
//      // fen1.value must_== "5/s1s3/1sSs3/SS2sss1/9/8/7/6/5 0 1 w 3 6" // 7.5
//      // fen1.value must_== "5/s1s3/1s1s3/SSS1sss1/9/8/7/6/5 0 1 b 4 7" // 8
//      // fen1.value must_== "5/2s3/1s1s3/1SSssss1/3s5/8/7/6/5 0 1 b 4 3" // 9
//      fen1.value must_== "5/3S2/1S2S2/2SS1SS1/1sss5/8/7/6/5 0 1 w 9 9"
//      /*
//            _ _ _ _ _   1 _ _
//           _ _ _ 1 _ _   _ _
//          _ 1 _ _ 1 _ _   _
//         _ _ 1 1 _ 1 1 _
//        _ 2 2 2 _ _ _ _ _
//         _ _ _ _ _ _ _ _
//          _ _ _ _ _ _ _   _
//           _ _ _ _ _ _   _ _
//            _ _ _ _ _   _ _ _
//       */
//    })
//  }
//
//  "playing a few common moves from Belgian Daisy start position..." in {
//    _testEveryMoveLoadFenIsometry(lib, StratFen(lib, variant.Abalone.initialFen.value), stratVariant)(
//      List(
//        "a1d4",
//        "e7g9",
//        "b1d3",
//        "e9h9",
//        "g7g9"
//      ).map(uciStr => StratUci(lib, gameFamily, uciStr).get)
//    ) must beValid.like(gameData => {
//      val fen1 = StratForsyth.>>(lib, gameData.game)
//      val fen2 = StratForsyth.>>(lib, gameData.fenGame)
//      fen1 must_== fen2
//      fen1.value must_== "1sSsS/sssSSS/1s3S1/8/9/3S4/1SSSss1/SSSsss/3ss 1 1 w 0 3"
//    })
//  }
//
//  "pushing out 6 marbles consecutively from custom position" in {
//    /*
//            _ _ _ * *
//           _ _ _ 0 * *
//          _ _ _ _ 0 0 _
//         _ _ _ _ _ _ 0 _
//        _ _ _ _ 0 0 0 * *
//         _ _ _ _ _ _ _ _
//          _ _ _ _ _ _ _
//           _ _ _ _ _ _
//     * _ _ _ _
//     */
//    _testEveryMoveLoadFenIsometry(
//      lib,
//      StratFen(lib, format.FEN("3ss/3Sss/4SS1/6S1/4SSSss/8/7/6/s4 0 0 b 0 0").value),
//      stratVariant
//    )(
//      List(
//        "e5h5",
//        "a1a2",
//        "f5i5",
//        "a2a1",
//        "h5h8",
//        "a1a2",
//        "h6h9",
//        "a2a1",
//        "g8i8",
//        "a1a2",
//        "g7i9"
//      ).map(uciStr => StratUci(lib, gameFamily, uciStr).get)
//    ) must beValid.like(gameData => {
//      val fen1 = StratForsyth.>>(lib, gameData.game)
//      val fen2 = StratForsyth.>>(lib, gameData.fenGame)
//      fen1 must_== fen2
//      fen1.value must_== "3SS/4SS/5S1/8/6S1S/8/7/s5/5 6 0 w 0 6"
//      /*
//            _ _ _ 0 0   _ _ _
//           _ _ _ _ 0 0   _ _
//          _ _ _ _ _ 0 _   _
//         _ _ _ _ _ _ _ _
//        _ _ _ _ _ _ 0 _ 0
//         _ _ _ _ _ _ _ _
//          _ _ _ _ _ _ _   *
//       * _ _ _ _ _   * *
//            _ _ _ _ _   * * *
//       */
//    })
//  }
//
//  "MSO 2024 final game - Francesco SALERNO vs Kyungmin KANG" in {
//    _testEveryMoveLoadFenIsometry(lib, StratFen(lib, variant.Abalone.initialFen.value), stratVariant)(
//      List(
//        "a1d4",
//        "e9e6",
//        "b1d3",
//        "f9f7",
//        "h7f6",
//        "e1e4",
//        "b2e5",
//        "e3g4", // 01
//        "c2e4",
//        "e7c6",
//        "e4e6",
//        "f4h5",
//        "g8i7",
//        "d8e9",
//        "h7f5",
//        "d2g3", // 02
//        "d3d5",
//        "e4f4",
//        "g6e4",
//        "f3h4",
//        "a2c4",
//        "e3h6",
//        "i7h7",
//        "g4g6", // 03
//        "c4f7",
//        "h4i6",
//        "c3b4",
//        "e8h8",
//        "h7e7",
//        "d6d8",
//        "e4g6",
//        "h6h9",
//        "d5g8",
//        "h9h6",
//        "e7h7",
//        "i6h4",
//        "g8g5",
//        "h4e4",
//        "e6h6",
//        "i6f3",
//        "d4b5",
//        "d8d6",
//        "d5e6",
//        "g4d4",
//        "b5d5",
//        "f8e7",
//        "g6d6"  // p2 resigns
//      ).map(uciStr => StratUci(lib, gameFamily, uciStr).get)
//    ) must beValid.like(gameData => {
//      val fen1 = StratForsyth.>>(lib, gameData.game)
//      val fen2 = StratForsyth.>>(lib, gameData.fenGame)
//      fen1 must_== fen2
//      // fen1.value must_== "3ss/SSSsss/1SSS3/3Sss2/4s4/3sSSS1/1sss3/s1sSSS/3S1 0 0 b 8 5" // 01
//      // fen1.value must_== "S2ss/1SS3/2SSs1s/1SSsss2/4ssSS1/3sS3/1sssSSS/s5/3S1 0 0 b 16 9" // 02
//      // fen1.value must_== "S2ss/1SS3/2SSss1/1SSssSS1/3sssSS1/2sssS1S/1ss4/6/3S1 0 0 b 24 13" // 03
//      /*
//       * _ _ 0 0
//           _ * * _ _ _
//          _ _ * * 0 0 _
//         _ * * 0 0 * * _
//        _ _ _ 0 0 0 * * _
//         _ _ 0 0 0 * _ *
//          _ 0 0 _ _ _ _
//           _ _ _ _ _ _
//            _ _ _ * _
//       */
//      fen1.value must_== "s3S/4s1/1ssSSSs/ssSSS1S1/2SSSSSs1/3sss2/5s1/6/3s1 1 1 w 14 24"
//      /*
//       * _ _ _ 0   0 _ _
//           _ _ _ _ * _   _ _
//          _ * * 0 0 0 *   _
//         _ * * 0 0 0 0 _
//        _ _ 0 0 0 0 0 * _
//         _ _ _ * * * _ _
//          _ _ _ _ _ * _   _
//           _ _ _ _ _ _   _ _
//            _ _ _ * _   * _ _
//       */
//    })
//  }
//
//  "MSO 2023 final game - Jiyun LIM vs Vincent FROCHOT" in {
//    _testEveryMoveLoadFenIsometry(lib, StratFen(lib, variant.Abalone.initialFen.value), stratVariant)(
//      List(
//        "a1d4",
//        "e9e6",
//        "b1d3",
//        "f9f7",
//        "i9i7",
//        "e8e5",
//        "c2e4",
//        "f2f4",
//        "i7f6",
//        "f3f5", // opening
//        "b3e3",
//        "e2g4",
//        "h6e6",
//        "d8d5",
//        "a2c2",
//        "d1f2",
//        "d4d2",
//        "g4e4",
//        "d2f4",
//        "f2g4", // 02
//        "g6d6",
//        "g3g6",
//        "f6c6",
//        "f8d6",
//        "b2e5",
//        "d7d4",
//        "c2e2",
//        "f2g3",
//        "d2g5",
//        "d6d3", // 03
//        "g8i7",
//        "h6e6",
//        "i7f7",
//        "d5f7",
//        "c5e7",
//        "b6b5",
//        "e7c5",
//        "d3d5",
//        "h7f7",
//        "d4c4", // 04
//        "f4f3",
//        "g6d6",
//        "e3g3",
//        "f6c6",
//        "f7g6",
//        "g4f4",
//        "h9f7",
//        "d1c1",
//        "c3d4",
//        "d6b6", // 05
//        "f7h8",
//        "e4g6",
//        "h7f7",
//        "b6d6",
//        "e2d3",
//        "e7e5",
//        "d4f4",
//        "d5c3",
//        "g3e2",
//        "f8e7", // 06
//        "f2d2",
//        "d7d5",
//        "f4d4",
//        "c4f7",
//        "d2c2",
//        "d5g5",
//        "c2f5",
//        "g4g7",
//        "d3g6",
//        "c6f6", // 07
//        "e2f4",
//        "d6g6",
//        "i6g4",
//        "i8f5",
//        "e4d2",
//        "f7f4",
//        "h6h4",
//        "f6f3",
//        "h4f4",
//        "g5g8", // 08
//        "g9f8",
//        "e6e3",
//        "e2c2",
//        "g8g5",
//        "h5f3",
//        "c3d3",
//        "f2f5",
//        "f6d4",
//        "f3f6",
//        "d4d2", // 09
//        "g4e4",
//        "e5c3",
//        "f4d4",
//        "c1f4",
//        "f6f4",
//        "c4c2",
//        "d1b1",
//        "d3b1",
//        "c1d1",
//        "f3d1"  // 10
//      ).map(uciStr => StratUci(lib, gameFamily, uciStr).get)
//    ) must beValid.like(gameData => {
//      val fen1 = StratForsyth.>>(lib, gameData.game)
//      val fen2 = StratForsyth.>>(lib, gameData.fenGame)
//      fen1 must_== fen2
//      // fen1.value must_== "3s1/S1Ssss/1SSS3/3Ssss1/4SS3/3ssS2/1sssS2/ss1SS1/3SS 0 0 b 10 6"  // opening
//      /*
//            _ _ _ 0 _
//       * _ * 0 0 0
//          _ * * * _ _ _
//         _ _ _ * 0 0 0 _
//        _ _ _ _ * * _ _ _
//         _ _ _ 0 0 * _ _
//          _ 0 0 0 * _ _
//           0 0 _ * * _
//            _ _ _ * *
//       */
//      // fen1.value must_== "3s1/2Ssss/1SSS3/2Ssss2/3SSSS2/3sSsS1/2sss1S/1ss1S1/3S1 0 0 b 20 11" // 02
//      // fen1.value must_== "3s1/3sss/2SS3/Ss1sSSS1/2sSsSs2/3SSsS1/2sSs1S/3ss1/3S1 0 0 b 30 16" // 03
//      // fen1.value must_== "3s1/2Ss2/1SSss2/1ssSSS2/1SsSsSs2/2S1SsS1/2s1s1S/3ss1/3S1 0 0 b 40 21" // 04
//      // fen1.value must_== "5/2Ss2/1SSs3/SS1Sss2/1SsSsSs2/2SsSS2/5ss/3ss1/2S2 1 2 b 0 26" // 05
//      // fen1.value must_== "5/4s1/1SSss2/1SSSsS2/1Ss1SSs2/3SssS1/2Sss2/4ss/2S2 1 2 b 10 31" // 06
//      // fen1.value must_== "2s2/3ssS/2SSSS1/2SSSss1/1Ss1SsSs1/3ss3/2S1s2/4s1/2S2 1 2 b 20 36" // 07
//      // fen1.value must_== "2s2/3Ss1/2S1SS1/3S1S2/1Ss1SS1s1/3sSss1/2S1sS1/3s1s/2S2 1 4 b 0 41" // 08
//      // fen1.value must_== "5/2s1s1/2S1SS1/4sS2/1Ss1SsS2/4Sss1/2sSS2/2sSS1/2Ss1 1 4 b 10 46" // 09
//      fen1.value must_== "5/2S1S1/2s1ss1/5s2/1sS2Ss2/3SSS2/2s1s2/1Ssss1/1s1s1 1 6 b 0 51" // 10
//      /*
//            _ _ _ _ _   0 0 0
//           _ _ 0 _ 0 _   0 0
//          _ _ * _ * * _   0
//         _ _ _ _ _ * _ _
//        _ * 0 _ _ 0 * _ _
//         _ _ _ 0 0 0 _ _
//          _ _ * _ * _ _   _
//           _ 0 * * * _   _ _
//            _ * _ * _   * _ _
//       */
//    })
//  }
//
//  "MSO 2022 final game - Francesco SALERNO vs Vincent FROCHOT" in {
//    _testEveryMoveLoadFenIsometry(lib, StratFen(lib, variant.Abalone.initialFen.value), stratVariant)(
//      List(
//        "a1d4",
//        "e9e6",
//        "b1d3",
//        "f3e4",
//        "c2e4",
//        "e8e5",
//        "b2e5",
//        "f9f7",
//        "g7h6",
//        "f7d5", // opening
//        "b3e3",
//        "d8e8",
//        "c3f6",
//        "e7c6",
//        "e3e6",
//        "f8f7",
//        "g6g5",
//        "f2e3",
//        "d4f4",
//        "f3d3", // 02
//        "h6e3",
//        "d2d4",
//        "f4d4",
//        "f5g6",
//        "g5f5",
//        "d6b5",
//        "f5d3",
//        "e8d6",
//        "f6d6",
//        "b5e5", // 03
//        "c3e5",
//        "c6b5",
//        "e6e7",
//        "c1c3",
//        "e7c5",
//        "c2c5",
//        "e3f4",
//        "b5e5",
//        "c6e6",
//        "b4d6", // 04
//        "e4g6",
//        "c5e7",
//        "h9h7",
//        "e7h7",
//        "f8e7",
//        "e5h8",
//        "e7e5",
//        "c3c5",
//        "e6e4",
//        "h8e5", // 05
//        "c3e3",
//        "c5f5",
//        "e3e5",
//        "h6h8",
//        "e4e6",
//        "h8e5",
//        "i7h6",
//        "d5g5",
//        "i5i6",
//        "g7d4", // 06
//        "e6d5",
//        "g4g6",
//        "i8h8",
//        "f5h5",
//        "a2c2",
//        "d6e6",
//        "b2d2",
//        "e6h6",
//        "c2e4",
//        "h7f5", // 07
//        "c3e3",
//        "d7g7",
//        "d2g5",
//        "e5h8",
//        "i9i8",
//        "f7f4",
//        "f3e2",
//        "f6i6",
//        "g8f7",
//        "i7i5", // 08
//        "e2e5",
//        "g7g8",
//        "i8i9",
//        "f5h7",
//        "d3f5",
//        "g8i8"
//      ).map(uciStr => StratUci(lib, gameFamily, uciStr).get)
//    ) must beValid.like(gameData => {
//      val fen1 = StratForsyth.>>(lib, gameData.game)
//      val fen2 = StratForsyth.>>(lib, gameData.fenGame)
//      fen1 must_== fen2
//      // fen1.value must_== "3ss/S1Ssss/1SS4/3SSss1/3SsS3/3ssS2/1sss3/s2SSS/3SS 0 0 b 10 6" // opening
//      /*
//            _ _ _ 0 0
//       * _ * 0 0 0
//          _ * * _ _ _ _
//         _ _ _ * * 0 0 _
//        _ _ _ * 0 * _ _ _
//         _ _ _ 0 0 * _ _
//          _ 0 0 0 _ _ _
//           0 _ _ * * *
//            _ _ _ * *
//       */
//      // fen1.value must_== "3ss/1S1sss/2SSS2/1SSss1s1/3SsSs2/4ssS1/2sSS2/s2S2/3SS 0 0 b 20 11" // 02
//      // fen1.value must_== "3ss/3sss/1S1SS2/1Sss1S2/2SSSs3/2Sss1S1/2sss2/s1S3/2SSS 0 0 b 30 16" // 03
//      // fen1.value must_== "3ss/3sss/1SsSS2/2SsSS2/2SSSss2/2SsssS1/2Ss3/s5/3SS 0 0 b 40 21" // 04
//      // fen1.value must_== "4s/3s1s/1S1SSSs/2S1SsS1/2SSSss2/2SsssS1/2ss3/s5/3SS 0 1 b 4 26" // 05
//      // fen1.value must_== "4s/3s1s/1SSS1S1/2SsSsss/4SSSs1/2SS1sS1/2ss3/ss4/3SS 0 1 b 14 31" // 06
//      // fen1.value must_== "4s/3ss1/1SSSs2/4SSSs/3sSSSSs/2SSss2/2ss3/3s2/3SS 0 2 b 2 36" // 07
//      // fen1.value must_== "5/4Ss/2SsSs1/5SSS/3s1SsSS/2SSsS2/3ss2/4s1/3SS 0 5 b 0 41" // 08
//      fen1.value must_== "4S/4ss/2sS1s1/5sss/3SSSSss/2ssSs2/4S2/6/3ss 0 6 b 0 44"
//      /*
//            _ _ _ _ 0   0 0 0
//           _ _ _ _ * *   0 0
//          _ _ * 0 _ * _   0
//         _ _ _ _ _ * * *
//        _ _ _ 0 0 0 0 * *
//         _ _ * * 0 * _ _
//          _ _ _ _ 0 _ _   _
//           _ _ _ _ _ _   _ _
//            _ _ _ * *   _ _ _
//       */
//    })
//  }
}
