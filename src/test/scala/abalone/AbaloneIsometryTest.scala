package strategygames.abalone

import strategygames.abalone.variant.Abalone

class AbaloneIsometryTest extends AbaloneTest {
  /*
   *     o o · ● ●   · · ·         o · · ● ●   ● · ·
   *    o o o ● ● ●   · ·         · o o · · ·   · ·
   *   · o o · ● ● ·   ·         · · o o ● ● ·   ·
   *  · · · · · · · ·           · o o ● ● o o ·
   * · · · · · · · · ·     ->  · · · ● ● ● o o ·
   *  · · · · · · · ·           · · ● ● ● o · o
   *   · ● ● · o o ·   ·         · ● ● · · · ·   ·
   *    ● ● ● o o o   · ·         · · · · · ·   · ·
   *     ● ● · o o   · · ·         · · · o ·   o · ·
   */
  "MSO 2024 final game - Francesco SALERNO vs Kyungmin KANG" in {
    checkFinalFen(
      Abalone,
      List(
        "a1d4",
        "i5f5",
        "a2c4",
        "i6g6",
        "g8f6",
        "a5d5",
        "b2e5",
        "c5d7",
        "b3e6",
        "g5f3",
        "d5g5",
        "d6e8",
        "h7g9",
        "h4i5",
        "g8d5",
        "b4c7",
        "c4e4",
        "d5d6",
        "f7d5",
        "c6d8",
        "b1d3",
        "c5f8",
        "g9g8",
        "d7f7",
        "d3h7",
        "d8f9",
        "c3d2",
        "h5h8",
        "g8g4",
        "f4h4",
        "d5g8",
        "f8i8", // f8xi8
        "e4i8", // e4xi8
        "i8f8",
        "g5g9",
        "f9d8",
        "h7d7",
        "d8d5",
        "f5f9",
        "f9c6",
        "d4e2",
        "h4f4",
        "e4f5",
        "d7d4",
        "e2e4",
        "h6g5",
        "f7f2"  // p2 resigns
      ),
      "s3S/4s1/1ssSSSs/ssSSS1S1/2SSSSSs1/3sss2/5s1/6/3s1 1 1 w 14 24"
    )
  }

  /*
   *     o o · ● ●   · · ·         · · · · ·   ● ● ●
   *    o o o ● ● ●   · ·         · · ● · ● ·   ● ●
   *   · o o · ● ● ·   ·         · · o · o o ·   ●
   *  · · · · · · · ·           · · · · · o · ·
   * · · · · · · · · ·     ->  · o ● · · ● o · ·
   *  · · · · · · · ·           · · · ● ● ● · ·
   *   · ● ● · o o ·   ·         · · o · o · ·   ·
   *    ● ● ● o o o   · ·         · ● o o o ·   · ·
   *     ● ● · o o   · · ·         · o · o ·   o · ·
   */
  "MSO 2023 final game - Jiyun LIM vs Vincent FROCHOT" in {
    checkFinalFen(
      Abalone,
      List(
        "a1d4",
        "i5f5",
        "a2c4",
        "i6g6",
        "i9g9",
        "h5e5",
        "b3d5",
        "b6d6",
        "g9f6",
        "c6e6", // opening
        "c2c6",
        "b5d7",
        "f8f4",
        "h4e4",
        "b1b3",
        "a4b6",
        "d4a4",
        "d7d4",
        "b4e7",
        "b6d7",
        "f7f3",
        "c7f7",
        "f6f2",
        "h6e3",
        "b2f6",
        "g4b4",
        "b3b6",
        "b6c7",
        "b4f8",
        "f4b4",
        "h7g9",
        "f8f4",
        "g9g4",
        "e4h7",
        "e3h6",
        "f2e2",
        "g5e3",
        "c4e4",
        "g8g5",
        "d4d3",
        "d6c6",
        "f7f2",
        "c5c7", // c5xc7
        "f6f2", // f6xf2
        "g6f7",
        "d7d6",
        "i8g6",
        "a4a3",
        "c3d4",
        "f4f2", // f4xf2
        "g6h8",
        "d5g8",
        "g8g6",
        "f2f4",
        "b5c4",
        "g5d5",
        "d4d7",
        "e4c3",
        "c7b5",
        "h6g5",
        "b6b4",
        "g4e4",
        "d6d3",
        "d3h7",
        "b4b3",
        "e4e8",
        "b3g8",
        "d7i7",
        "c4h9",
        "f3f8",
        "b5d6",
        "f4f9",
        "f9d7",
        "h9c4",
        "d5b4",
        "g6b6",
        "f8d8",
        "f6b6", // f6xb6
        "d8d5",
        "e7i7", // e7xi7
        "i7h6",
        "f5b5",
        "b5b3",
        "h7e7",
        "e8b5",
        "c3c4",
        "b6f6",
        "f6c3",
        "c6f6",
        "d4a4",
        "d7d4",
        "e5b2",
        "d6d3",
        "a3d6",
        "f6c6",
        "d3a3",
        "a4a2",
        "c4a2", // c4xa2,
        "a3a4",
        "c6a4"  // c6xa4
      ),
      "5/2S1S1/2s1ss1/5s2/1sS2Ss2/3SSS2/2s1s2/1Ssss1/1s1s1 1 6 b 0 51"
    )
  }

  /*
   *     o o · ● ●   · · ·         · · · · ●   ● ● ●
   *    o o o ● ● ●   · ·         · · · · o o   ● ●
   *   · o o · ● ● ·   ·         · · o ● · o ·   ●
   *  · · · · · · · ·           · · · · · o o o
   * · · · · · · · · ·     ->  · · · ● ● ● ● o o
   *  · · · · · · · ·           · · o o ● o · ·
   *   · ● ● · o o ·   ·         · · · · ● · ·   ·
   *    ● ● ● o o o   · ·         · · · · · ·   · ·
   *     ● ● · o o   · · ·         · · · o o   · · ·
   */
  "MSO 2022 final game - Francesco SALERNO vs Vincent FROCHOT" in {
    checkFinalFen(
      Abalone,
      List(
        "a1d4",
        "i5f5",
        "a2c4",
        "c6d5",
        "b3e6",
        "h5e5",
        "b2f6",
        "i6g6",
        "g7f8",
        "g6e4", // Opening
        "c2c5",
        "h4h5",
        "c3g7",
        "g5f3",
        "c5g5",
        "h6g6",
        "f7e7",
        "b6c5",
        "d4d7",
        "c6c3", // 20
        "f8a3",
        "b4d4",
        "d6d3",
        "e6f7",
        "e7e6",
        "f4e2",
        "e6b3",
        "h5f4",
        "f6f3",
        "e2e6", // 30
        "c3f6",
        "f3e2",
        "f5g5",
        "a3c3",
        "g5d2",
        "b3f3",
        "c5d6",
        "e2e7",
        "f3f5",
        "d2g5", // 40
        "d5g8",
        "e3h6",
        "i8f8",
        "g5g9",
        "h6g5",
        "e5i9", // e5xi9
        "g5e5",
        "c3e3",
        "f5d5",
        "h8c3", // 50
        "c3c5",
        "e3e8",
        "c5f5",
        "f8h8",
        "d5g5",
        "h8c3",
        "g9f8",
        "e4e9",
        "e9f9",
        "g7b2", // 60
        "f5e4",
        "d7g7",
        "h9h8",
        "e6e9",
        "b1b3",
        "f4f5",
        "b2b4",
        "f5f9", // f5xf9
        "b3d5",
        "g8e6", // 07
        "c3c5",
        "g4g8",
        "b4g9",
        "e5i9", // e5xi9
        "i9h9",
        "g6c6",
        "c6b5",
        "f6f9", // f6xf9
        "h7g6",
        "g9e9", // g9xe9 // 08
        "b5e5",
        "g7h7",
        "h9i9",
        "e6h9",
        "c4e6",
        "h7h9"  // h7xh9
      ),
      "4S/4ss/2sS1s1/5sss/3SSSSss/2ssSs2/4S2/6/3ss 0 6 b 0 44"
    )
  }
}
