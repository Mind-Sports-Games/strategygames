package strategygames.abalone

import strategygames.abalone.variant.GrandAbalone

class GrandAbaloneIsometryTest extends AbaloneTest {
  /*
   *      ● ● · · o o   · · · ·         · ● o · · ·   ● ● ● ●
   *     ● ● ● · o o o   · · ·         · ● ● ● · · ·   · · ·
   *    · ● ● · · o o ·   · ·         · ● · o ● ● ● ·   · ·
   *   · · · · · · · · ·   ·         · · · · o · ● o ·   ·
   *  o o · · · · · · ● ●           · · · · o o ● o · ·
   * o o o · · · · · ● ● ●     ->  · · · o · · o o o · ·
   *  o o · · · · · · ● ●           o · ● · · o · ● · ·
   *   · · · · · · · · ·   ·         · · o · o ● · · ·   ·
   *    · ● ● · · o o ·   · ·         · · · ● ● ● o o   · ·
   *     ● ● ● · o o o   · · ·         · · · · · · o   · · ·
   *      ● ● · · o o   · · · ·         · · · · · o   o o · ·
   */
  "2024-09-02 Alex Borello vs Vincent Frochot" in {
    checkFinalFen(
      GrandAbalone,
      List(
        "a1d4",
        "f1f4",
        "f2f5",
        "b2e5",
        "k6h6",
        "f3f6",
        "k11h8",
        "f11f8",
        "j6g6",
        "j10g7",
        "i9b2",
        "i6e6",
        "j5i6",
        "f4f7",
        "h8a1",
        "f10f4",
        "i6d6",
        "a5c5",
        "g2f3",
        "a2c4",
        "b3d5",
        "f3e4",
        "e2e7",
        "f7d7",
        "c4f7",
        "e3e8",
        "b6i6",
        "e10e3",
        "e9e2",
        "b5b4",
        "b4g9",
        "d5g8",
        "f9f4",
        "e2f3",
        "f2f9",
        "e6d5",
        "j6e6",
        "c5h10",
        "e4h7",
        "f9b5",
        "e5h8",
        "d6i11",
        "e7i11", // e7xi11
        "e8c5",
        "c5e5",
        "f8i11", // f8xi11
        "f3f8",
        "g11f9",
        "f10f3",
        "h8e8",
        "i11c5",
        "c5g5",
        "d6d3",
        "f3b3",
        "j11h9",
        "f9d7",
        "b1b4",
        "e3a3",
        "d3a3",  // d3xa3
        "f5f9",
        "e8h8",
        "h10h5",
        "h9h4",
        "d4i9",
        "a1d4",
        "a3d3",
        "b3e3",
        "e3e4",
        "d7e8",
        "k10i8",
        "i10i7",
        "h4i5",
        "i5f5",
        "e6e9",
        "h6e6",
        "g5h6",
        "g8g5",
        "f5i5",
        "j9f5",
        "i5j6",
        "d5i5",
        "i5h4",
        "i9i5",
        "g5j8",
        "f7g8",
        "g6d6",
        "e8e3",
        "e4g6",
        "f8k8",
        "c3i9",
        "e6f7",
        "h6e6",
        "f5f8",
        "e5c4",
        "c4h9",
        "h9h6",
        "k7d7",
        "h4h9",
        "h5h10",
        "i9i10",
        "f6i9",
        "i6f6",
        "g6j9",
        "h10h5",
        "e9e8",
        "i8i11",
        "i9i11", // i9xi11
        "j9i8",
        "g8k8",  // g8xk8
        "h5g6",
        "g5g8",
        "h8i9",
        "i7i11", // i7xi11
        "i11h10",
        "b5c5",
        "b4c4",
        "b2c3",
        "d7i7",
        "e7k7",
        "c2c6",
        "c3c8"   // Unfinished
      ),
      "5s/6s/3SSSss/2s1sS3/s1S2s1S2/3s1Ssss2/4ssSs2/4s1Ss1/1S1sSSS1/1SSS3/1Ss3 2 4 w b 8 31"
    )
  }
}
