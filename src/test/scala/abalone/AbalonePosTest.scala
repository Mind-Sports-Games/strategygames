package strategygames.abalone

import org.specs2.matcher.ValidatedMatchers
import strategygames.abalone.variant.Abalone

class AbalonePosTest extends AbaloneTest with ValidatedMatchers {
  "grid coordinates" should {
    "describe 61 positions" in {
      Abalone.boardType.cellList.size must_== 61
    }

    "be shaped as a hexagon" in {
      // First row
      Abalone.boardType.isCell(new Pos(0, 0)) must_== true
      Abalone.boardType.isCell(new Pos(1, 0)) must_== true
      Abalone.boardType.isCell(new Pos(2, 0)) must_== true
      Abalone.boardType.isCell(new Pos(3, 0)) must_== true
      Abalone.boardType.isCell(new Pos(4, 0)) must_== true
      Abalone.boardType.isCell(new Pos(5, 0)) must_== false
      Abalone.boardType.isCell(new Pos(6, 0)) must_== false
      Abalone.boardType.isCell(new Pos(7, 0)) must_== false
      Abalone.boardType.isCell(new Pos(8, 0)) must_== false

      Abalone.boardType.isCell(new Pos(0, 1)) must_== true
      Abalone.boardType.isCell(new Pos(1, 1)) must_== true
      Abalone.boardType.isCell(new Pos(2, 1)) must_== true
      Abalone.boardType.isCell(new Pos(3, 1)) must_== true
      Abalone.boardType.isCell(new Pos(4, 1)) must_== true
      Abalone.boardType.isCell(new Pos(5, 1)) must_== true
      Abalone.boardType.isCell(new Pos(6, 1)) must_== false
      Abalone.boardType.isCell(new Pos(7, 1)) must_== false
      Abalone.boardType.isCell(new Pos(8, 1)) must_== false

      Abalone.boardType.isCell(new Pos(0, 2)) must_== true
      Abalone.boardType.isCell(new Pos(1, 2)) must_== true
      Abalone.boardType.isCell(new Pos(2, 2)) must_== true
      Abalone.boardType.isCell(new Pos(3, 2)) must_== true
      Abalone.boardType.isCell(new Pos(4, 2)) must_== true
      Abalone.boardType.isCell(new Pos(5, 2)) must_== true
      Abalone.boardType.isCell(new Pos(6, 2)) must_== true
      Abalone.boardType.isCell(new Pos(7, 2)) must_== false
      Abalone.boardType.isCell(new Pos(8, 2)) must_== false

      Abalone.boardType.isCell(new Pos(0, 3)) must_== true
      Abalone.boardType.isCell(new Pos(1, 3)) must_== true
      Abalone.boardType.isCell(new Pos(2, 3)) must_== true
      Abalone.boardType.isCell(new Pos(3, 3)) must_== true
      Abalone.boardType.isCell(new Pos(4, 3)) must_== true
      Abalone.boardType.isCell(new Pos(5, 3)) must_== true
      Abalone.boardType.isCell(new Pos(6, 3)) must_== true
      Abalone.boardType.isCell(new Pos(7, 3)) must_== true
      Abalone.boardType.isCell(new Pos(8, 3)) must_== false

      Abalone.boardType.isCell(new Pos(0, 4)) must_== true
      Abalone.boardType.isCell(new Pos(1, 4)) must_== true
      Abalone.boardType.isCell(new Pos(2, 4)) must_== true
      Abalone.boardType.isCell(new Pos(3, 4)) must_== true
      Abalone.boardType.isCell(new Pos(4, 4)) must_== true
      Abalone.boardType.isCell(new Pos(5, 4)) must_== true
      Abalone.boardType.isCell(new Pos(6, 4)) must_== true
      Abalone.boardType.isCell(new Pos(7, 4)) must_== true
      Abalone.boardType.isCell(new Pos(8, 4)) must_== true

      Abalone.boardType.isCell(new Pos(0, 5)) must_== false
      Abalone.boardType.isCell(new Pos(1, 5)) must_== true
      Abalone.boardType.isCell(new Pos(2, 5)) must_== true
      Abalone.boardType.isCell(new Pos(3, 5)) must_== true
      Abalone.boardType.isCell(new Pos(4, 5)) must_== true
      Abalone.boardType.isCell(new Pos(5, 5)) must_== true
      Abalone.boardType.isCell(new Pos(6, 5)) must_== true
      Abalone.boardType.isCell(new Pos(7, 5)) must_== true
      Abalone.boardType.isCell(new Pos(8, 5)) must_== true

      Abalone.boardType.isCell(new Pos(0, 6)) must_== false
      Abalone.boardType.isCell(new Pos(1, 6)) must_== false
      Abalone.boardType.isCell(new Pos(2, 6)) must_== true
      Abalone.boardType.isCell(new Pos(3, 6)) must_== true
      Abalone.boardType.isCell(new Pos(4, 6)) must_== true
      Abalone.boardType.isCell(new Pos(5, 6)) must_== true
      Abalone.boardType.isCell(new Pos(6, 6)) must_== true
      Abalone.boardType.isCell(new Pos(7, 6)) must_== true
      Abalone.boardType.isCell(new Pos(8, 6)) must_== true

      Abalone.boardType.isCell(new Pos(0, 7)) must_== false
      Abalone.boardType.isCell(new Pos(1, 7)) must_== false
      Abalone.boardType.isCell(new Pos(2, 7)) must_== false
      Abalone.boardType.isCell(new Pos(3, 7)) must_== true
      Abalone.boardType.isCell(new Pos(4, 7)) must_== true
      Abalone.boardType.isCell(new Pos(5, 7)) must_== true
      Abalone.boardType.isCell(new Pos(6, 7)) must_== true
      Abalone.boardType.isCell(new Pos(7, 7)) must_== true
      Abalone.boardType.isCell(new Pos(8, 7)) must_== true

      // Last row
      Abalone.boardType.isCell(new Pos(0, 8)) must_== false
      Abalone.boardType.isCell(new Pos(1, 8)) must_== false
      Abalone.boardType.isCell(new Pos(2, 8)) must_== false
      Abalone.boardType.isCell(new Pos(3, 8)) must_== false
      Abalone.boardType.isCell(new Pos(4, 8)) must_== true
      Abalone.boardType.isCell(new Pos(5, 8)) must_== true
      Abalone.boardType.isCell(new Pos(6, 8)) must_== true
      Abalone.boardType.isCell(new Pos(7, 8)) must_== true
      Abalone.boardType.isCell(new Pos(8, 8)) must_== true
    }

    "compute its index based on a shape of square (sort of...) when accessed in 2D" in {
      new Pos(0, 0) must_== Pos.fromIndex(0)
      new Pos(0, 1) must_== Pos.fromIndex(8)

      new Pos(1, 1) must_== Pos.fromIndex(9)

      new Pos(6, 9) must_== Pos.fromIndex(80)
    }
  }

  /*
   * 8 -              i5 i6 i7 i8 i9
   * 7 -            h4 h5 h6 h7 h8 h9
   * 6 -          g3 g4 g5 g6 g7 g8 g9
   * 5 -        f2 f3 f4 f5 f6 f7 f8 f9
   * 4 -      e1 e2 e3 e4 e5 e6 e7 e8 e9
   * 3 -       d1 d2 d3 d4 d5 d6 d7 d8
   * 2 -        c1 c2 c3 c4 c5 c6 c7
   * 1 -         b1 b2 b3 b4 b5 b6
   * 0 -          a1 a2 a3 a4 a5
   *               \  \  \  \  \  \  \  \  \
   * y/x            0  1  2  3  4  5  6  7  8
   */
  "official notation" should {
    "be written yx with y as a lowercase letter and x as a number" in {
      new Pos(6, 5).key must_== "f7"
    }
  }
}
