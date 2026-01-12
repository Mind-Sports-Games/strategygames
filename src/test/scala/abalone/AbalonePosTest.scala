package strategygames.abalone

import org.specs2.matcher.ValidatedMatchers

class AbalonePosTest extends AbaloneTest with ValidatedMatchers {
  "grid coordinates" should {
    "describe 61 positions" in {
      Pos.all.size === 61
    }

    "be shaped as an hexagon when accessed in 1D" in {
      Pos.isInHexagon(-1) === false

      Pos.isInHexagon(0) === true
      Pos.isInHexagon(1) === true
      Pos.isInHexagon(4) === true
      Pos.isInHexagon(5) === false
      Pos.isInHexagon(6) === false
      Pos.isInHexagon(7) === false
      Pos.isInHexagon(8) === false

      Pos.isInHexagon(9) === true
      Pos.isInHexagon(14) === true
      Pos.isInHexagon(15) === false
      Pos.isInHexagon(16) === false
      Pos.isInHexagon(17) === false

      Pos.isInHexagon(18) === true
      Pos.isInHexagon(25) === false
      Pos.isInHexagon(26) === false

      Pos.isInHexagon(35) === false
      Pos.isInHexagon(36) === true
      Pos.isInHexagon(44) === true

      Pos.isInHexagon(45) === false
      Pos.isInHexagon(53) === true

      Pos.isInHexagon(54) === false
      Pos.isInHexagon(55) === false
      Pos.isInHexagon(60) === true

      Pos.isInHexagon(63) === false
      Pos.isInHexagon(54) === false
      Pos.isInHexagon(65) === false
      Pos.isInHexagon(71) === true

      Pos.isInHexagon(72) === false
      Pos.isInHexagon(73) === false
      Pos.isInHexagon(74) === false
      Pos.isInHexagon(75) === false
      Pos.isInHexagon(80) === true

      Pos.isInHexagon(81) === false
      Pos.isInHexagon(88) === false

      Pos.isInHexagon(9001) === false
    }

    "be shaped as an hexagon when accessed in 2D" in {
      // testing first row
      Pos(File(0).get, Rank(0).get) !== None
      Pos(File(1).get, Rank(0).get) !== None
      Pos(File(2).get, Rank(0).get) !== None
      Pos(File(3).get, Rank(0).get) !== None
      Pos(File(4).get, Rank(0).get) !== None
      Pos(File(5).get, Rank(0).get) === None
      Pos(File(6).get, Rank(0).get) === None
      Pos(File(7).get, Rank(0).get) === None
      Pos(File(8).get, Rank(0).get) === None

      // testing last row
      Pos(File(0).get, Rank(8).get) === None
      Pos(File(1).get, Rank(8).get) === None
      Pos(File(2).get, Rank(8).get) === None
      Pos(File(3).get, Rank(8).get) === None
      Pos(File(4).get, Rank(8).get) !== None
      Pos(File(5).get, Rank(8).get) !== None
      Pos(File(6).get, Rank(8).get) !== None
      Pos(File(7).get, Rank(8).get) !== None
      Pos(File(8).get, Rank(8).get) !== None
    }

    "compute its index based on a shape of square when accessed in 2D" in {
      Pos(File(0).get, Rank(0).get) === Pos(0)
      Pos(File(8).get, Rank(0).get) === Pos(8)

      Pos(File(0).get, Rank(1).get) === Pos(9)

      Pos(File(8).get, Rank(8).get) === Pos(80)
    }
  }

  "general navigation system (from E5)" should {
    "increment letter only when moving to the right" in {
      Pos.E5.right === Some(Pos.F5)
    }
    "decrement letter only when moving to the left" in {
      Pos.E5.left === Some(Pos.D5)
    }
    "increment number and letter when moving upRight" in {
      Pos.E5.upRight === Some(Pos.F6)
    }
    "increment number only when moving upLeft" in {
      Pos.E5.upLeft === Some(Pos.E6)
    }
    "decrement number and letter when moving downLeft" in {
      Pos.E5.downLeft === Some(Pos.D4)
    }
    "decrement number only when moving downRight" in {
      Pos.E5.downRight === Some(Pos.E4)
    }
  }

  "directions from A1" should {
    "prevent moving outside the grid (left and down)" in {
      Pos.A1.left === None
      Pos.A1.downLeft == None
      Pos.A1.downRight == None
    }

    "allow moving inside the grid (right and up)" in {
      Pos.A1.right === Some(Pos.B1)
      Pos.A1.upLeft === Some(Pos.A2)
      Pos.A1.upRight === Some(Pos.B2)
      Pos.A1.directionString(Pos.A2) === DiagonalDirectionString.UpLeft
      Pos.A1.directionString(Pos.B2) === DiagonalDirectionString.UpRight
      Pos.A1.directionString(Pos.B1) === DirectionString.Right
    }
  }

  "directions from I9" should {
    "prevent moving outside the grid (right and up)" in {
      Pos.I9.right === None
      Pos.I9.upLeft === None
      Pos.I9.upRight === None
    }

    "allow moving inside the grid (left and down)" in {
      Pos.I9.left === Some(Pos.H9)
      Pos.I9.downLeft === Some(Pos.H8)
      Pos.I9.downRight === Some(Pos.I8)
      Pos.I9.directionString(Pos.I8) === DiagonalDirectionString.DownRight
      Pos.I9.directionString(Pos.H9) === DirectionString.Left
      Pos.I9.directionString(Pos.H8) === DiagonalDirectionString.DownLeft
    }
  }

  "directions from C7" should {
    "prevent moving outside the grid (upLeft and left)" in {
      Pos.C7.left === None
      Pos.C7.upLeft === None
    }

    "allow moving inside the grid (right, upRight and down)" in {
      Pos.C7.downLeft === Some(Pos.B6)
      Pos.C7.right === Some(Pos.D7)
      Pos.C7.downRight === Some(Pos.C6)
      Pos.C7.upRight === Some(Pos.D8)
    }
  }

  "directions from G7" should {
    "allow moving everywhere inside the grid" in {
      Pos.G7.downLeft === Some(Pos.F6)
      Pos.G7.right === Some(Pos.H7)
      Pos.G7.downRight === Some(Pos.G6)
      Pos.G7.upRight === Some(Pos.H8)
      Pos.G7.left === Some(Pos.F7)
      Pos.G7.upLeft === Some(Pos.G8)
    }
  }

// 9 -              &  \' (  )  *
// 8 -            7  8  9  !  ?  ¥
// 7 -          Y  Z  0  1  2  3  £
// 6 -        P  Q  R  S  T  U  V  ¡
// 5 -      G  H  I  J  K  L  M  N  }
// 4 -       y  z  A  B  C  D  E  F
// 3 -        q  r  s  t  u  v  w
// 2 -         i  j  k  l  m  n
// 1 -          a  b  c  d  e
//               \  \  \  \  \  \  \  \  \
//                A  B  C  D  E  F  G  H  I
  "official notation" should {
    "swap file and rank indexes" in {
      Pos.G6.officialNotationKey === "f7"
    }
  }

}
