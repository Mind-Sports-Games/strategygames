package strategygames.abalone

import org.specs2.matcher.ValidatedMatchers

class AbalonePosTest extends AbaloneTest with ValidatedMatchers {
    "grid coordinates" should {
        "describe 61 positions" in {
            Pos.all.size must_== 61
        }

        "be shaped as an hexagon when accessed in 1D" in {
            Pos.isInHexagon(-1) must_== false

            Pos.isInHexagon(0) must_== true
            Pos.isInHexagon(1) must_== true
            Pos.isInHexagon(4) must_== true
            Pos.isInHexagon(5) must_== false
            Pos.isInHexagon(6) must_== false
            Pos.isInHexagon(7) must_== false
            Pos.isInHexagon(8) must_== false

            Pos.isInHexagon(9) must_== true
            Pos.isInHexagon(14) must_== true
            Pos.isInHexagon(15) must_== false
            Pos.isInHexagon(16) must_== false
            Pos.isInHexagon(17) must_== false

            Pos.isInHexagon(18) must_== true
            Pos.isInHexagon(25) must_== false
            Pos.isInHexagon(26) must_== false

            Pos.isInHexagon(35) must_== false
            Pos.isInHexagon(36) must_== true
            Pos.isInHexagon(44) must_== true

            Pos.isInHexagon(45) must_== false
            Pos.isInHexagon(53) must_== true

            Pos.isInHexagon(54) must_== false
            Pos.isInHexagon(55) must_== false
            Pos.isInHexagon(60) must_== true

            Pos.isInHexagon(63) must_== false
            Pos.isInHexagon(54) must_== false
            Pos.isInHexagon(65) must_== false
            Pos.isInHexagon(71) must_== true

            Pos.isInHexagon(72) must_== false
            Pos.isInHexagon(73) must_== false
            Pos.isInHexagon(74) must_== false
            Pos.isInHexagon(75) must_== false
            Pos.isInHexagon(80) must_== true

            Pos.isInHexagon(81) must_== false
            Pos.isInHexagon(88) must_== false

            Pos.isInHexagon(9001) must_== false
        }

        "be shaped as an hexagon when accessed in 2D" in {
            // testing first row
            Pos(File(0).get, Rank(0).get) must_!= None
            Pos(File(1).get, Rank(0).get) must_!= None
            Pos(File(2).get, Rank(0).get) must_!= None
            Pos(File(3).get, Rank(0).get) must_!= None
            Pos(File(4).get, Rank(0).get) must_!= None
            Pos(File(5).get, Rank(0).get) must_== None
            Pos(File(6).get, Rank(0).get) must_== None
            Pos(File(7).get, Rank(0).get) must_== None
            Pos(File(8).get, Rank(0).get) must_== None

            // testing last row
            Pos(File(0).get, Rank(8).get) must_== None
            Pos(File(1).get, Rank(8).get) must_== None
            Pos(File(2).get, Rank(8).get) must_== None
            Pos(File(3).get, Rank(8).get) must_== None
            Pos(File(4).get, Rank(8).get) must_!= None
            Pos(File(5).get, Rank(8).get) must_!= None
            Pos(File(6).get, Rank(8).get) must_!= None
            Pos(File(7).get, Rank(8).get) must_!= None
            Pos(File(8).get, Rank(8).get) must_!= None
        }

        "compute its index based on a shape of square when accessed in 2D" in {
            Pos(File(0).get, Rank(0).get) must_== Pos(0)
            Pos(File(8).get, Rank(0).get) must_== Pos(8)

            Pos(File(0).get, Rank(1).get) must_== Pos(9)

            Pos(File(8).get, Rank(8).get) must_== Pos(80)
        }
    }

    "general navigation system (from E5)" should {
        "increment letter only when moving to the right" in {
            Pos.E5.right must_== Some(Pos.F5)
        }
        "decrement letter only when moving to the left" in {
            Pos.E5.left must_== Some(Pos.D5)
        }
        "increment number and letter when moving upRight" in {
            Pos.E5.upRight must_== Some(Pos.F6)
        }
        "increment number only when moving upLeft" in {
            Pos.E5.upLeft must_== Some(Pos.E6)
        }
        "decrement number and letter when moving downLeft" in {
            Pos.E5.downLeft must_== Some(Pos.D4)
        }
        "decrement number only when moving downRight" in {
            Pos.E5.downRight must_== Some(Pos.E4)
        }        
    }

    "directions from A1" should {
        "prevent moving outside the grid (left and down)" in {
            Pos.A1.left must_== None
            Pos.A1.downLeft == None
            Pos.A1.downRight == None
        }

        "allow moving inside the grid (right and up)" in {
            Pos.A1.right must_== Some(Pos.B1)
            Pos.A1.upLeft must_== Some(Pos.A2)
            Pos.A1.upRight must_== Some(Pos.B2)
            Pos.A1.directionString(Pos.A2) must_== "upLeft"
            Pos.A1.directionString(Pos.B2) must_== "upRight"
            Pos.A1.directionString(Pos.B1) must_== "right"
        }
    }

    "directions from I9" should {
        "prevent moving outside the grid (right and up)" in {
            Pos.I9.right must_== None
            Pos.I9.upLeft must_== None
            Pos.I9.upRight must_== None
        }

        "allow moving inside the grid (left and down)" in {
            Pos.I9.left must_== Some(Pos.H9)
            Pos.I9.downLeft must_== Some(Pos.H8)
            Pos.I9.downRight must_== Some(Pos.I8)
            Pos.I9.directionString(Pos.I8) must_== "downRight"
            Pos.I9.directionString(Pos.H9) must_== "left"
            Pos.I9.directionString(Pos.H8) must_== "downLeft"
        }
    }

    "directions from C7" should {
        "prevent moving outside the grid (upLeft and left)" in {
            Pos.C7.left must_== None
            Pos.C7.upLeft must_== None
        }

        "allow moving inside the grid (right, upRight and down)" in {
            Pos.C7.downLeft must_== Some(Pos.B6)
            Pos.C7.right must_== Some(Pos.D7)
            Pos.C7.downRight must_== Some(Pos.C6)
            Pos.C7.upRight must_== Some(Pos.D8)
        }
    }

    "directions from G7" should {
        "allow moving everywhere inside the grid" in {
            Pos.G7.downLeft must_== Some(Pos.F6)
            Pos.G7.right must_== Some(Pos.H7)
            Pos.G7.downRight must_== Some(Pos.G6)
            Pos.G7.upRight must_== Some(Pos.H8)
            Pos.G7.left must_== Some(Pos.F7)
            Pos.G7.upLeft must_== Some(Pos.G8)
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
            Pos.G6.officialNotationKey must_== "f7"
        }
    }

}