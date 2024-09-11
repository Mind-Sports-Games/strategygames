package strategygames.abalone

import org.specs2.matcher.ValidatedMatchers

class AbalonePosTest extends AbaloneTest with ValidatedMatchers {
    "grid coordinates" should {
        "describe 61 positions" in {
            Pos.allPiotrs.size must_== 61
        }
    }

    "general navigation system (from E5)" should {
        "increment letter only when moving to the right" in {
            Pos.E5.right must_== Some(Pos.F5)
        }
        "decrement letter only when moving to the left" in {
            Pos.E5.right must_== Some(Pos.F5)
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
}