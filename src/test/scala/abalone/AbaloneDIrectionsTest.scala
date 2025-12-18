package strategygames.abalone

import org.specs2.matcher.ValidatedMatchers

class AbaloneDirectionsTest extends AbaloneTest with ValidatedMatchers {
  "side moves of 3 marbles from E5" should {
    "in case of downLeft direction, generate up to 3 distinct directions to apply on marbles" in {
      /*
            _ _ _ _ _   _ _ _
           _ _ _ _ _ _   _ _
          _ _ _ _ _ _ _   _
         _ _ _ _ _ _ _ _
        _ _ a s s _ _ _ _
         _ A _ s s _ _ _
          _ B b _ c _ _   _
           _ _ C D _ _   _ _
            _ _ _ _ _   _ _ _
       */
      val a = Pos.C5
      val b = Pos.C3
      val c = Pos.E3
      val A = Pos.B4
      val B = Pos.B3
      val C = Pos.C2
      val D = Pos.D2
      Pos.E5.directionString(A) === DiagonalDirectionString.DownLeft
      Pos.E5.directionString(B) === DiagonalDirectionString.DownLeft
      Pos.E5.directionString(C) === DiagonalDirectionString.DownLeft
      Pos.E5.directionString(D) === DiagonalDirectionString.DownLeft
      a.downLeft.get.index === A.index
      b.left.get.index === B.index
      b.downRight.get.index === C.index
      c.downLeft.get.index === D.index
    }

    "in case of downRight direction, generate up to 2 distinct directions to apply on marbles" in {
      /*
            _ _ _ _ _   _ _ _
           _ _ _ _ _ _   _ _
          _ _ _ _ _ _ _   _
         _ _ _ _ _ _ _ _
        _ _ _ _ s s b _ _
         _ _ _ _ s _ B _
          _ _ _ _ a A _   _
           _ _ _ _ _ _   _ _
            _ _ _ _ _   _ _ _
       */
      val a = Pos.E3
      val b = Pos.G5
      val A = Pos.F3
      val B = Pos.G4
      Pos.E5.directionString(A) === DiagonalDirectionString.DownRight
      Pos.E5.directionString(B) === DiagonalDirectionString.DownRight
      a.right.get.index === A.index
      b.downRight.get.index === B.index
    }

    "in case of upRight direction, generate up to 3 distinct directions to apply on marbles" in {
      /*
            _ _ _ _ _   _ _ _
           _ _ D C _ _   _ _
          _ _ c _ b B _   _
         _ _ _ s s _ A _
        _ _ _ _ s s a _ _
         _ _ _ _ _ _ _ _
          _ _ _ _ _ _ _   _
           _ _ _ _ _ _   _ _
            _ _ _ _ _   _ _ _
       */
      val a = Pos.G5
      val b = Pos.G7
      val c = Pos.E7
      val A = Pos.H6
      val B = Pos.H7
      val C = Pos.G8
      val D = Pos.F8
      Pos.E5.directionString(A) === DiagonalDirectionString.UpRight
      Pos.E5.directionString(B) === DiagonalDirectionString.UpRight
      Pos.E5.directionString(C) === DiagonalDirectionString.UpRight
      Pos.E5.directionString(D) === DiagonalDirectionString.UpRight
      a.upRight.get.index === A.index
      b.right.get.index === B.index
      b.upLeft.get.index === C.index
      c.upRight.get.index === D.index
    }

    "in case of upLeft direction, generate up to 2 distinct directions to apply on marbles" in {
      /*
            _ _ _ _ _   _ _ _
           _ _ _ _ _ _   _ _
          _ B b _ _ _ _   _
         _ A _ s _ _ _ _
        _ _ a s s _ _ _ _
         _ _ _ _ _ _ _ _
          _ _ _ _ _ _ _   _
           _ _ _ _ _ _   _ _
            _ _ _ _ _   _ _ _
       */
      val a = Pos.C5
      val b = Pos.E7
      val A = Pos.C6
      val B = Pos.D7
      Pos.E5.directionString(A) === DiagonalDirectionString.UpLeft
      Pos.E5.directionString(B) === DiagonalDirectionString.UpLeft
      a.upLeft.get.index === A.index
      b.left.get.index === B.index
    }
  }
}
