package strategygames.fairysf

import org.specs2.matcher.ValidatedMatchers
import org.specs2.mutable.Specification

import strategygames.fairysf.{ File, Pos, Rank }

class FairyPosTest extends Specification with ValidatedMatchers {

  "Pos all size" should {
    "match all files and ranks" in {
      Pos.all.size === File.all.size * Rank.all.size
    }
  }

  "Pos less than 90" should {
    "fit in 9 files, 10 ranks" in {
      val file = File.C
      val rank = Rank.Fifth
      Pos(file.index + File.formerAll.size * rank.index) === Some(Pos(file, rank))
    }
  }

  "Pos 90 or more" should {
    "only map to file J" in {
      val file = File.J
      val rank = Rank.Fifth
      Pos(rank.index + File.formerAll.size * Rank.all.size) === Some(Pos(file, rank))
    }
  }

  "largest Pos" should {
    "be J10" in {
      Pos(File.all.size * Rank.all.size - 1) === Some(Pos.J10)
    }
    "be J10" in {
      Pos(File.all.last, Rank.all.last) === Pos.J10
    }
  }

  "smallest Pos" should {
    "be A1" in {
      Pos(0) === Some(Pos.A1)
    }
    "be A1" in {
      Pos(File.all(0), Rank.all(0)) === Pos.A1
    }
  }

}
