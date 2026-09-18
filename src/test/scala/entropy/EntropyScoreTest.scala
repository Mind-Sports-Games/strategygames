package strategygames.entropy

class EntropyScoreTest extends EntropyTest {

  "a run with no gaps" should {
    "score an adjacent matching pair" in {
      GuaranteedScore.scoreLine(line("rr")) === 2
    }
    "not score a single counter" in {
      GuaranteedScore.scoreLine(line("r")) === 0
    }
    "not score a mismatched pair" in {
      GuaranteedScore.scoreLine(line("rg")) === 0
    }
    "score a three-run" in {
      GuaranteedScore.scoreLine(line("rgr")) === 3
    }
    "score every nested and overlapping run" in {
      // rgbgr = 5, gbg = 3
      GuaranteedScore.scoreLine(line("rgbgr")) === 8
      // bbb = 3, and the two adjacent pairs = 2 each
      GuaranteedScore.scoreLine(line("bbb")) === 7
    }
  }

  "a run with one empty square at the dead centre" should {
    "score, because any colour there still reads the same both ways" in {
      GuaranteedScore.scoreLine(line("r.r")) === 3
    }
    "score the outer run and the nested one" in {
      // rg.gr = 5, g.g = 3
      GuaranteedScore.scoreLine(line("rg.gr")) === 8
    }
  }

  "a run whose gap is not a free centre" should {
    "not score when the length is even" in {
      GuaranteedScore.scoreLine(line("r.gr")) === 0
      GuaranteedScore.scoreLine(line(".r")) === 0
    }
    "not score when there is more than one gap" in {
      GuaranteedScore.scoreLine(line("r..gr")) === 0
    }
    "not score when the gap is off centre" in {
      GuaranteedScore.scoreLine(line("rg.rg")) === 0
    }
  }

  "a gap between two runs" should {
    "break them apart" in {
      // the gap separates rg from bb, so only bb scores
      GuaranteedScore.scoreLine(line("rg.bb")) === 2
    }
  }

  "an empty line" should {
    "score nothing" in {
      GuaranteedScore.scoreLine(line(".......")) === 0
    }
  }

  "an empty board" should {
    "score nothing" in {
      Board.init(variant.Entropy).guaranteedScore === 0
    }
  }

  "a board with one row of a single colour" should {
    "score that row in both directions" in {
      val board = Board(format.FEN("7/7/7/7/7/7/RRRRRRR[] w 0 0 1 1").pieces, variant.Entropy)
      // every sub-run of length 2..7 of one colour is a palindrome:
      // 12 + 15 + 16 + 15 + 12 + 7 = 77. The columns hold one counter each and score nothing.
      board.guaranteedScore === 77
    }
  }
}
