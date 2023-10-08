package strategygames.chess
import scala.language.reflectiveCalls

class SituationTest extends ChessTest {

  "a game" should {
    "detect check" should {
      "by rook" in {
        ("""
K  r
""" as P1).check must beTrue
      }
      "by knight" in {
        ("""
  n
K
""" as P1).check must beTrue
      }
      "by bishop" in {
        ("""
  b

   
     K
""" as P1).check must beTrue
      }
      "by pawn" in {
        ("""
    p
     K
""" as P1).check must beTrue
      }
      "not" in {
        ("""
   n
K
""" as P1).check must beFalse
      }
    }
    "detect check mate" in {
      "by rook" in {
        ("""
PP
K  r
""" as P1).checkMate must beTrue
      }
      "by knight" in {
        ("""
PPn
KR
""" as P1).checkMate must beTrue
      }
      "not" in {
        ("""
  n
K
""" as P1).checkMate must beFalse
      }
    }
    "stale mate" in {
      "stuck in a corner" in {
        ("""
prr
K
""" as P1).staleMate must beTrue
      }
      "not" in {
        ("""
  b
K
""" as P1).staleMate must beFalse
      }
    }

    "Give the correct winner for a game" in {
      val game =
        """
PP
K  r
""" as P1

      game.checkMate must beTrue
      game.winner must beSome.like { case player =>
        player == P2
      }
    }

    "Not give a winner if the game is still in progress" in {
      val game = """
    p
     K
    """ as P1

      game.winner must beNone

    }

    "not be playable" in {
      "with touching kings" in {
        val game = "kK BN" as P2
        game.playable(true) must beFalse
        game.playable(false) must beFalse
      }

      "with other side in check" in {
        val game = "k Q K" as P1
        game.playable(true) must beFalse
        game.playable(false) must beFalse
      }
    }

  }
}
