package strategygames.fiveinarow

import org.specs2.mutable.Specification

class FiveInARowPiotrTest extends Specification {

  "piotr" should {

    "match the character lila's shared table gives each square's key" in {
      Pos.all.map(p => p.key -> p.piotr) ===
        Pos.all.map(p => p.key -> strategygames.go.Pos.fromKey(p.key).get.piotr)
    }

    "follow the 8-file layout on the first ranks" in {
      Pos.fromKey("a1").get.piotr === 'a'
      Pos.fromKey("a2").get.piotr === 'i'
    }

    "give every square its own character, and decode back to it" in {
      Pos.all.map(_.piotr).distinct.size === Pos.allSize
      Pos.all.forall(p => Pos.piotr(p.piotr).contains(p)) must beTrue
    }
  }
}
