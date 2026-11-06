package strategygames.yonkoma

import strategygames.Piotr

// lila decodes piotr characters with one table shared by every game, keyed by square name.
// Yonkoma takes its characters from that table, so every square reads the same in lila as
// the same square of any other game, and a square can never move.
class YonkomaPiotrTest extends YonkomaTest {

  "the piotr characters" should {

    "be the shared table's character for each square" in {
      Pos.all.map(p => p.key -> p.piotr) === Pos.all.map(p => p.key -> Piotr.lookup(p.key))
      Pos.all.size === 144
    }

    "never use a comma or a semicolon, which lila's analysis storage reserves" in {
      Pos.all.map(_.piotr).toSet.intersect(Set(',', ';')) must beEmpty
    }

    "be distinct and read back to the same square" in {
      Pos.all.map(_.piotr).distinct.size === Pos.all.size
      Pos.all.forall(p => Pos.piotr(p.piotr) == Some(p)) must beTrue
    }
  }
}
