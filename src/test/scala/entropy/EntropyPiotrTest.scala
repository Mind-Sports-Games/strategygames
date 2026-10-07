package strategygames.entropy

import org.specs2.mutable.Specification

// lila decodes dests with one piotr table shared by every game, laid out on an 8-file board
// (ui/stratutils/src/piotr.ts). A square must encode to the character that table gives its key.
class EntropyPiotrTest extends Specification {

  private val sharedTable = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

  private def sharedPiotr(pos: Pos): Char = sharedTable(pos.rank.index * 8 + pos.file.index)

  "piotr" should {

    "match lila's shared table for every square" in {
      Pos.all.map(p => p.key -> p.piotr) === Pos.all.map(p => p.key -> sharedPiotr(p))
    }

    "skip the h-file characters, so a2 follows g1 as i" in {
      Pos.A2.piotr === 'i'
      Pos.G7.piotr === '2'
    }

    "decode back to the same square" in {
      Pos.all.forall(p => Pos.piotr(p.piotr).contains(p)) must beTrue
    }
  }
}
