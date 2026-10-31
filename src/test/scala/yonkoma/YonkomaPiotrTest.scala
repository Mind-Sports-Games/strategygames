package strategygames.yonkoma

// lila decodes piotr characters with one table shared by every game
// (ui/stratutils/src/piotr.ts). A square must carry the character that table gives its key,
// not its own number in the yonkoma square order, or moves land on the wrong squares.
class YonkomaPiotrTest extends YonkomaTest {

  // every entry of lila's table that is a yonkoma square, key -> character
  val lilaTable: Map[String, Char] = Map(
    "a1"  -> 'a',
    "b1"  -> 'b',
    "c1"  -> 'c',
    "d1"  -> 'd',
    "e1"  -> 'e',
    "f1"  -> 'f',
    "g1"  -> 'g',
    "h1"  -> 'h',
    "i1"  -> '[',
    "j1"  -> '\\',
    "k1"  -> '©',
    "l1"  -> 'ª',
    "a2"  -> 'i',
    "b2"  -> 'j',
    "c2"  -> 'k',
    "d2"  -> 'l',
    "e2"  -> 'm',
    "f2"  -> 'n',
    "g2"  -> 'o',
    "h2"  -> 'p',
    "i2"  -> ']',
    "j2"  -> '^',
    "k2"  -> '²',
    "l2"  -> '³',
    "a3"  -> 'q',
    "b3"  -> 'r',
    "c3"  -> 's',
    "d3"  -> 't',
    "e3"  -> 'u',
    "f3"  -> 'v',
    "g3"  -> 'w',
    "h3"  -> 'x',
    "i3"  -> '_',
    "j3"  -> '`',
    "a4"  -> 'y',
    "b4"  -> 'z',
    "c4"  -> 'A',
    "d4"  -> 'B',
    "e4"  -> 'C',
    "f4"  -> 'D',
    "g4"  -> 'E',
    "h4"  -> 'F',
    "i4"  -> '{',
    "j4"  -> '|',
    "a5"  -> 'G',
    "b5"  -> 'H',
    "c5"  -> 'I',
    "d5"  -> 'J',
    "e5"  -> 'K',
    "f5"  -> 'L',
    "g5"  -> 'M',
    "h5"  -> 'N',
    "i5"  -> '}',
    "j5"  -> '~',
    "a6"  -> 'O',
    "b6"  -> 'P',
    "c6"  -> 'Q',
    "d6"  -> 'R',
    "e6"  -> 'S',
    "f6"  -> 'T',
    "g6"  -> 'U',
    "h6"  -> 'V',
    "i6"  -> '¡',
    "j6"  -> '¢',
    "a7"  -> 'W',
    "b7"  -> 'X',
    "c7"  -> 'Y',
    "d7"  -> 'Z',
    "e7"  -> '0',
    "f7"  -> '1',
    "g7"  -> '2',
    "h7"  -> '3',
    "i7"  -> '£',
    "j7"  -> '¤',
    "a8"  -> '4',
    "b8"  -> '5',
    "c8"  -> '6',
    "d8"  -> '7',
    "e8"  -> '8',
    "f8"  -> '9',
    "g8"  -> '!',
    "h8"  -> '?',
    "i8"  -> '¥',
    "j8"  -> '¦',
    "a9"  -> '"',
    "b9"  -> '#',
    "c9"  -> '$',
    "d9"  -> '%',
    "e9"  -> '&',
    "f9"  -> '\'',
    "g9"  -> '(',
    "h9"  -> ')',
    "i9"  -> '*',
    "j9"  -> '+',
    "a10" -> '§',
    "b10" -> '-',
    "c10" -> '.',
    "d10" -> '/',
    "e10" -> ':',
    "f10" -> '¨',
    "g10" -> '<',
    "h10" -> '=',
    "i10" -> '>',
    "j10" -> '@'
  )

  "the piotr characters" should {

    "be the ones lila's shared table gives each key" in {
      lilaTable.size === 104
      lilaTable.map { case (key, _) => key -> pos(key).piotr } === lilaTable
    }

    "match the go package for the squares lila's table does not have" in {
      val others = Pos.all.filterNot(p => lilaTable.contains(p.key))
      others.size === 40
      others.map(p => p.key -> Option(p.piotr)) ===
        others.map(p => p.key -> strategygames.go.Pos.fromKey(p.key).map(_.piotr))
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
