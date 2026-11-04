package strategygames

import org.specs2.mutable.Specification
import org.specs2.specification.core.Fragment

class PiotrTest extends Specification {

  private def keyed(all: List[(String, Char)]): List[(String, Option[Char])] =
    all.map { case (key, piotr) => key -> Some(piotr) }

  private def shared(all: List[(String, Char)]): List[(String, Option[Char])] =
    all.map { case (key, _) => key -> Piotr.lookup.get(key) }

  // code points of every square up to 19x19, one rank per row from rank 1, files a to s
  private val snapshotRanks: List[List[Int]] = List(
    List(97, 98, 99, 100, 101, 102, 103, 104, 91, 92, 169, 170, 171, 172, 173, 174, 175, 176, 177),
    List(105, 106, 107, 108, 109, 110, 111, 112, 93, 94, 178, 179, 180, 181, 182, 183, 184, 185, 187),
    List(113, 114, 115, 116, 117, 118, 119, 120, 95, 96, 188, 189, 190, 191, 192, 193, 194, 195, 196),
    List(121, 122, 65, 66, 67, 68, 69, 70, 123, 124, 197, 198, 199, 200, 201, 202, 203, 204, 205),
    List(71, 72, 73, 74, 75, 76, 77, 78, 125, 126, 206, 207, 208, 209, 210, 211, 212, 213, 214),
    List(79, 80, 81, 82, 83, 84, 85, 86, 161, 162, 215, 216, 217, 218, 219, 220, 221, 222, 223),
    List(87, 88, 89, 90, 48, 49, 50, 51, 163, 164, 224, 225, 226, 227, 228, 229, 230, 231, 232),
    List(52, 53, 54, 55, 56, 57, 33, 63, 165, 166, 233, 234, 235, 236, 237, 238, 239, 240, 241),
    List(34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 242, 243, 244, 245, 246, 247, 248, 249, 250),
    List(167, 45, 46, 47, 58, 168, 60, 61, 62, 64, 251, 252, 253, 254, 255, 256, 257, 258, 259),
    List(260, 261, 262, 263, 264, 265, 266, 267, 268, 269, 270, 271, 272, 273, 274, 275, 276, 277, 278),
    List(279, 280, 281, 282, 283, 284, 285, 286, 287, 288, 289, 290, 291, 292, 293, 294, 295, 296, 297),
    List(298, 299, 300, 301, 302, 303, 304, 305, 306, 307, 308, 309, 310, 311, 312, 313, 314, 315, 316),
    List(317, 318, 319, 320, 321, 322, 323, 324, 325, 326, 327, 328, 330, 331, 332, 333, 334, 335, 336),
    List(337, 338, 339, 340, 341, 342, 343, 344, 345, 346, 347, 348, 349, 350, 351, 352, 353, 354, 355),
    List(356, 357, 358, 359, 360, 361, 362, 363, 364, 365, 366, 367, 368, 369, 370, 371, 372, 373, 374),
    List(375, 376, 377, 378, 379, 380, 381, 382, 383, 384, 385, 386, 387, 388, 389, 390, 391, 392, 393),
    List(394, 395, 396, 397, 398, 399, 400, 401, 402, 403, 404, 405, 406, 407, 408, 409, 410, 411, 412),
    List(413, 414, 415, 416, 417, 418, 419, 420, 421, 422, 423, 424, 425, 426, 427, 428, 429, 430, 431)
  )

  private val snapshot: Map[String, Char] =
    snapshotRanks.zipWithIndex.flatMap { case (codePoints, rank) =>
      codePoints.zip('a' to 's').map { case (codePoint, file) => s"$file${rank + 1}" -> codePoint.toChar }
    }.toMap

  private val games: List[(String, List[(String, Char)])] = List(
    "chess"        -> chess.Pos.all.map(p => p.key -> p.piotr),
    "fairysf"      -> fairysf.Pos.all.map(p => p.key -> p.piotr),
    "samurai"      -> samurai.Pos.all.map(p => p.key -> p.piotr),
    "togyzkumalak" -> togyzkumalak.Pos.all.map(p => p.key -> p.piotr),
    "go"           -> go.Pos.all.map(p => p.key -> p.piotr),
    "backgammon"   -> backgammon.Pos.all.map(p => p.key -> p.piotr),
    "dameo"        -> dameo.Pos.all.map(p => p.key -> p.piotr),
    "entropy"      -> entropy.Pos.all.map(p => p.key -> p.piotr),
    "fiveinarow"   -> fiveinarow.Pos.all.map(p => p.key -> p.piotr)
  )

  "the shared piotr lookup" should {
    "give each square name one char" in {
      Piotr.lookup.values.toSet.size === Piotr.lookup.size
    }
    // a snapshot of the chars lila already decodes, so an edit to the lookup itself is caught
    "keep the characters lila decodes" in {
      Piotr.lookup === snapshot
    }
    "refuse a square missing from the lookup" in {
      Piotr.table(List("a1", "t20")) must throwA[RuntimeException]
    }
    Fragment.foreach(games) { case (name, all) =>
      s"encode every $name square as $name always has" in {
        shared(all) === keyed(all)
      }
    }
  }
}
