package strategygames.draughts
package opening

import format.FEN

import cats.syntax.option._

object DrawTablesIDF {

  import StartingPosition.Category

  val categoriesIDF = List(
    Category(
      "1",
      List(
        StartingPosition(
          "1-I",
          FEN("B:W17,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,12"),
          "1. ab4",
          "ab4".some
        ),
        StartingPosition(
          "1-II",
          FEN("B:W13,21,22,23,24,25,26,27,28,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,12"),
          "",
          "a1-a5".some
        ),
        StartingPosition(
          "1-III",
          FEN("W:W17,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,14"),
          "1. ab4 bc5",
          "ab4 bc5".some
        ),
        StartingPosition(
          "1-IV",
          FEN("W:W15,21,22,23,24,25,26,27,28,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,16"),
          "",
          "a1-e5 d6-g5".some
        ),
        StartingPosition(
          "1-V",
          FEN("B:W17,20,22,23,24,25,26,28,29,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,15"),
          "1. ab4 fe5 2. gh4 gf6 3. fg3",
          "ab4 fe5 gh4 gf6 fg3".some
        ),
        StartingPosition(
          "1-VI",
          FEN("W:W14,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,21"),
          "",
          "a3-c5 d6-a3".some
        ),
        StartingPosition(
          "1-VII",
          FEN("W:W17,21,22,23,24,25,27,28,29,30,31,32:B1,2,3,4,6,7,8,9,10,11,12,14"),
          "1. cb4 bc5 2. dc3 ab6",
          "cb4 bc5 dc3 ab6".some
        ),
        StartingPosition(
          "1-VIII",
          FEN("W:W16,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,14"),
          "",
          "a3-g5 d6-c5".some
        ),
        StartingPosition(
          "1-IX",
          FEN("W:W17,21,22,23,24,25,27,28,29,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,15"),
          "1. cb4 fe5 2. dc3 gf6",
          "cb4 fe5 dc3 gf6".some
        ),
        StartingPosition(
          "1-X",
          FEN("W:W21,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,6,7,8,9,10,11,12,14"),
          "",
          "--- a7-c5".some
        ),
        StartingPosition(
          "1-XI",
          FEN("B:W18,19,21,22,23,26,27,28,29,30,31,32:B1,2,3,4,6,7,8,9,10,11,12,13"),
          "1. cd4 ba5 2. bc3 ab6 3. gf4",
          "cd4 ba5 bc3 ab6 gf4".some
        ),
        StartingPosition(
          "1-XII",
          FEN("W:W16,21,22,23,24,26,27,28,29,30,31,32:B1,2,3,4,6,7,8,9,10,11,12,13"),
          "",
          "b2-g5 a7-a5".some
        ),
        StartingPosition(
          "1-XIII",
          FEN("B:W18,21,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,7,8,10,11,12,13"),
          "1. cd4 bc5 2. db6 ca5 3. ed4",
          "cd4 bc5 db6 ca5 ed4".some
        ),
        StartingPosition(
          "1-XIV",
          FEN("W:W17,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,6,7,8,9,10,11,12,15"),
          "",
          "c3-b4 a7-e5".some
        ),
        StartingPosition(
          "1-XV",
          FEN("W:W18,21,22,23,24,26,27,28,29,30,31,32:B1,2,3,4,5,6,8,9,10,11,12,16"),
          "1. cd4 fg5 2. bc3 ef6",
          "cd4 fg5 bc3 ef6".some
        ),
        StartingPosition(
          "1-XVI",
          FEN("W:W16,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,6,7,8,9,10,11,12,13"),
          "",
          "c3-g5 a7-a5".some
        ),
        StartingPosition(
          "1-XVII",
          FEN("W:W16,18,21,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,14,20"),
          "1. cd4 hg5 2. gf4 gh4 3. fg5 dc5",
          "cd4 hg5 gf4 gh4 fg5 dc5".some
        ),
        StartingPosition(
          "1-XVIII",
          FEN("W:W17,21,22,23,24,25,27,28,29,30,31,32:B1,2,3,4,6,7,8,9,10,11,12,16"),
          "",
          "d2-b4 a7-g5".some
        ),
        StartingPosition(
          "1-XIX",
          FEN("W:W17,19,21,24,25,27,28,29,30,31,32:B1,2,3,4,5,7,8,9,10,11,12"),
          "1. ed4 dc5 2. cb4 ce3 3. df4 cd6",
          "ed4 dc5 cb4 ce3 df4 cd6".some
        ),
        StartingPosition(
          "1-XX",
          FEN("W:W19,21,22,23,24,25,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,14"),
          "",
          "d2-f4 b6-c5".some
        ),
        StartingPosition(
          "1-XXI",
          FEN("W:W18,21,22,23,24,25,26,28,29,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,16"),
          "1. ed4 fg5 2. fe3 gf6",
          "ed4 fg5 fe3 gf6".some
        ),
        StartingPosition(
          "1-XXII",
          FEN("W:W17,21,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,6,7,8,9,10,11,12,14"),
          "",
          "e3-b4 a7-c5".some
        ),
        StartingPosition(
          "1-XXIII",
          FEN("B:W19,21,22,23,24,25,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,14"),
          "1. ef4 dc5 2. de3",
          "ef4 dc5 de3".some
        ),
        StartingPosition(
          "1-XXIV",
          FEN("W:W20,21,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,13"),
          "",
          "e3-h4 f6-a5".some
        ),
        StartingPosition(
          "1-XXV",
          FEN("B:W18,19,21,23,24,25,26,27,29,30,31,32:B1,2,3,4,5,6,7,8,10,12,14,15"),
          "1. gf4 bc5 2. hg3 fe5 3. cd4",
          "gf4 bc5 hg3 fe5 cd4".some
        ),
        StartingPosition(
          "1-XXVI",
          FEN("W:W18,21,22,23,24,25,26,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,16"),
          "",
          "f2-d4 h6-g5".some
        ),
        StartingPosition(
          "1-XXVII",
          FEN("W:W17,19,21,22,23,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,9,11,12,15,16"),
          "1. gf4 fg5 2. cb4 gf6 3. bc3 de5",
          "gf4 fg5 cb4 gf6 bc3 de5".some
        ),
        StartingPosition(
          "1-XXVIII",
          FEN("W:W13,21,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,12"),
          "",
          "g3-a5 ---".some
        ),
        StartingPosition(
          "1-XXIX",
          FEN("W:W20,21,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,15"),
          "1. gh4 de5",
          "gh4 de5".some
        ),
        StartingPosition(
          "1-XXX",
          FEN("W:W19,21,22,23,25,26,27,28,29,30,31,32:B1,2,3,5,6,7,8,9,10,11,12,16"),
          "",
          "g3-f4 h8-g5".some
        ),
        StartingPosition(
          "1-XXXI",
          FEN("W:W19,20,21,22,25,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,13"),
          "1. gh4 hg5 2. ef4 ge3 3. df4 ba5",
          "gh4 hg5 ef4 ge3 df4 ba5".some
        ),
        StartingPosition(
          "1-XXXII",
          FEN("W:W14,21,22,23,24,25,26,27,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,28"),
          "",
          "h2-c5 f6-h2".some
        ),
        StartingPosition(
          "1-XXXIII",
          FEN("W:W20,21,22,23,24,25,26,27,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,28"),
          "",
          "h2-h4 h6-h2".some
        )
      )
    ),
    Category(
      "2",
      List(
        StartingPosition(
          "2-I",
          FEN("W:W17,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,13"),
          "1. ab4 ba5",
          "ab4 ba5".some
        ),
        StartingPosition(
          "2-II",
          FEN("W:W13,21,22,23,24,25,26,27,28,30,31,32:B1,2,3,4,6,7,8,9,10,11,12,20"),
          "",
          "a1-a5 a7-h4".some
        ),
        StartingPosition(
          "2-III",
          FEN("W:W13,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,17"),
          "1. ab4 bc5 2. ba5 cb4",
          "ab4 bc5 ba5 cb4".some
        ),
        StartingPosition(
          "2-IV",
          FEN("W:W15,21,22,23,24,25,26,27,28,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,20"),
          "",
          "a1-e5 g7-h4".some
        ),
        StartingPosition(
          "2-V",
          FEN("W:W17,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,16"),
          "1. ab4 fg5",
          "ab4 fg5".some
        ),
        StartingPosition(
          "2-VI",
          FEN("W:W14,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,21"),
          "",
          "a3-c5 f6-a3".some
        ),
        StartingPosition(
          "2-VII",
          FEN("W:W17,21,22,23,24,25,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,12,14,15"),
          "1. cb4 bc5 2. dc3 fe5",
          "cb4 bc5 dc3 fe5".some
        ),
        StartingPosition(
          "2-VIII",
          FEN("W:W16,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,21"),
          "",
          "a3-g5 f6-a3".some
        ),
        StartingPosition(
          "2-IX",
          FEN("W:W17,19,21,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,12,13,15"),
          "1. cb4 fe5 2. ef4 ba5",
          "cb4 fe5 ef4 ba5".some
        ),
        StartingPosition(
          "2-X",
          FEN("W:W21,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,6,7,8,9,10,11,12,16"),
          "",
          "--- a7-g5".some
        ),
        StartingPosition(
          "2-XI",
          FEN("W:W18,21,22,23,24,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,11,12,13,15"),
          "1. cd4 ba5 2. bc3 de5",
          "cd4 ba5 bc3 de5".some
        ),
        StartingPosition(
          "2-XII",
          FEN("W:W16,21,22,23,24,26,27,28,29,30,31,32:B1,2,3,4,5,7,8,9,10,11,12,13"),
          "",
          "b2-g5 c7-a5".some
        ),
        StartingPosition(
          "2-XIII",
          FEN("W:W18,21,22,23,24,26,27,28,29,30,31,32:B1,2,3,4,5,7,8,9,10,11,12,14"),
          "1. cd4 dc5 2. bc3 cd6",
          "cd4 dc5 bc3 cd6".some
        ),
        StartingPosition(
          "2-XIV",
          FEN("W:W17,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,6,7,8,9,10,11,12,20"),
          "",
          "c3-b4 a7-h4".some
        ),
        StartingPosition(
          "2-XV",
          FEN("B:W18,21,22,23,24,25,26,27,28,30,31,32:B1,2,3,4,5,6,8,9,10,11,12,16"),
          "1. cd4 fg5 2. bc3 ef6 3. ab2",
          "cd4 fg5 bc3 ef6 ab2".some
        ),
        StartingPosition(
          "2-XVI",
          FEN("W:W16,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,6,7,8,9,10,11,12,15"),
          "",
          "c3-g5 a7-e5".some
        ),
        StartingPosition(
          "2-XVII",
          FEN("W:W16,18,21,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,15,20"),
          "1. cd4 hg5 2. gf4 gh4 3. fg5 de5",
          "cd4 hg5 gf4 gh4 fg5 de5".some
        ),
        StartingPosition(
          "2-XVIII",
          FEN("W:W17,21,22,23,24,25,27,28,29,30,31,32:B1,2,3,4,6,7,8,9,10,11,12,20"),
          "",
          "d2-b4 a7-h4".some
        ),
        StartingPosition(
          "2-XIX",
          FEN("W:W18,21,22,23,24,25,26,28,29,30,31,32:B1,2,3,4,5,7,8,9,10,11,12,14"),
          "1. ed4 dc5 2. fe3 cd6",
          "ed4 dc5 fe3 cd6".some
        ),
        StartingPosition(
          "2-XX",
          FEN("W:W19,21,22,23,24,25,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,13"),
          "",
          "d2-f4 d6-a5".some
        ),
        StartingPosition(
          "2-XXI",
          FEN("W:W18,20,21,22,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,12,13,16"),
          "1. ed4 fg5 2. gh4 ba5",
          "ed4 fg5 gh4 ba5".some
        ),
        StartingPosition(
          "2-XXII",
          FEN("W:W17,21,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,6,7,8,9,10,11,12,15"),
          "",
          "e3-b4 a7-e5".some
        ),
        StartingPosition(
          "2-XXIII",
          FEN("B:W19,21,22,23,24,25,26,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,14"),
          "1. ef4 dc5 2. fe3",
          "ef4 dc5 fe3".some
        ),
        StartingPosition(
          "2-XXIV",
          FEN("W:W20,21,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,16"),
          "",
          "e3-h4 g7-g5".some
        ),
        StartingPosition(
          "2-XXV",
          FEN("W:W19,21,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,14"),
          "1. gf4 dc5",
          "gf4 dc5".some
        ),
        StartingPosition(
          "2-XXVI",
          FEN("W:W19,21,22,23,24,25,26,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,15"),
          "",
          "f2-f4 d6-e5".some
        ),
        StartingPosition(
          "2-XXVII",
          FEN("B:W20,21,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,12"),
          "1. gh4",
          "gh4".some
        ),
        StartingPosition(
          "2-XXVIII",
          FEN("W:W13,21,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,6,7,8,9,10,11,12,15"),
          "",
          "g3-a5 a7-e5".some
        ),
        StartingPosition(
          "2-XXIX",
          FEN("W:W17,20,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,7,8,9,10,11,12,15"),
          "1. gh4 de5 2. ab4 cd6",
          "gh4 de5 ab4 cd6".some
        ),
        StartingPosition(
          "2-XXX",
          FEN("W:W16,21,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,6,7,8,9,10,11,12,20"),
          "",
          "g3-g5 a7-h4".some
        ),
        StartingPosition(
          "2-XXXI",
          FEN("W:W19,20,21,22,25,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,14"),
          "1. gh4 hg5 2. ef4 ge3 3. df4 bc5",
          "gh4 hg5 ef4 ge3 df4 bc5".some
        ),
        StartingPosition(
          "2-XXXII",
          FEN("W:W14,21,22,23,24,25,26,27,29,30,31,32:B1,2,3,5,6,7,8,9,10,11,12,28"),
          "",
          "h2-c5 h8-h2".some
        ),
        StartingPosition(
          "2-XXXIII",
          FEN("W:W20,21,22,23,24,25,26,27,29,30,31,32:B1,2,3,4,6,7,8,9,10,11,12,15"),
          "",
          "h2-h4 a7-e5".some
        )
      )
    ),
    Category(
      "3",
      List(
        StartingPosition(
          "3-I",
          FEN("W:W17,21,22,23,24,26,27,28,29,30,31,32:B1,2,3,4,6,7,8,9,10,11,12,13"),
          "1. ab4 ba5 2. ba3 ab6",
          "ab4 ba5 ba3 ab6".some
        ),
        StartingPosition(
          "3-II",
          FEN("W:W13,21,22,23,24,25,26,27,28,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,16"),
          "",
          "a1-a5 b6-g5".some
        ),
        StartingPosition(
          "3-III",
          FEN("W:W13,18,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,12,15,17"),
          "1. ab4 bc5 2. ba5 cb4 3. ed4 fe5",
          "ab4 bc5 ba5 cb4 ed4 fe5".some
        ),
        StartingPosition(
          "3-IV",
          FEN("W:W19,21,22,23,24,25,26,27,28,30,31,32:B1,2,3,4,6,7,8,9,10,11,12,14"),
          "",
          "a1-f4 a7-c5".some
        ),
        StartingPosition(
          "3-V",
          FEN("B:W13,18,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,16"),
          "1. ab4 fg5 2. ba5 gf6 3. ed4",
          "ab4 fg5 ba5 gf6 ed4".some
        ),
        StartingPosition(
          "3-VI",
          FEN("W:W14,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,21"),
          "",
          "a3-c5 g7-a3".some
        ),
        StartingPosition(
          "3-VII",
          FEN("W:W17,19,21,22,24,25,27,28,29,30,31,32:B1,2,3,4,5,6,8,10,11,12,14,15"),
          "1. cb4 bc5 2. dc3 fe5 3. ef4 ef6",
          "cb4 bc5 dc3 fe5 ef4 ef6".some
        ),
        StartingPosition(
          "3-VIII",
          FEN("W:W16,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,21"),
          "",
          "a3-g5 g7-a3".some
        ),
        StartingPosition(
          "3-IX",
          FEN("B:W14,19,21,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,12,13,15"),
          "1. cb4 fe5 2. ef4 ba5 3. bc5",
          "cb4 fe5 ef4 ba5 bc5".some
        ),
        StartingPosition(
          "3-X",
          FEN("W:W21,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,6,7,8,9,10,11,12,20"),
          "",
          "--- a7-h4".some
        ),
        StartingPosition(
          "3-XI",
          FEN("B:W18,19,21,22,24,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,11,12,13,15"),
          "1. cd4 ba5 2. bc3 de5 3. ef4",
          "cd4 ba5 bc3 de5 ef4".some
        ),
        StartingPosition(
          "3-XII",
          FEN("W:W16,21,22,23,24,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,15"),
          "",
          "b2-g5 d6-e5".some
        ),
        StartingPosition(
          "3-XIII",
          FEN("W:W18,21,22,23,24,25,26,27,28,30,31,32:B1,2,3,4,5,7,8,9,11,12,14,15"),
          "1. cd4 dc5 2. bc3 cd6 3. ab2 de5",
          "cd4 dc5 bc3 cd6 ab2 de5".some
        ),
        StartingPosition(
          "3-XIV",
          FEN("W:W17,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,5,6,7,8,9,10,11,12,16"),
          "",
          "c3-b4 h8-g5".some
        ),
        StartingPosition(
          "3-XV",
          FEN("W:W18,21,22,23,24,25,26,27,28,30,31,32:B1,2,3,4,5,6,7,9,11,12,15,16"),
          "1. cd4 fg5 2. bc3 gf6 3. ab2 de5",
          "cd4 fg5 bc3 gf6 ab2 de5".some
        ),
        StartingPosition(
          "3-XVI",
          FEN("W:W16,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,14"),
          "",
          "c3-g5 b6-c5".some
        ),
        StartingPosition(
          "3-XVII",
          FEN("W:W16,18,21,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,15,20"),
          "1. cd4 hg5 2. gf4 gh4 3. fg5 fe5",
          "cd4 hg5 gf4 gh4 fg5 fe5".some
        ),
        StartingPosition(
          "3-XVIII",
          FEN("W:W17,21,22,23,24,25,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,15"),
          "",
          "d2-b4 d6-e5".some
        ),
        StartingPosition(
          "3-XIX",
          FEN("W:W18,21,22,23,24,25,26,28,29,30,31,32:B1,2,3,4,5,6,8,9,10,11,12,14"),
          "1. ed4 dc5 2. fe3 ed6",
          "ed4 dc5 fe3 ed6".some
        ),
        StartingPosition(
          "3-XX",
          FEN("W:W19,21,22,23,24,25,27,28,29,30,31,32:B1,2,3,4,5,6,8,9,10,11,12,13"),
          "",
          "d2-f4 e7-a5".some
        ),
        StartingPosition(
          "3-XXI",
          FEN("W:W18,20,21,22,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,12,14,16"),
          "1. ed4 fg5 2. gh4 dc5",
          "ed4 fg5 gh4 dc5".some
        ),
        StartingPosition(
          "3-XXII",
          FEN("W:W17,21,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,6,7,8,9,10,11,12,20"),
          "",
          "e3-b4 a7-h4".some
        ),
        StartingPosition(
          "3-XXIII",
          FEN("W:W19,20,21,22,24,25,26,27,29,30,31,32:B1,2,3,4,5,6,8,9,10,12,14,15"),
          "1. ef4 dc5 2. gh4 ed6 3. hg3 fe5",
          "ef4 dc5 gh4 ed6 hg3 fe5".some
        ),
        StartingPosition(
          "3-XXIV",
          FEN("W:W20,21,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,13"),
          "",
          "e3-h4 h6-a5".some
        ),
        StartingPosition(
          "3-XXV",
          FEN("W:W17,19,21,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,11,12,13,14"),
          "1. gf4 dc5 2. cb4 ba5",
          "gf4 dc5 cb4 ba5".some
        ),
        StartingPosition(
          "3-XXVI",
          FEN("W:W19,21,22,23,24,25,26,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,15"),
          "",
          "f2-f4 f6-e5".some
        ),
        StartingPosition(
          "3-XXVII",
          FEN("W:W20,21,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,13"),
          "1. gh4 ba5",
          "gh4 ba5".some
        ),
        StartingPosition(
          "3-XXVIII",
          FEN("W:W13,21,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,15"),
          "",
          "g3-a5 b6-e5".some
        ),
        StartingPosition(
          "3-XXIX",
          FEN("B:W20,21,22,23,24,25,26,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,15"),
          "1. gh4 de5 2. fg3",
          "gh4 de5 fg3".some
        ),
        StartingPosition(
          "3-XXX",
          FEN("W:W16,21,22,23,25,26,27,28,29,30,31,32:B1,2,3,5,6,7,8,9,10,11,12,17"),
          "",
          "g3-g5 h8-b4".some
        ),
        StartingPosition(
          "3-XXXI",
          FEN("W:W20,21,22,23,24,25,26,27,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,15,16"),
          "1. gh4 hg5 2. hg3 de5",
          "gh4 hg5 hg3 de5".some
        ),
        StartingPosition(
          "3-XXXII",
          FEN("W:W18,21,22,23,24,25,26,27,29,30,31,32:B1,2,3,4,6,7,8,9,10,11,12,28"),
          "",
          "h2-d4 a7-h2".some
        ),
        StartingPosition(
          "3-XXXIII",
          FEN("W:W20,21,22,23,24,25,26,27,29,30,31,32:B1,2,3,4,6,7,8,9,10,11,12,28"),
          "",
          "h2-h4 a7-h2".some
        )
      )
    ),
    Category(
      "4",
      List(
        StartingPosition(
          "4-I",
          FEN("W:W17,21,22,23,24,25,26,27,28,30,31,32:B1,2,3,4,6,7,8,9,11,12,13,14"),
          "1. ab4 ba5 2. ba3 ab6 3. ab2 dc5",
          "ab4 ba5 ba3 ab6 ab2 dc5".some
        ),
        StartingPosition(
          "4-II",
          FEN("W:W13,21,22,23,24,25,26,27,28,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,20"),
          "",
          "a1-a5 b6-h4".some
        ),
        StartingPosition(
          "4-III",
          FEN("W:W13,18,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,12,16,17"),
          "1. ab4 bc5 2. ba5 cb4 3. ed4 fg5",
          "ab4 bc5 ba5 cb4 ed4 fg5".some
        ),
        StartingPosition(
          "4-IV",
          FEN("W:W19,21,22,23,24,25,26,27,28,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,14"),
          "",
          "a1-f4 b6-c5".some
        ),
        StartingPosition(
          "4-V",
          FEN("B:W17,18,21,22,24,26,27,28,29,30,31,32:B1,2,3,4,5,6,8,9,10,11,12,16"),
          "1. ab4 fg5 2. ba3 ef6 3. ed4",
          "ab4 fg5 ba3 ef6 ed4".some
        ),
        StartingPosition(
          "4-VI",
          FEN("W:W14,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,5,6,7,8,9,10,11,12,21"),
          "",
          "a3-c5 h8-a3".some
        ),
        StartingPosition(
          "4-VII",
          FEN("W:W17,20,21,22,23,25,27,28,29,30,31,32:B1,2,3,4,6,7,8,9,10,12,14,15"),
          "1. cb4 bc5 2. dc3 fe5 3. gh4 ab6",
          "cb4 bc5 dc3 fe5 gh4 ab6".some
        ),
        StartingPosition(
          "4-VIII",
          FEN("W:W16,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,5,6,7,8,9,10,11,12,21"),
          "",
          "a3-g5 h8-a3".some
        ),
        StartingPosition(
          "4-IX",
          FEN("B:W17,19,21,22,24,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,15"),
          "1. cb4 fe5 2. ef4 gf6 3. bc3",
          "cb4 fe5 ef4 gf6 bc3".some
        ),
        StartingPosition(
          "4-X",
          FEN("W:W13,21,22,23,24,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,12"),
          "",
          "b2-a5 ---".some
        ),
        StartingPosition(
          "4-XI",
          FEN("B:W18,19,21,22,23,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,11,12,13,15"),
          "1. cd4 ba5 2. bc3 de5 3. gf4",
          "cd4 ba5 bc3 de5 gf4".some
        ),
        StartingPosition(
          "4-XII",
          FEN("W:W16,21,22,23,24,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,13"),
          "",
          "b2-g5 g7-a5".some
        ),
        StartingPosition(
          "4-XIII",
          FEN("B:W18,20,21,22,23,26,27,28,29,30,31,32:B1,2,3,4,5,6,8,9,10,11,12,14"),
          "1. cd4 dc5 2. bc3 ed6 3. gh4",
          "cd4 dc5 bc3 ed6 gh4".some
        ),
        StartingPosition(
          "4-XIV",
          FEN("W:W17,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,15"),
          "",
          "c3-b4 h6-e5".some
        ),
        StartingPosition(
          "4-XV",
          FEN("W:W18,21,22,23,24,25,26,27,28,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,20"),
          "1. cd4 fg5 2. bc3 gf6 3. ab2 gh4",
          "cd4 fg5 bc3 gf6 ab2 gh4".some
        ),
        StartingPosition(
          "4-XVI",
          FEN("W:W16,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,7,8,9,10,11,12,14"),
          "",
          "c3-g5 c7-c5".some
        ),
        StartingPosition(
          "4-XVII",
          FEN("B:W18,21,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,12"),
          "1. ed4",
          "ed4".some
        ),
        StartingPosition(
          "4-XVIII",
          FEN("W:W17,21,22,23,24,25,27,28,29,30,31,32:B1,2,3,5,6,7,8,9,10,11,12,13"),
          "",
          "d2-b4 h8-a5".some
        ),
        StartingPosition(
          "4-XIX",
          FEN("W:W18,20,21,22,23,25,26,28,29,30,31,32:B1,2,3,4,5,7,8,9,10,12,14,16"),
          "1. ed4 dc5 2. fe3 fg5 3. gh4 cd6",
          "ed4 dc5 fe3 fg5 gh4 cd6".some
        ),
        StartingPosition(
          "4-XX",
          FEN("W:W16,21,22,23,24,25,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,20"),
          "",
          "d2-g5 h6-h4".some
        ),
        StartingPosition(
          "4-XXI",
          FEN("W:W18,20,21,22,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,8,9,10,11,12,16"),
          "1. ed4 fg5 2. gh4 ef6",
          "ed4 fg5 gh4 ef6".some
        ),
        StartingPosition(
          "4-XXII",
          FEN("W:W17,21,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,19"),
          "",
          "e3-b4 b6-f4".some
        ),
        StartingPosition(
          "4-XXIII",
          FEN("W:W19,21,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,15"),
          "1. ef4 de5",
          "ef4 de5".some
        ),
        StartingPosition(
          "4-XXIV",
          FEN("W:W20,21,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,16"),
          "",
          "e3-h4 h6-g5".some
        ),
        StartingPosition(
          "4-XXV",
          FEN("W:W17,19,21,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,12,14,16"),
          "1. gf4 dc5 2. cb4 fg5",
          "gf4 dc5 cb4 fg5".some
        ),
        StartingPosition(
          "4-XXVI",
          FEN("W:W19,21,22,23,24,25,26,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,13"),
          "",
          "f2-f4 h6-a5".some
        ),
        StartingPosition(
          "4-XXVII",
          FEN("W:W20,21,22,23,24,25,26,27,28,29,30,31:B1,2,3,4,6,7,8,9,10,12,13,16"),
          "1. gh4 ba5 2. fg3 ab6 3. gf2 fg5",
          "gh4 ba5 fg3 ab6 gf2 fg5".some
        ),
        StartingPosition(
          "4-XXVIII",
          FEN("W:W13,21,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,16"),
          "",
          "g3-a5 h6-g5".some
        ),
        StartingPosition(
          "4-XXIX",
          FEN("W:W20,21,22,23,24,25,26,27,29,30,31,32:B1,2,3,4,5,6,7,8,11,12,14,15"),
          "1. gh4 de5 2. hg3 bc5",
          "gh4 de5 hg3 bc5".some
        ),
        StartingPosition(
          "4-XXX",
          FEN("W:W16,21,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,13"),
          "",
          "g3-g5 h6-a5".some
        ),
        StartingPosition(
          "4-XXXI",
          FEN("W:W20,21,22,23,24,25,26,27,29,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,16"),
          "1. gh4 hg5 2. hg3 gh6",
          "gh4 hg5 hg3 gh6".some
        ),
        StartingPosition(
          "4-XXXII",
          FEN("W:W18,21,22,23,24,25,26,27,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,14"),
          "",
          "h2-d4 b6-c5".some
        ),
        StartingPosition(
          "4-XXXIII",
          FEN("W:W20,21,22,23,24,25,26,27,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,15"),
          "",
          "h2-h4 d6-e5".some
        )
      )
    ),
    Category(
      "5",
      List(
        StartingPosition(
          "5-I",
          FEN("W:W17,21,22,23,24,25,26,27,28,30,31,32:B1,2,3,4,6,7,8,9,11,12,13,15"),
          "1. ab4 ba5 2. ba3 ab6 3. ab2 de5",
          "ab4 ba5 ba3 ab6 ab2 de5".some
        ),
        StartingPosition(
          "5-II",
          FEN("W:W13,21,22,23,24,25,26,27,28,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,20"),
          "",
          "a1-a5 f6-h4".some
        ),
        StartingPosition(
          "5-III",
          FEN("W:W13,18,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,16,17"),
          "1. ab4 bc5 2. ba5 cb4 3. ed4 hg5",
          "ab4 bc5 ba5 cb4 ed4 hg5".some
        ),
        StartingPosition(
          "5-IV",
          FEN("W:W19,21,22,23,24,25,26,27,28,30,31,32:B1,2,3,4,5,7,8,9,10,11,12,13"),
          "",
          "a1-f4 c7-a5".some
        ),
        StartingPosition(
          "5-V",
          FEN("W:W22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,12,16,17"),
          "1. ab4 fg5 2. bc5 db4",
          "ab4 fg5 bc5 db4".some
        ),
        StartingPosition(
          "5-VI",
          FEN("W:W18,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,21"),
          "",
          "a3-d4 b6-a3".some
        ),
        StartingPosition(
          "5-VII",
          FEN("W:W17,20,21,22,23,25,27,28,29,30,31,32:B1,2,3,4,5,6,7,10,11,12,14,15"),
          "1. cb4 bc5 2. dc3 fe5 3. gh4 gf6",
          "cb4 bc5 dc3 fe5 gh4 gf6".some
        ),
        StartingPosition(
          "5-VIII",
          FEN("W:W20,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,6,7,8,9,10,11,12,21"),
          "",
          "a3-h4 a7-a3".some
        ),
        StartingPosition(
          "5-IX",
          FEN("W:W17,19,21,23,24,25,27,28,29,30,31,32:B1,2,3,4,5,6,7,10,11,12,14,15"),
          "1. cb4 fe5 2. ef4 gf6 3. de3 bc5",
          "cb4 fe5 ef4 gf6 de3 bc5".some
        ),
        StartingPosition(
          "5-X",
          FEN("W:W13,21,22,23,24,26,27,28,29,30,31,32:B1,2,3,4,6,7,8,9,10,11,12,15"),
          "",
          "b2-a5 a7-e5".some
        ),
        StartingPosition(
          "5-XI",
          FEN("W:W18,21,22,23,24,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,12,13,15"),
          "1. cd4 ba5 2. bc3 fe5",
          "cd4 ba5 bc3 fe5".some
        ),
        StartingPosition(
          "5-XII",
          FEN("W:W16,21,22,23,24,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,15"),
          "",
          "b2-g5 h6-e5".some
        ),
        StartingPosition(
          "5-XIII",
          FEN("W:W17,18,21,23,24,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,12,13,14,16"),
          "1. cd4 dc5 2. bc3 fg5 3. cb4 ba5",
          "cd4 dc5 bc3 fg5 cb4 ba5".some
        ),
        StartingPosition(
          "5-XIV",
          FEN("W:W14,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,16"),
          "",
          "c3-c5 f6-g5".some
        ),
        StartingPosition(
          "5-XV",
          FEN("W:W18,21,22,23,24,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,19"),
          "1. cd4 fg5 2. bc3 gf4",
          "cd4 fg5 bc3 gf4".some
        ),
        StartingPosition(
          "5-XVI",
          FEN("W:W16,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,7,8,9,10,11,12,15"),
          "",
          "c3-g5 c7-e5".some
        ),
        StartingPosition(
          "5-XVII",
          FEN("W:W18,21,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,13"),
          "1. ed4 ba5",
          "ed4 ba5".some
        ),
        StartingPosition(
          "5-XVIII",
          FEN("W:W17,21,22,23,24,25,27,28,29,30,31,32:B1,2,3,5,6,7,8,9,10,11,12,16"),
          "",
          "d2-b4 h8-g5".some
        ),
        StartingPosition(
          "5-XIX",
          FEN("W:W18,20,21,22,23,25,26,28,29,30,31,32:B1,2,3,4,5,6,7,9,11,12,14,16"),
          "1. ed4 dc5 2. fe3 fg5 3. gh4 gf6",
          "ed4 dc5 fe3 fg5 gh4 gf6".some
        ),
        StartingPosition(
          "5-XX",
          FEN("W:W20,21,22,23,24,25,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,14"),
          "",
          "d2-h4 b6-c5".some
        ),
        StartingPosition(
          "5-XXI",
          FEN("W:W18,20,21,22,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,16"),
          "1. ed4 fg5 2. gh4 gf6",
          "ed4 fg5 gh4 gf6".some
        ),
        StartingPosition(
          "5-XXII",
          FEN("W:W17,21,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,16"),
          "",
          "e3-b4 b6-g5".some
        ),
        StartingPosition(
          "5-XXIII",
          FEN("B:W21,22,23,24,25,27,28,29,30,31,32:B1,2,3,4,5,6,8,9,11,12,14"),
          "1. ef4 de5 2. fd6 ec5 3. de3",
          "ef4 de5 fd6 ec5 de3".some
        ),
        StartingPosition(
          "5-XXIV",
          FEN("W:W12,21,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,13"),
          "",
          "e3-h6 h6-a5".some
        ),
        StartingPosition(
          "5-XXV",
          FEN("W:W18,19,21,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,8,9,10,11,12,14"),
          "1. gf4 dc5 2. cd4 ed6",
          "gf4 dc5 cd4 ed6".some
        ),
        StartingPosition(
          "5-XXVI",
          FEN("W:W19,21,22,23,24,25,26,28,29,30,31,32:B1,2,3,5,6,7,8,9,10,11,12,15"),
          "",
          "f2-f4 h8-e5".some
        ),
        StartingPosition(
          "5-XXVII",
          FEN("B:W18,20,21,22,24,25,26,28,29,30,31,32:B1,2,3,4,5,6,7,8,11,12,13,15"),
          "1. gh4 ba5 2. fg3 de5 3. ed4",
          "gh4 ba5 fg3 de5 ed4".some
        ),
        StartingPosition(
          "5-XXVIII",
          FEN("W:W17,21,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,6,7,8,9,10,11,12,15"),
          "",
          "g3-b4 a7-e5".some
        ),
        StartingPosition(
          "5-XXIX",
          FEN("W:W20,21,22,23,24,25,26,27,29,30,31,32:B1,2,3,4,5,7,8,9,10,11,12,15"),
          "1. gh4 de5 2. hg3 cd6",
          "gh4 de5 hg3 cd6".some
        ),
        StartingPosition(
          "5-XXX",
          FEN("W:W20,21,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,12"),
          "",
          "g3-h4 ---".some
        ),
        StartingPosition(
          "5-XXXI",
          FEN("W:W13,21,22,23,24,25,26,27,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,20"),
          "",
          "h2-a5 f6-h4".some
        ),
        StartingPosition(
          "5-XXXII",
          FEN("W:W18,21,22,23,24,25,26,27,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,28"),
          "",
          "h2-d4 b6-h2".some
        ),
        StartingPosition(
          "5-XXXIII",
          FEN("W:W20,21,22,23,24,25,26,27,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,28"),
          "",
          "h2-h4 d6-h2".some
        )
      )
    ),
    Category(
      "6",
      List(
        StartingPosition(
          "6-I",
          FEN("W:W17,21,22,23,24,25,26,27,28,30,31,32:B1,2,3,4,5,7,8,10,11,12,13,14"),
          "1. ab4 ba5 2. ba3 cb6 3. ab2 bc5",
          "ab4 ba5 ba3 cb6 ab2 bc5".some
        ),
        StartingPosition(
          "6-II",
          FEN("W:W13,21,22,23,24,25,26,27,28,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,15"),
          "",
          "a1-a5 g7-e5".some
        ),
        StartingPosition(
          "6-III",
          FEN("W:W13,19,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,12,15,17"),
          "1. ab4 bc5 2. ba5 cb4 3. gf4 fe5",
          "ab4 bc5 ba5 cb4 gf4 fe5".some
        ),
        StartingPosition(
          "6-IV",
          FEN("W:W19,21,22,23,24,25,26,27,28,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,14"),
          "",
          "a1-f4 d6-c5".some
        ),
        StartingPosition(
          "6-V",
          FEN("W:W17,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,16"),
          "1. ab4 hg5",
          "ab4 hg5".some
        ),
        StartingPosition(
          "6-VI",
          FEN("W:W18,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,7,8,9,10,11,12,13"),
          "",
          "a3-d4 c7-a5".some
        ),
        StartingPosition(
          "6-VII",
          FEN("W:W17,18,21,24,25,26,28,29,30,31,32:B1,2,3,4,5,7,8,9,10,11,12"),
          "1. cb4 bc5 2. ed4 ce3 3. fd4 cb6",
          "cb4 bc5 ed4 ce3 fd4 cb6".some
        ),
        StartingPosition(
          "6-VIII",
          FEN("W:W20,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,6,7,8,9,10,11,12,13"),
          "",
          "a3-h4 a7-a5".some
        ),
        StartingPosition(
          "6-IX",
          FEN("B:W16,17,21,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,12,13,15"),
          "1. cb4 fe5 2. gh4 ba5 3. hg5",
          "cb4 fe5 gh4 ba5 hg5".some
        ),
        StartingPosition(
          "6-X",
          FEN("W:W13,21,22,23,24,26,27,28,29,30,31,32:B1,2,3,4,6,7,8,9,10,11,12,20"),
          "",
          "b2-a5 a7-h4".some
        ),
        StartingPosition(
          "6-XI",
          FEN("B:W18,21,22,23,24,25,26,27,28,30,31,32:B1,2,3,4,5,6,7,8,10,11,13,16"),
          "1. cd4 ba5 2. bc3 hg5 3. ab2",
          "cd4 ba5 bc3 hg5 ab2".some
        ),
        StartingPosition(
          "6-XII",
          FEN("W:W16,21,22,23,24,26,27,28,29,30,31,32:B1,2,3,5,6,7,8,9,10,11,12,13"),
          "",
          "b2-g5 h8-a5".some
        ),
        StartingPosition(
          "6-XIII",
          FEN("W:W17,18,21,23,24,25,27,28,29,30,31,32:B1,2,3,4,5,6,8,9,10,12,14,16"),
          "1. cd4 dc5 2. dc3 ed6 3. cb4 fg5",
          "cd4 dc5 dc3 ed6 cb4 fg5".some
        ),
        StartingPosition(
          "6-XIV",
          FEN("W:W14,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,15"),
          "",
          "c3-c5 g7-e5".some
        ),
        StartingPosition(
          "6-XV",
          FEN("W:W17,18,21,23,24,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,12,15,20"),
          "1. cd4 fg5 2. bc3 gh4 3. cb4 de5",
          "cd4 fg5 bc3 gh4 cb4 de5".some
        ),
        StartingPosition(
          "6-XVI",
          FEN("W:W16,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,14"),
          "",
          "c3-g5 g7-c5".some
        ),
        StartingPosition(
          "6-XVII",
          FEN("W:W14,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,12,13,16"),
          "1. ed4 ba5 2. dc5 db4 3. ac5 fg5",
          "ed4 ba5 dc5 db4 ac5 fg5".some
        ),
        StartingPosition(
          "6-XVIII",
          FEN("W:W17,21,22,23,24,25,27,28,29,30,31,32:B1,2,3,5,6,7,8,9,10,11,12,20"),
          "",
          "d2-b4 h8-h4".some
        ),
        StartingPosition(
          "6-XIX",
          FEN("W:W18,20,21,22,23,25,26,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,12,14,19"),
          "1. ed4 dc5 2. fe3 fg5 3. gh4 gf4",
          "ed4 dc5 fe3 fg5 gh4 gf4".some
        ),
        StartingPosition(
          "6-XX",
          FEN("W:W20,21,22,23,24,25,27,28,29,30,31,32:B1,2,3,4,5,6,8,9,10,11,12,14"),
          "",
          "d2-h4 e7-c5".some
        ),
        StartingPosition(
          "6-XXI",
          FEN("W:W18,21,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,16"),
          "1. ed4 hg5",
          "ed4 hg5".some
        ),
        StartingPosition(
          "6-XXII",
          FEN("W:W17,21,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,15"),
          "",
          "e3-b4 f6-e5".some
        ),
        StartingPosition(
          "6-XXIII",
          FEN("W:W21,22,23,24,25,27,28,29,30,31,32:B1,2,3,4,5,7,8,9,12,15,16"),
          "1. ef4 de5 2. fd6 ce5 3. de3 fg5",
          "ef4 de5 fd6 ce5 de3 fg5".some
        ),
        StartingPosition(
          "6-XXIV",
          FEN("W:W12,21,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,15"),
          "",
          "e3-h6 h6-e5".some
        ),
        StartingPosition(
          "6-XXV",
          FEN("W:W19,20,21,22,23,25,26,27,29,30,31,32:B1,2,3,4,5,6,8,9,10,12,14,15"),
          "1. gf4 dc5 2. hg3 ed6 3. gh4 fe5",
          "gf4 dc5 hg3 ed6 gh4 fe5".some
        ),
        StartingPosition(
          "6-XXVI",
          FEN("W:W16,21,22,23,24,25,26,28,29,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,13"),
          "",
          "f2-g5 g7-a5".some
        ),
        StartingPosition(
          "6-XXVII",
          FEN("B:W18,20,21,22,24,25,26,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,12,13,15"),
          "1. gh4 ba5 2. fg3 fe5 3. ed4",
          "gh4 ba5 fg3 fe5 ed4".some
        ),
        StartingPosition(
          "6-XXVIII",
          FEN("W:W17,21,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,15"),
          "",
          "g3-b4 b6-e5".some
        ),
        StartingPosition(
          "6-XXIX",
          FEN("W:W20,21,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,15"),
          "1. gh4 fe5",
          "gh4 fe5".some
        ),
        StartingPosition(
          "6-XXX",
          FEN("W:W20,21,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,6,7,8,9,10,11,12,16"),
          "",
          "g3-h4 a7-g5".some
        ),
        StartingPosition(
          "6-XXXI",
          FEN("W:W13,21,22,23,24,25,26,27,29,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,17"),
          "",
          "h2-a5 g7-b4".some
        ),
        StartingPosition(
          "6-XXXII",
          FEN("W:W18,21,22,23,24,25,26,27,29,30,31,32:B1,2,3,4,5,7,8,9,10,11,12,28"),
          "",
          "h2-d4 c7-h2".some
        ),
        StartingPosition(
          "6-XXXIII",
          FEN("W:W12,21,22,23,24,25,26,27,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,19"),
          "",
          "h2-h6 h6-f4".some
        )
      )
    ),
    Category(
      "7",
      List(
        StartingPosition(
          "7-I",
          FEN("W:W17,21,22,23,24,25,26,27,28,29,31,32:B1,2,3,4,5,7,8,9,11,12,13,15"),
          "1. ab4 ba5 2. ba3 cb6 3. cb2 de5",
          "ab4 ba5 ba3 cb6 cb2 de5".some
        ),
        StartingPosition(
          "7-II",
          FEN("W:W13,21,22,23,24,25,26,27,28,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,16"),
          "",
          "a1-a5 g7-g5".some
        ),
        StartingPosition(
          "7-III",
          FEN("B:W13,20,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,17"),
          "1. ab4 bc5 2. ba5 cb4 3. gh4",
          "ab4 bc5 ba5 cb4 gh4".some
        ),
        StartingPosition(
          "7-IV",
          FEN("W:W19,21,22,23,24,25,26,27,28,30,31,32:B1,2,3,4,5,6,8,9,10,11,12,15"),
          "",
          "a1-f4 e7-e5".some
        ),
        StartingPosition(
          "7-V",
          FEN("W:W17,21,22,23,24,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,19"),
          "1. ab4 hg5 2. ba3 gf4",
          "ab4 hg5 ba3 gf4".some
        ),
        StartingPosition(
          "7-VI",
          FEN("W:W18,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,8,9,10,11,12,14"),
          "",
          "a3-d4 e7-c5".some
        ),
        StartingPosition(
          "7-VII",
          FEN("W:W17,18,21,24,25,26,28,29,30,31,32:B1,2,3,4,5,6,7,8,11,12,15"),
          "1. cb4 bc5 2. ed4 ce3 3. fd4 de5",
          "cb4 bc5 ed4 ce3 fd4 de5".some
        ),
        StartingPosition(
          "7-VIII",
          FEN("W:W20,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,14"),
          "",
          "a3-h4 b6-c5".some
        ),
        StartingPosition(
          "7-IX",
          FEN("B:W16,17,21,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,12,14,15"),
          "1. cb4 fe5 2. gh4 bc5 3. hg5",
          "cb4 fe5 gh4 bc5 hg5".some
        ),
        StartingPosition(
          "7-X",
          FEN("W:W13,21,22,23,24,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,17"),
          "",
          "b2-a5 b6-b4".some
        ),
        StartingPosition(
          "7-XI",
          FEN("W:W18,21,22,23,24,25,26,27,28,29,31,32:B1,2,3,4,5,6,7,8,10,11,13,19"),
          "1. cd4 ba5 2. bc3 hg5 3. cb2 gf4",
          "cd4 ba5 bc3 hg5 cb2 gf4".some
        ),
        StartingPosition(
          "7-XII",
          FEN("W:W20,21,22,23,24,26,27,28,29,30,31,32:B1,2,3,4,6,7,8,9,10,11,12,13"),
          "",
          "b2-h4 a7-a5".some
        ),
        StartingPosition(
          "7-XIII",
          FEN("W:W18,21,22,23,24,25,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,12,14,15"),
          "1. cd4 dc5 2. dc3 fe5",
          "cd4 dc5 dc3 fe5".some
        ),
        StartingPosition(
          "7-XIV",
          FEN("W:W14,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,20"),
          "",
          "c3-c5 g7-h4".some
        ),
        StartingPosition(
          "7-XV",
          FEN("W:W17,18,21,23,24,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,16,20"),
          "1. cd4 fg5 2. bc3 gh4 3. cb4 hg5",
          "cd4 fg5 bc3 gh4 cb4 hg5".some
        ),
        StartingPosition(
          "7-XVI",
          FEN("W:W16,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,15"),
          "",
          "c3-g5 g7-e5".some
        ),
        StartingPosition(
          "7-XVII",
          FEN("W:W18,21,22,23,24,25,26,28,29,30,31,32:B1,2,3,4,6,7,8,9,10,11,12,13"),
          "1. ed4 ba5 2. fe3 ab6",
          "ed4 ba5 fe3 ab6".some
        ),
        StartingPosition(
          "7-XVIII",
          FEN("W:W14,21,22,23,24,25,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,12"),
          "",
          "d2-c5 ---".some
        ),
        StartingPosition(
          "7-XIX",
          FEN("W:W18,20,21,22,25,26,28,29,30,31,32:B1,2,3,4,5,6,7,8,11,12,13"),
          "1. ed4 dc5 2. gh4 ce3 3. fd4 ba5",
          "ed4 dc5 gh4 ce3 fd4 ba5".some
        ),
        StartingPosition(
          "7-XX",
          FEN("W:W20,21,22,23,24,25,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,16"),
          "",
          "d2-h4 f6-g5".some
        ),
        StartingPosition(
          "7-XXI",
          FEN("W:W17,18,21,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,15,16"),
          "1. ed4 hg5 2. cb4 de5",
          "ed4 hg5 cb4 de5".some
        ),
        StartingPosition(
          "7-XXII",
          FEN("W:W17,21,22,24,25,26,27,28,29,30,31,32:B1,2,3,5,6,7,8,9,10,11,12,15"),
          "",
          "e3-b4 h8-e5".some
        ),
        StartingPosition(
          "7-XXIII",
          FEN("B:W18,19,21,24,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12"),
          "1. ef4 fe5 2. cd4 ec3 3. bd4",
          "ef4 fe5 cd4 ec3 bd4".some
        ),
        StartingPosition(
          "7-XXIV",
          FEN("W:W21,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,8,9,10,11,12,20"),
          "",
          "--- e7-h4".some
        ),
        StartingPosition(
          "7-XXV",
          FEN("W:W19,21,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,15"),
          "1. gf4 de5",
          "gf4 de5".some
        ),
        StartingPosition(
          "7-XXVI",
          FEN("W:W16,21,22,23,24,25,26,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,13"),
          "",
          "f2-g5 h6-a5".some
        ),
        StartingPosition(
          "7-XXVII",
          FEN("B:W20,21,22,23,24,25,26,27,28,29,30,31:B1,2,3,4,5,6,7,8,10,11,13,16"),
          "1. gh4 ba5 2. fg3 hg5 3. gf2",
          "gh4 ba5 fg3 hg5 gf2".some
        ),
        StartingPosition(
          "7-XXVIII",
          FEN("W:W17,21,22,23,25,26,27,28,29,30,31,32:B1,2,4,5,6,7,8,9,10,11,12,19"),
          "",
          "g3-b4 f8-f4".some
        ),
        StartingPosition(
          "7-XXIX",
          FEN("W:W17,20,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,18"),
          "1. gh4 fe5 2. ab4 ed4",
          "gh4 fe5 ab4 ed4".some
        ),
        StartingPosition(
          "7-XXX",
          FEN("W:W20,21,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,7,8,9,10,11,12,19"),
          "",
          "g3-h4 c7-f4".some
        ),
        StartingPosition(
          "7-XXXI",
          FEN("W:W13,21,22,23,24,25,26,27,29,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,28"),
          "",
          "h2-a5 g7-h2".some
        ),
        StartingPosition(
          "7-XXXII",
          FEN("W:W18,21,22,23,24,25,26,27,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,28"),
          "",
          "h2-d4 d6-h2".some
        ),
        StartingPosition(
          "7-XXXIII",
          FEN("W:W12,21,22,23,24,25,26,27,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,20"),
          "",
          "h2-h6 h6-h4".some
        )
      )
    ),
    Category(
      "8",
      List(
        StartingPosition(
          "8-I",
          FEN("B:W17,19,21,22,23,26,27,28,29,30,31,32:B1,2,3,4,5,7,8,9,10,11,12,13"),
          "1. ab4 ba5 2. ba3 cb6 3. gf4",
          "ab4 ba5 ba3 cb6 gf4".some
        ),
        StartingPosition(
          "8-II",
          FEN("W:W13,21,22,23,24,25,26,27,28,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,20"),
          "",
          "a1-a5 g7-h4".some
        ),
        StartingPosition(
          "8-III",
          FEN("W:W13,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,12,14,16"),
          "1. ab4 bc5 2. ba5 fg5",
          "ab4 bc5 ba5 fg5".some
        ),
        StartingPosition(
          "8-IV",
          FEN("W:W19,21,22,23,24,25,26,27,28,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,15"),
          "",
          "a1-f4 f6-e5".some
        ),
        StartingPosition(
          "8-V",
          FEN("W:W17,19,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,14,16"),
          "1. ab4 hg5 2. gf4 bc5",
          "ab4 hg5 gf4 bc5".some
        ),
        StartingPosition(
          "8-VI",
          FEN("W:W18,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,21"),
          "",
          "a3-d4 f6-a3".some
        ),
        StartingPosition(
          "8-VII",
          FEN("W:W17,19,21,23,24,25,26,27,29,30,31,32:B1,2,3,4,6,7,8,9,10,12,14,16"),
          "1. cb4 bc5 2. gf4 ab6 3. hg3 fg5",
          "cb4 bc5 gf4 ab6 hg3 fg5".some
        ),
        StartingPosition(
          "8-VIII",
          FEN("W:W20,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,7,8,9,10,11,12,21"),
          "",
          "a3-h4 c7-a3".some
        ),
        StartingPosition(
          "8-IX",
          FEN("W:W17,18,21,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,12,13,16"),
          "1. cb4 fg5 2. ed4 ba5",
          "cb4 fg5 ed4 ba5".some
        ),
        StartingPosition(
          "8-X",
          FEN("W:W13,21,22,23,24,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,15"),
          "",
          "b2-a5 f6-e5".some
        ),
        StartingPosition(
          "8-XI",
          FEN("B:W18,19,21,22,23,25,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,11,12,13,15"),
          "1. cd4 ba5 2. dc3 de5 3. gf4",
          "cd4 ba5 dc3 de5 gf4".some
        ),
        StartingPosition(
          "8-XII",
          FEN("W:W20,21,22,23,24,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,13"),
          "",
          "b2-h4 b6-a5".some
        ),
        StartingPosition(
          "8-XIII",
          FEN("W:W18,21,22,23,24,25,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,14,16"),
          "1. cd4 dc5 2. dc3 hg5",
          "cd4 dc5 dc3 hg5".some
        ),
        StartingPosition(
          "8-XIV",
          FEN("W:W18,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,6,7,8,9,10,11,12,15"),
          "",
          "c3-d4 a7-e5".some
        ),
        StartingPosition(
          "8-XV",
          FEN("B:W14,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,16"),
          "1. cd4 fg5 2. dc5",
          "cd4 fg5 dc5".some
        ),
        StartingPosition(
          "8-XVI",
          FEN("W:W16,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,5,6,7,8,9,10,11,12,13"),
          "",
          "c3-g5 h8-a5".some
        ),
        StartingPosition(
          "8-XVII",
          FEN("W:W17,18,22,23,24,25,26,28,29,30,31,32:B1,2,3,4,6,7,8,9,11,12,13,14"),
          "1. ed4 ba5 2. fe3 ab6 3. ab4 dc5",
          "ed4 ba5 fe3 ab6 ab4 dc5".some
        ),
        StartingPosition(
          "8-XVIII",
          FEN("W:W14,21,22,23,24,25,27,28,29,30,31,32:B1,2,3,4,6,7,8,9,10,11,12,26"),
          "",
          "d2-c5 a7-d2".some
        ),
        StartingPosition(
          "8-XIX",
          FEN("W:W18,21,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,15"),
          "1. ed4 de5",
          "ed4 de5".some
        ),
        StartingPosition(
          "8-XX",
          FEN("W:W20,21,22,23,24,25,27,28,29,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,15"),
          "",
          "d2-h4 g7-e5".some
        ),
        StartingPosition(
          "8-XXI",
          FEN("W:W17,18,21,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,19"),
          "1. ed4 hg5 2. cb4 gf4",
          "ed4 hg5 cb4 gf4".some
        ),
        StartingPosition(
          "8-XXII",
          FEN("W:W17,21,22,24,25,26,27,28,29,30,31,32:B1,2,3,5,6,7,8,9,10,11,12,20"),
          "",
          "e3-b4 h8-h4".some
        ),
        StartingPosition(
          "8-XXIII",
          FEN("B:W19,21,22,23,24,25,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,15"),
          "1. ef4 fe5 2. de3",
          "ef4 fe5 de3".some
        ),
        StartingPosition(
          "8-XXIV",
          FEN("W:W13,21,22,23,24,25,26,28,29,30,31,32:B1,3,4,5,6,7,8,9,10,11,12,19"),
          "",
          "f2-a5 d8-f4".some
        ),
        StartingPosition(
          "8-XXV",
          FEN("W:W21,22,23,24,25,26,28,29,30,31,32:B2,3,4,5,6,7,8,9,11,12,15"),
          "1. gf4 de5 2. fd6 ce5 3. fg3 bc7",
          "gf4 de5 fd6 ce5 fg3 bc7".some
        ),
        StartingPosition(
          "8-XXVI",
          FEN("W:W16,21,22,23,24,25,26,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,14"),
          "",
          "f2-g5 h6-c5".some
        ),
        StartingPosition(
          "8-XXVII",
          FEN("B:W18,20,21,22,24,25,26,27,29,30,31,32:B1,2,3,4,5,6,7,8,10,12,13,15"),
          "1. gh4 ba5 2. hg3 fe5 3. ed4",
          "gh4 ba5 hg3 fe5 ed4".some
        ),
        StartingPosition(
          "8-XXVIII",
          FEN("W:W17,21,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,16"),
          "",
          "g3-b4 g7-g5".some
        ),
        StartingPosition(
          "8-XXIX",
          FEN("W:W17,20,21,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,19"),
          "1. gh4 fe5 2. cb4 ef4",
          "gh4 fe5 cb4 ef4".some
        ),
        StartingPosition(
          "8-XXX",
          FEN("W:W20,21,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,13"),
          "",
          "g3-h4 g7-a5".some
        ),
        StartingPosition(
          "8-XXXI",
          FEN("W:W13,21,22,23,24,25,26,27,29,30,31,32:B1,2,3,5,6,7,8,9,10,11,12,28"),
          "",
          "h2-a5 h8-h2".some
        ),
        StartingPosition(
          "8-XXXII",
          FEN("W:W18,21,22,23,24,25,26,27,29,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,28"),
          "",
          "h2-d4 g7-h2".some
        ),
        StartingPosition(
          "8-XXXIII",
          FEN("W:W21,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,20"),
          "",
          "--- h6-h4".some
        )
      )
    ),
    Category(
      "9",
      List(
        StartingPosition(
          "9-I",
          FEN("W:W17,20,21,22,23,26,27,28,29,30,31,32:B1,2,3,4,5,7,8,9,10,12,13,15"),
          "1. ab4 ba5 2. ba3 cb6 3. gh4 fe5",
          "ab4 ba5 ba3 cb6 gh4 fe5".some
        ),
        StartingPosition(
          "9-II",
          FEN("W:W13,21,22,23,24,25,26,27,28,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,17"),
          "",
          "a1-a5 h6-b4".some
        ),
        StartingPosition(
          "9-III",
          FEN("W:W17,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,14"),
          "1. ab4 dc5",
          "ab4 dc5".some
        ),
        StartingPosition(
          "9-IV",
          FEN("W:W16,21,22,23,24,25,26,27,28,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,15"),
          "",
          "a1-g5 d6-e5".some
        ),
        StartingPosition(
          "9-V",
          FEN("W:W17,19,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,16"),
          "1. ab4 hg5 2. gf4 gh6",
          "ab4 hg5 gf4 gh6".some
        ),
        StartingPosition(
          "9-VI",
          FEN("W:W18,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,21"),
          "",
          "a3-d4 g7-a3".some
        ),
        StartingPosition(
          "9-VII",
          FEN("W:W17,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,14"),
          "1. cb4 dc5",
          "cb4 dc5".some
        ),
        StartingPosition(
          "9-VIII",
          FEN("W:W20,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,21"),
          "",
          "a3-h4 d6-a3".some
        ),
        StartingPosition(
          "9-IX",
          FEN("W:W17,18,21,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,12,14,16"),
          "1. cb4 fg5 2. ed4 dc5",
          "cb4 fg5 ed4 dc5".some
        ),
        StartingPosition(
          "9-X",
          FEN("W:W17,21,22,23,24,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,15"),
          "",
          "b2-b4 d6-e5".some
        ),
        StartingPosition(
          "9-XI",
          FEN("B:W18,20,21,22,23,25,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,11,12,13,15"),
          "1. cd4 ba5 2. dc3 de5 3. gh4",
          "cd4 ba5 dc3 de5 gh4".some
        ),
        StartingPosition(
          "9-XII",
          FEN("W:W20,21,22,23,24,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,15"),
          "",
          "b2-h4 g7-e5".some
        ),
        StartingPosition(
          "9-XIII",
          FEN("B:W15,19,21,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,7,8,9,10,11,12,14"),
          "1. cd4 dc5 2. gf4 cd6 3. de5",
          "cd4 dc5 gf4 cd6 de5".some
        ),
        StartingPosition(
          "9-XIV",
          FEN("W:W18,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,13"),
          "",
          "c3-d4 d6-a5".some
        ),
        StartingPosition(
          "9-XV",
          FEN("W:W18,21,22,23,24,25,26,27,28,29,30,32:B1,2,3,4,5,6,8,10,11,12,14,16"),
          "1. cd4 fg5 2. dc3 ef6 3. ed2 bc5",
          "cd4 fg5 dc3 ef6 ed2 bc5".some
        ),
        StartingPosition(
          "9-XVI",
          FEN("W:W20,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,12"),
          "",
          "c3-h4 ---".some
        ),
        StartingPosition(
          "9-XVII",
          FEN("W:W18,21,22,23,24,25,26,28,29,30,31,32:B1,2,3,4,5,7,8,9,10,11,12,13"),
          "1. ed4 ba5 2. fe3 cb6",
          "ed4 ba5 fe3 cb6".some
        ),
        StartingPosition(
          "9-XVIII",
          FEN("W:W14,21,22,23,24,25,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,16"),
          "",
          "d2-c5 b6-g5".some
        ),
        StartingPosition(
          "9-XIX",
          FEN("W:W17,18,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,7,8,9,10,11,12,15"),
          "1. ed4 de5 2. ab4 cd6",
          "ed4 de5 ab4 cd6".some
        ),
        StartingPosition(
          "9-XX",
          FEN("W:W20,21,22,23,24,25,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,15"),
          "",
          "d2-h4 h6-e5".some
        ),
        StartingPosition(
          "9-XXI",
          FEN("W:W17,18,21,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,20"),
          "1. ed4 hg5 2. cb4 gh4",
          "ed4 hg5 cb4 gh4".some
        ),
        StartingPosition(
          "9-XXII",
          FEN("W:W9,21,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,17"),
          "",
          "e3-b6 b6-b4".some
        ),
        StartingPosition(
          "9-XXIII",
          FEN("W:W17,19,22,23,24,25,27,28,29,30,31,32:B1,2,3,4,5,7,8,9,10,12,13,15"),
          "1. ef4 fe5 2. de3 ba5 3. ab4 cb6",
          "ef4 fe5 de3 ba5 ab4 cb6".some
        ),
        StartingPosition(
          "9-XXIV",
          FEN("W:W13,21,22,23,24,25,26,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,15"),
          "",
          "f2-a5 d6-e5".some
        ),
        StartingPosition(
          "9-XXV",
          FEN("B:W17,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,7,8,9,11,12,15"),
          "1. gf4 de5 2. fd6 ce5 3. ab4",
          "gf4 de5 fd6 ce5 ab4".some
        ),
        StartingPosition(
          "9-XXVI",
          FEN("W:W20,21,22,23,24,25,26,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,12"),
          "",
          "f2-h4 ---".some
        ),
        StartingPosition(
          "9-XXVII",
          FEN("B:W19,20,21,22,24,25,26,27,29,30,31,32:B1,2,3,4,6,7,8,9,11,12,13,14"),
          "1. gh4 ba5 2. hg3 ab6 3. ef4 dc5",
          "gh4 ba5 hg3 ab6 ef4 dc5".some
        ),
        StartingPosition(
          "9-XXVIII",
          FEN("W:W17,21,22,23,25,26,27,28,29,30,31,32:B1,2,3,5,6,7,8,9,10,11,12,20"),
          "",
          "g3-b4 h8-h4".some
        ),
        StartingPosition(
          "9-XXIX",
          FEN("W:W18,20,21,23,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,12,13"),
          "1. gh4 fe5 2. cd4 ec3 3. bd4 ba5",
          "gh4 fe5 cd4 ec3 bd4 ba5".some
        ),
        StartingPosition(
          "9-XXX",
          FEN("W:W20,21,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,14"),
          "",
          "g3-h4 g7-c5".some
        ),
        StartingPosition(
          "9-XXXI",
          FEN("W:W13,21,22,23,24,25,26,27,29,30,31,32:B1,2,3,5,6,7,8,9,10,11,12,20"),
          "",
          "h2-a5 h8-h4".some
        ),
        StartingPosition(
          "9-XXXII",
          FEN("W:W15,21,22,23,24,25,26,27,29,30,31,32:B1,2,3,4,6,7,8,9,10,11,12,17"),
          "",
          "h2-e5 a7-b4".some
        ),
        StartingPosition(
          "9-XXXIII",
          FEN("W:W21,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,5,6,7,8,9,10,11,12,20"),
          "",
          "--- h8-h4".some
        )
      )
    ),
    Category(
      "10",
      List(
        StartingPosition(
          "10-I",
          FEN("W:W17,21,22,23,24,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,11,12,13,15"),
          "1. ab4 ba5 2. ba3 de5",
          "ab4 ba5 ba3 de5".some
        ),
        StartingPosition(
          "10-II",
          FEN("W:W17,21,22,23,24,25,26,27,28,30,31,32:B1,2,3,4,6,7,8,9,10,11,12,13"),
          "",
          "a1-b4 a7-a5".some
        ),
        StartingPosition(
          "10-III",
          FEN("B:W20,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,8,9,11,12,14"),
          "1. ab4 dc5 2. bd6 ec5 3. gh4",
          "ab4 dc5 bd6 ec5 gh4".some
        ),
        StartingPosition(
          "10-IV",
          FEN("W:W13,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,6,7,8,9,10,11,12,21"),
          "",
          "a3-a5 a7-a3".some
        ),
        StartingPosition(
          "10-V",
          FEN("W:W17,20,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,13,16"),
          "1. ab4 hg5 2. gh4 ba5",
          "ab4 hg5 gh4 ba5".some
        ),
        StartingPosition(
          "10-VI",
          FEN("W:W18,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,13"),
          "",
          "a3-d4 g7-a5".some
        ),
        StartingPosition(
          "10-VII",
          FEN("W:W19,21,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,8,11,12,13,14"),
          "1. cb4 dc5 2. bd6 ec5 3. gf4 ba5",
          "cb4 dc5 bd6 ec5 gf4 ba5".some
        ),
        StartingPosition(
          "10-VIII",
          FEN("W:W20,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,14"),
          "",
          "a3-h4 d6-c5".some
        ),
        StartingPosition(
          "10-IX",
          FEN("W:W17,18,21,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,8,9,10,11,12,16"),
          "1. cb4 fg5 2. ed4 ef6",
          "cb4 fg5 ed4 ef6".some
        ),
        StartingPosition(
          "10-X",
          FEN("W:W17,21,22,23,24,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,16"),
          "",
          "b2-b4 d6-g5".some
        ),
        StartingPosition(
          "10-XI",
          FEN("W:W18,20,21,22,23,25,27,28,29,30,31,32:B1,2,3,4,6,7,8,9,10,12,13,16"),
          "1. cd4 ba5 2. dc3 fg5 3. gh4 ab6",
          "cd4 ba5 dc3 fg5 gh4 ab6".some
        ),
        StartingPosition(
          "10-XII",
          FEN("W:W12,21,22,23,24,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,13"),
          "",
          "b2-h6 h6-a5".some
        ),
        StartingPosition(
          "10-XIII",
          FEN("W:W18,20,21,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,12,14,15"),
          "1. cd4 dc5 2. gh4 fe5",
          "cd4 dc5 gh4 fe5".some
        ),
        StartingPosition(
          "10-XIV",
          FEN("W:W18,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,8,9,10,11,12,15"),
          "",
          "c3-d4 e7-e5".some
        ),
        StartingPosition(
          "10-XV",
          FEN("W:W18,19,21,22,23,25,27,28,29,30,31,32:B1,2,3,4,5,6,7,9,11,12,15,16"),
          "1. cd4 fg5 2. dc3 gf6 3. gf4 de5",
          "cd4 fg5 dc3 gf6 gf4 de5".some
        ),
        StartingPosition(
          "10-XVI",
          FEN("W:W20,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,6,7,8,9,10,11,12,16"),
          "",
          "c3-h4 a7-g5".some
        ),
        StartingPosition(
          "10-XVII",
          FEN("W:W18,20,21,22,23,25,26,28,29,30,31,32:B1,2,3,4,6,7,8,9,11,12,13,15"),
          "1. ed4 ba5 2. fe3 de5 3. gh4 ab6",
          "ed4 ba5 fe3 de5 gh4 ab6".some
        ),
        StartingPosition(
          "10-XVIII",
          FEN("W:W14,21,22,23,24,25,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,20"),
          "",
          "d2-c5 d6-h4".some
        ),
        StartingPosition(
          "10-XIX",
          FEN("W:W17,18,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,8,9,10,11,12,15"),
          "1. ed4 de5 2. ab4 ed6",
          "ed4 de5 ab4 ed6".some
        ),
        StartingPosition(
          "10-XX",
          FEN("W:W21,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,13"),
          "",
          "--- d6-a5".some
        ),
        StartingPosition(
          "10-XXI",
          FEN("W:W18,21,22,23,24,25,26,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,19"),
          "1. ed4 hg5 2. fe3 gf4",
          "ed4 hg5 fe3 gf4".some
        ),
        StartingPosition(
          "10-XXII",
          FEN("W:W14,21,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,16"),
          "",
          "e3-c5 d6-g5".some
        ),
        StartingPosition(
          "10-XXIII",
          FEN("W:W19,21,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,16"),
          "1. ef4 fg5",
          "ef4 fg5".some
        ),
        StartingPosition(
          "10-XXIV",
          FEN("W:W13,21,22,23,24,25,26,28,29,30,31,32:B1,2,3,4,5,6,8,9,10,11,12,15"),
          "",
          "f2-a5 e7-e5".some
        ),
        StartingPosition(
          "10-XXV",
          FEN("B:W21,22,23,24,25,26,27,29,30,31,32:B1,2,3,4,5,6,8,9,11,12,14"),
          "1. gf4 de5 2. fd6 ec5 3. hg3",
          "gf4 de5 fd6 ec5 hg3".some
        ),
        StartingPosition(
          "10-XXVI",
          FEN("W:W20,21,22,23,24,25,26,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,14"),
          "",
          "f2-h4 f6-c5".some
        ),
        StartingPosition(
          "10-XXVII",
          FEN("W:W20,21,22,23,24,25,26,27,28,29,30,31:B2,3,4,5,6,7,8,9,10,11,12,13"),
          "1. gh4 ba5 2. hg3 ab6 3. gh2 ba7",
          "gh4 ba5 hg3 ab6 gh2 ba7".some
        ),
        StartingPosition(
          "10-XXVIII",
          FEN("W:W17,21,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,16"),
          "",
          "g3-b4 h6-g5".some
        ),
        StartingPosition(
          "10-XXIX",
          FEN("W:W18,20,21,22,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,12,14,15"),
          "1. gh4 fe5 2. ed4 bc5",
          "gh4 fe5  ed4 bc5".some
        ),
        StartingPosition(
          "10-XXX",
          FEN("W:W20,21,22,23,25,26,27,28,29,30,31,32:B1,2,3,5,6,7,8,9,10,11,12,15"),
          "",
          "g3-h4 h8-e5".some
        ),
        StartingPosition(
          "10-XXXI",
          FEN("W:W5,21,22,23,24,25,26,27,29,30,31,32:B1,2,3,4,6,7,8,9,10,11,12,18"),
          "",
          "h2-a7 a7-d4".some
        ),
        StartingPosition(
          "10-XXXII",
          FEN("W:W15,21,22,23,24,25,26,27,29,30,31,32:B1,2,3,4,6,7,8,9,10,11,12,18"),
          "",
          "h2-e5 a7-d4".some
        )
      )
    ),
    Category(
      "11",
      List(
        StartingPosition(
          "11-I",
          FEN("B:W17,20,21,22,23,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,12,13,15"),
          "1. ab4 ba5 2. ba3 fe5 3. gh4",
          "ab4 ba5 ba3 fe5 gh4".some
        ),
        StartingPosition(
          "11-II",
          FEN("W:W17,21,22,23,24,25,26,27,28,30,31,32:B1,2,3,4,6,7,8,9,10,11,12,16"),
          "",
          "a1-b4 a7-g5".some
        ),
        StartingPosition(
          "11-III",
          FEN("B:W19,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,7,8,9,11,12,15"),
          "1. ab4 dc5 2. bd6 ce5 3. ef4",
          "ab4 dc5 bd6 ce5 ef4".some
        ),
        StartingPosition(
          "11-IV",
          FEN("W:W13,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,21"),
          "",
          "a3-a5 b6-a3".some
        ),
        StartingPosition(
          "11-V",
          FEN("W:W17,20,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,19"),
          "1. ab4 hg5 2. gh4 gf4",
          "ab4 hg5 gh4 gf4".some
        ),
        StartingPosition(
          "11-VI",
          FEN("W:W18,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,5,6,7,8,9,10,11,12,21"),
          "",
          "a3-d4 h8-a3".some
        ),
        StartingPosition(
          "11-VII",
          FEN("W:W20,21,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,7,8,11,12,13,15"),
          "1. cb4 dc5 2. bd6 ce5 3. gh4 ba5",
          "cb4 dc5 bd6 ce5 gh4 ba5".some
        ),
        StartingPosition(
          "11-VIII",
          FEN("W:W20,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,8,9,10,11,12,21"),
          "",
          "a3-h4 e7-a3".some
        ),
        StartingPosition(
          "11-IX",
          FEN("W:W17,18,21,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,19"),
          "1. cb4 fg5 2. ed4 gf4",
          "cb4 fg5 ed4 gf4".some
        ),
        StartingPosition(
          "11-X",
          FEN("W:W17,21,22,23,24,26,27,28,29,30,31,32:B1,2,3,5,6,7,8,9,10,11,12,13"),
          "",
          "b2-b4 h8-a5".some
        ),
        StartingPosition(
          "11-XI",
          FEN("W:W21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,11,12,13,19"),
          "1. cd4 ba5 2. de5 df4",
          "cd4 ba5 de5 df4".some
        ),
        StartingPosition(
          "11-XII",
          FEN("W:W12,21,22,23,24,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,15"),
          "",
          "b2-h6 h6-e5".some
        ),
        StartingPosition(
          "11-XIII",
          FEN("W:W18,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,15"),
          "1. cd4 de5",
          "cd4 de5".some
        ),
        StartingPosition(
          "11-XIV",
          FEN("W:W18,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,14"),
          "",
          "c3-d4 f6-c5".some
        ),
        StartingPosition(
          "11-XV",
          FEN("B:W17,18,21,23,24,25,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,20"),
          "1. cd4 fg5 2. dc3 gh4 3. cb4",
          "cd4 fg5 dc3 gh4 cb4".some
        ),
        StartingPosition(
          "11-XVI",
          FEN("W:W20,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,13"),
          "",
          "c3-h4 f6-a5".some
        ),
        StartingPosition(
          "11-XVII",
          FEN("W:W18,20,21,22,23,25,26,28,29,30,31,32:B1,2,3,4,5,7,8,9,11,12,13,15"),
          "1. ed4 ba5 2. fe3 de5 3. gh4 cb6",
          "ed4 ba5 fe3 de5 gh4 cb6".some
        ),
        StartingPosition(
          "11-XVIII",
          FEN("W:W18,21,22,23,24,25,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,12"),
          "",
          "d2-d4 ---".some
        ),
        StartingPosition(
          "11-XIX",
          FEN("W:W18,21,22,23,24,25,27,28,29,30,31,32:B1,2,3,4,5,6,8,9,10,11,12,15"),
          "1. ed4 de5 2. de3 ed6",
          "ed4 de5 de3 ed6".some
        ),
        StartingPosition(
          "11-XX",
          FEN("W:W21,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,16"),
          "",
          "--- d6-g5".some
        ),
        StartingPosition(
          "11-XXI",
          FEN("W:W18,20,21,22,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,16"),
          "1. ed4 hg5 2. gh4 gh6",
          "ed4 hg5 gh4 gh6".some
        ),
        StartingPosition(
          "11-XXII",
          FEN("W:W14,21,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,20"),
          "",
          "e3-c5 g7-h4".some
        ),
        StartingPosition(
          "11-XXIII",
          FEN("B:W17,18,21,24,25,26,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12"),
          "1. ef4 fg5 2. cb4 ge3 3. fd4",
          "ef4 fg5 cb4 ge3 fd4".some
        ),
        StartingPosition(
          "11-XXIV",
          FEN("W:W13,21,22,23,24,25,26,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,15"),
          "",
          "f2-a5 f6-e5".some
        ),
        StartingPosition(
          "11-XXV",
          FEN("W:W19,21,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,15"),
          "1. gf4 fe5",
          "gf4 fe5".some
        ),
        StartingPosition(
          "11-XXVI",
          FEN("W:W21,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,13"),
          "",
          "--- f6-a5".some
        ),
        StartingPosition(
          "11-XXVII",
          FEN("W:W20,21,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,14"),
          "1. gh4 bc5",
          "gh4 bc5".some
        ),
        StartingPosition(
          "11-XXVIII",
          FEN("W:W14,21,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,16"),
          "",
          "g3-c5 b6-g5".some
        ),
        StartingPosition(
          "11-XXIX",
          FEN("W:W18,20,21,22,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,12,14,15"),
          "1. gh4 fe5 2. ed4 dc5",
          "gh4 fe5 ed4 dc5".some
        ),
        StartingPosition(
          "11-XXX",
          FEN("W:W12,21,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,13"),
          "",
          "g3-h6 h6-a5".some
        ),
        StartingPosition(
          "11-XXXI",
          FEN("W:W17,21,22,23,24,25,26,27,29,30,31,32:B1,2,3,4,6,7,8,9,10,11,12,15"),
          "",
          "h2-b4 a7-e5".some
        ),
        StartingPosition(
          "11-XXXII",
          FEN("W:W15,21,22,23,24,25,26,27,29,30,31,32:B1,2,3,4,5,6,8,9,10,11,12,28"),
          "",
          "h2-e5 e7-h2".some
        )
      )
    ),
    Category(
      "12",
      List(
        StartingPosition(
          "12-I",
          FEN("W:W17,18,22,23,24,25,26,28,29,30,31,32:B2,3,4,5,6,7,8,9,10,11,12,13"),
          "1. ab4 ba5 2. ed4 ab6 3. fe3 ba7",
          "ab4 ba5 ed4 ab6 fe3 ba7".some
        ),
        StartingPosition(
          "12-II",
          FEN("W:W17,21,22,23,24,25,26,27,28,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,13"),
          "",
          "a1-b4 b6-a5".some
        ),
        StartingPosition(
          "12-III",
          FEN("W:W17,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,15"),
          "1. ab4 de5",
          "ab4 de5".some
        ),
        StartingPosition(
          "12-IV",
          FEN("W:W13,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,17"),
          "",
          "a3-a5 d6-b4".some
        ),
        StartingPosition(
          "12-V",
          FEN("B:W17,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,12"),
          "1. cb4",
          "cb4".some
        ),
        StartingPosition(
          "12-VI",
          FEN("W:W18,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,5,6,7,8,9,10,11,12,20"),
          "",
          "a3-d4 h8-h4".some
        ),
        StartingPosition(
          "12-VII",
          FEN("W:W17,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,15"),
          "1. cb4 de5",
          "cb4 de5".some
        ),
        StartingPosition(
          "12-VIII",
          FEN("W:W20,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,8,9,10,11,12,13"),
          "",
          "a3-h4 e7-a5".some
        ),
        StartingPosition(
          "12-IX",
          FEN("W:W17,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,16"),
          "1. cb4 hg5",
          "cb4 hg5".some
        ),
        StartingPosition(
          "12-X",
          FEN("W:W17,21,22,23,24,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,15"),
          "",
          "b2-b4 h6-e5".some
        ),
        StartingPosition(
          "12-XI",
          FEN("W:W18,19,21,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,11,12,13,14"),
          "1. cd4 ba5 2. ef4 dc5",
          "cd4 ba5 ef4 dc5".some
        ),
        StartingPosition(
          "12-XII",
          FEN("W:W12,21,22,23,24,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,19"),
          "",
          "b2-h6 h6-f4".some
        ),
        StartingPosition(
          "12-XIII",
          FEN("W:W18,21,22,23,24,25,26,27,28,29,31,32:B1,2,4,5,6,7,8,9,10,11,12,15"),
          "1. cd4 de5 2. bc3 ed6 3. cb2 fe7",
          "cd4 de5 bc3 ed6 cb2 fe7".some
        ),
        StartingPosition(
          "12-XIV",
          FEN("W:W18,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,5,6,7,8,9,10,11,12,16"),
          "",
          "c3-d4 h8-g5".some
        ),
        StartingPosition(
          "12-XV",
          FEN("W:W18,19,21,22,23,26,27,28,29,30,31,32:B1,2,3,5,6,7,8,9,10,11,12,16"),
          "1. cd4 fg5 2. gf4 gf6 3. bc3 hg7",
          "cd4 fg5 gf4 gf6 bc3 hg7".some
        ),
        StartingPosition(
          "12-XVI",
          FEN("W:W20,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,15"),
          "",
          "c3-h4 f6-e5".some
        ),
        StartingPosition(
          "12-XVII",
          FEN("W:W18,21,22,23,24,25,26,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,12,13,15"),
          "1. ed4 ba5 2. fe3 fe5",
          "ed4 ba5 fe3 fe5".some
        ),
        StartingPosition(
          "12-XVIII",
          FEN("W:W18,21,22,23,24,25,27,28,29,30,31,32:B1,2,3,4,6,7,8,9,10,11,12,16"),
          "",
          "d2-d4 a7-g5".some
        ),
        StartingPosition(
          "12-XIX",
          FEN("B:W18,21,22,23,24,25,26,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,15"),
          "1. ed4 de5 2. fe3",
          "ed4 de5 fe3".some
        ),
        StartingPosition(
          "12-XX",
          FEN("W:W21,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,20"),
          "",
          "--- d6-h4".some
        ),
        StartingPosition(
          "12-XXI",
          FEN("B:W19,21,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,12"),
          "1. ef4",
          "ef4".some
        ),
        StartingPosition(
          "12-XXII",
          FEN("W:W18,21,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,12"),
          "",
          "e3-d4 ---".some
        ),
        StartingPosition(
          "12-XXIII",
          FEN("W:W17,19,21,24,25,27,28,29,30,31,32:B1,2,3,4,5,6,8,9,10,11,12"),
          "1. ef4 fg5 2. cb4 ge3 3. df4 ef6",
          "ef4 fg5 cb4 ge3 df4 ef6".some
        ),
        StartingPosition(
          "12-XXIV",
          FEN("W:W13,21,22,23,24,25,26,28,29,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,15"),
          "",
          "f2-a5 g7-e5".some
        ),
        StartingPosition(
          "12-XXV",
          FEN("W:W17,20,22,23,25,26,28,29,30,31,32:B1,2,3,4,5,6,7,9,10,11,12"),
          "1. gf4 fe5 2. ab4 eg3 3. fh4 gf6",
          "gf4 fe5 ab4 eg3 fh4 gf6".some
        ),
        StartingPosition(
          "12-XXVI",
          FEN("W:W21,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,20"),
          "",
          "--- f6-h4".some
        ),
        StartingPosition(
          "12-XXVII",
          FEN("W:W18,20,21,23,25,26,27,28,29,30,31,32:B1,2,3,4,6,7,8,9,10,11,12,14"),
          "1. gh4 bc5 2. cd4 ab6",
          "gh4 bc5 cd4 ab6".some
        ),
        StartingPosition(
          "12-XXVIII",
          FEN("W:W18,21,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,6,7,8,9,10,11,12,13"),
          "",
          "g3-d4 a7-a5".some
        ),
        StartingPosition(
          "12-XXIX",
          FEN("W:W18,20,21,22,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,8,9,10,11,12,15"),
          "1. gh4 fe5 2. ed4 ef6",
          "gh4 fe5 ed4 ef6".some
        ),
        StartingPosition(
          "12-XXX",
          FEN("W:W12,21,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,15"),
          "",
          "g3-h6 h6-e5".some
        ),
        StartingPosition(
          "12-XXXI",
          FEN("W:W17,21,22,23,24,25,26,27,29,30,31,32:B1,2,3,4,6,7,8,9,10,11,12,16"),
          "",
          "h2-b4 a7-g5".some
        ),
        StartingPosition(
          "12-XXXII",
          FEN("W:W15,21,22,23,24,25,26,27,29,30,31,32:B1,2,3,5,6,7,8,9,10,11,12,28"),
          "",
          "h2-e5 h8-h2".some
        )
      )
    ),
    Category(
      "13",
      List(
        StartingPosition(
          "13-I",
          FEN("W:W17,18,22,23,24,25,26,28,29,30,31,32:B1,2,3,4,6,7,8,9,11,12,13,14"),
          "1. ab4 ba5 2. ed4 ab6 3. fe3 dc5",
          "ab4 ba5 ed4 ab6 fe3 dc5".some
        ),
        StartingPosition(
          "13-II",
          FEN("W:W17,21,22,23,24,25,26,27,28,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,16"),
          "",
          "a1-b4 d6-g5".some
        ),
        StartingPosition(
          "13-III",
          FEN("B:W13,19,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,12,15,16"),
          "1. ab4 de5 2. ba5 fg5 3. ef4",
          "ab4 de5 ba5 fg5 ef4".some
        ),
        StartingPosition(
          "13-IV",
          FEN("W:W13,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,21"),
          "",
          "a3-a5 f6-a3".some
        ),
        StartingPosition(
          "13-V",
          FEN("W:W14,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,7,8,9,11,12,13"),
          "1. cb4 ba5 2. bc5 db4 3. ac5 cb6",
          "cb4 ba5 bc5 db4 ac5 cb6".some
        ),
        StartingPosition(
          "13-VI",
          FEN("W:W15,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,6,7,8,9,10,11,12,14"),
          "",
          "a3-e5 a7-c5".some
        ),
        StartingPosition(
          "13-VII",
          FEN("W:W13,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,11,12,14,15"),
          "1. cb4 de5 2. ba5 bc5",
          "cb4 de5 ba5 bc5".some
        ),
        StartingPosition(
          "13-VIII",
          FEN("W:W20,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,21"),
          "",
          "a3-h4 f6-a3".some
        ),
        StartingPosition(
          "13-IX",
          FEN("B:W13,18,21,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,20"),
          "1. cb4 hg5 2. ba5 gh4 3. ed4",
          "cb4 hg5 ba5 gh4 ed4".some
        ),
        StartingPosition(
          "13-X",
          FEN("W:W14,21,22,23,24,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,12"),
          "",
          "b2-c5 ---".some
        ),
        StartingPosition(
          "13-XI",
          FEN("W:W18,19,21,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,11,12,13,15"),
          "1. cd4 ba5 2. ef4 de5",
          "cd4 ba5 ef4 de5".some
        ),
        StartingPosition(
          "13-XII",
          FEN("W:W21,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,20"),
          "",
          "--- b6-h4".some
        ),
        StartingPosition(
          "13-XIII",
          FEN("W:W18,19,21,22,24,26,27,28,29,30,31,32:B1,2,4,5,6,7,8,9,10,11,12,15"),
          "1. cd4 de5 2. bc3 ed6 3. ef4 fe7",
          "cd4 de5 bc3 ed6 ef4 fe7".some
        ),
        StartingPosition(
          "13-XIV",
          FEN("W:W18,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,15"),
          "",
          "c3-d4 h6-e5".some
        ),
        StartingPosition(
          "13-XV",
          FEN("W:W18,19,21,22,23,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,20"),
          "1. cd4 fg5 2. gf4 gf6 3. bc3 gh4",
          "cd4 fg5 gf4 gf6 bc3 gh4".some
        ),
        StartingPosition(
          "13-XVI",
          FEN("W:W20,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,14"),
          "",
          "c3-h4 g7-c5".some
        ),
        StartingPosition(
          "13-XVII",
          FEN("W:W18,21,22,23,24,25,26,27,28,29,30,31:B1,2,3,4,5,6,7,8,10,12,13,20"),
          "1. ed4 ba5 2. fe3 fg5 3. gf2 gh4",
          "ed4 ba5 fe3 fg5 gf2 gh4".some
        ),
        StartingPosition(
          "13-XVIII",
          FEN("W:W18,21,22,23,24,25,27,28,29,30,31,32:B1,2,3,4,6,7,8,9,10,11,12,20"),
          "",
          "d2-d4 a7-h4".some
        ),
        StartingPosition(
          "13-XIX",
          FEN("W:W18,20,21,22,25,26,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,16"),
          "1. ed4 de5 2. gf4 eg3 3. fh4 hg5",
          "ed4 de5 gf4 eg3 fh4 hg5".some
        ),
        StartingPosition(
          "13-XX",
          FEN("W:W13,21,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,12"),
          "",
          "e3-a5 ---".some
        ),
        StartingPosition(
          "13-XXI",
          FEN("W:W18,19,21,23,24,25,27,28,29,30,31,32:B1,2,3,4,6,7,8,9,11,12,13,14"),
          "1. ef4 ba5 2. de3 ab6 3. cd4 dc5",
          "ef4 ba5 de3 ab6 cd4 dc5".some
        ),
        StartingPosition(
          "13-XXII",
          FEN("W:W18,21,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,6,7,8,9,10,11,12,15"),
          "",
          "e3-d4 a7-e5".some
        ),
        StartingPosition(
          "13-XXIII",
          FEN("W:W19,21,22,23,24,25,26,27,28,29,30,31:B1,2,3,4,5,6,7,8,10,12,13,20"),
          "1. ef4 fg5 2. fe3 gh4 3. gf2 ba5",
          "ef4 fg5 fe3 gh4 gf2 ba5".some
        ),
        StartingPosition(
          "13-XXIV",
          FEN("W:W17,21,22,23,24,25,26,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,13"),
          "",
          "f2-b4 d6-a5".some
        ),
        StartingPosition(
          "13-XXV",
          FEN("W:W19,21,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,22"),
          "1. gf4 fe5 2. cd4 ec3",
          "gf4 fe5 cd4 ec3".some
        ),
        StartingPosition(
          "13-XXVI",
          FEN("W:W13,21,22,23,24,25,26,27,28,29,30,31:B1,2,3,4,5,6,7,8,9,10,12,17"),
          "",
          "g1-a5 f6-b4".some
        ),
        StartingPosition(
          "13-XXVII",
          FEN("W:W18,20,21,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,12,14,15"),
          "1. gh4 bc5 2. cd4 fe5",
          "gh4 bc5 cd4 fe5".some
        ),
        StartingPosition(
          "13-XXVIII",
          FEN("W:W18,21,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,6,7,8,9,10,11,12,15"),
          "",
          "g3-d4 a7-e5".some
        ),
        StartingPosition(
          "13-XXIX",
          FEN("W:W18,20,21,22,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,15"),
          "1. gh4 fe5 2. ed4 gf6",
          "gh4 fe5 ed4 gf6".some
        ),
        StartingPosition(
          "13-XXX",
          FEN("W:W12,21,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,20"),
          "",
          "g3-h6 h6-h4".some
        ),
        StartingPosition(
          "13-XXXI",
          FEN("W:W17,21,22,23,24,25,26,27,29,30,31,32:B1,2,3,4,6,7,8,9,10,11,12,28"),
          "",
          "h2-b4 a7-h2".some
        ),
        StartingPosition(
          "13-XXXII",
          FEN("W:W19,21,22,23,24,25,26,27,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,12"),
          "",
          "h2-f4 ---".some
        )
      )
    ),
    Category(
      "14",
      List(
        StartingPosition(
          "14-I",
          FEN("W:W17,18,22,23,24,25,26,28,29,30,31,32:B1,2,3,4,6,7,8,9,10,12,13,15"),
          "1. ab4 ba5 2. ed4 ab6 3. fe3 fe5",
          "ab4 ba5 ed4 ab6 fe3 fe5".some
        ),
        StartingPosition(
          "14-II",
          FEN("W:W17,21,22,23,24,25,26,27,28,30,31,32:B1,2,3,5,6,7,8,9,10,11,12,15"),
          "",
          "a1-b4 h8-e5".some
        ),
        StartingPosition(
          "14-III",
          FEN("B:W13,20,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,12,15,16"),
          "1. ab4 de5 2. ba5 fg5 3. gh4",
          "ab4 de5 ba5 fg5 gh4".some
        ),
        StartingPosition(
          "14-IV",
          FEN("W:W13,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,21"),
          "",
          "a3-a5 h6-a3".some
        ),
        StartingPosition(
          "14-V",
          FEN("W:W14,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,12,13,15"),
          "1. cb4 ba5 2. bc5 db4 3. ac5 fe5",
          "cb4 ba5 bc5 db4 ac5 fe5".some
        ),
        StartingPosition(
          "14-VI",
          FEN("W:W15,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,21"),
          "",
          "a3-e5 b6-a3".some
        ),
        StartingPosition(
          "14-VII",
          FEN("W:W17,21,22,23,24,25,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,12,15,16"),
          "1. cb4 de5 2. dc3 fg5",
          "cb4 de5 dc3 fg5".some
        ),
        StartingPosition(
          "14-VIII",
          FEN("W:W20,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,14"),
          "",
          "a3-h4 f6-c5".some
        ),
        StartingPosition(
          "14-IX",
          FEN("W:W16,17,21,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,13,20"),
          "1. cb4 hg5 2. gf4 gh4 3. fg5 ba5",
          "cb4 hg5 gf4 gh4 fg5 ba5".some
        ),
        StartingPosition(
          "14-X",
          FEN("W:W14,21,22,23,24,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,16"),
          "",
          "b2-c5 g7-g5".some
        ),
        StartingPosition(
          "14-XI",
          FEN("W:W18,19,21,23,24,25,27,28,29,30,31,32:B1,2,3,4,5,7,8,9,11,12,13,14"),
          "1. cd4 ba5 2. ef4 cb6 3. de3 dc5",
          "cd4 ba5 ef4 cb6 de3 dc5".some
        ),
        StartingPosition(
          "14-XII",
          FEN("W:W13,21,22,23,24,25,26,27,28,29,31,32:B1,2,3,4,6,7,8,9,10,11,12,14"),
          "",
          "c1-a5 a7-c5".some
        ),
        StartingPosition(
          "14-XIII",
          FEN("B:W18,19,21,22,23,26,27,28,29,30,31,32:B1,2,3,4,5,6,8,9,10,11,12,15"),
          "1. cd4 de5 2. bc3 ed6 3. gf4",
          "cd4 de5 bc3 ed6 gf4".some
        ),
        StartingPosition(
          "14-XIV",
          FEN("W:W15,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,13"),
          "",
          "c3-e5 b6-a5".some
        ),
        StartingPosition(
          "14-XV",
          FEN("W:W18,20,21,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,12,13,16"),
          "1. cd4 fg5 2. gh4 ba5",
          "cd4 fg5 gh4 ba5".some
        ),
        StartingPosition(
          "14-XVI",
          FEN("W:W20,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,14"),
          "",
          "c3-h4 h6-c5".some
        ),
        StartingPosition(
          "14-XVII",
          FEN("B:W18,21,22,23,24,25,26,27,28,29,30,31:B1,2,3,4,5,6,7,8,10,11,13,16"),
          "1. ed4 ba5 2. fe3 hg5 3. gf2",
          "ed4 ba5 fe3 hg5 gf2".some
        ),
        StartingPosition(
          "14-XVIII",
          FEN("W:W18,21,22,23,24,25,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,13"),
          "",
          "d2-d4 b6-a5".some
        ),
        StartingPosition(
          "14-XIX",
          FEN("B:W18,20,21,22,23,25,26,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,15,16"),
          "1. ed4 de5 2. gh4 hg5 3. fe3",
          "ed4 de5 gh4 hg5 fe3".some
        ),
        StartingPosition(
          "14-XX",
          FEN("W:W13,21,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,6,7,8,9,10,11,12,15"),
          "",
          "e3-a5 a7-e5".some
        ),
        StartingPosition(
          "14-XXI",
          FEN("W:W19,21,22,23,24,25,26,27,28,29,31,32:B1,2,3,4,5,7,8,9,10,12,13,15"),
          "1. ef4 ba5 2. de3 cb6 3. cd2 fe5",
          "ef4 ba5 de3 cb6 cd2 fe5".some
        ),
        StartingPosition(
          "14-XXII",
          FEN("W:W18,21,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,6,7,8,9,10,11,12,16"),
          "",
          "e3-d4 a7-g5".some
        ),
        StartingPosition(
          "14-XXIII",
          FEN("W:W19,21,22,23,24,25,26,27,28,29,30,31:B1,2,3,4,5,6,7,8,9,12,15,20"),
          "1. ef4 fg5 2. fe3 gh4 3. gf2 de5",
          "ef4 fg5 fe3 gh4 gf2 de5".some
        ),
        StartingPosition(
          "14-XXIV",
          FEN("W:W17,21,22,23,24,25,26,28,29,30,31,32:B1,3,4,5,6,7,8,9,10,11,12,20"),
          "",
          "f2-b4 d8-h4".some
        ),
        StartingPosition(
          "14-XXV",
          FEN("W:W18,21,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,24"),
          "1. gf4 fe5 2. cd4 eg3",
          "gf4 fe5 cd4 eg3".some
        ),
        StartingPosition(
          "14-XXVI",
          FEN("W:W13,21,22,23,24,25,26,27,28,29,30,31:B1,2,3,4,5,6,7,9,10,11,12,14"),
          "",
          "g1-a5 g7-c5".some
        ),
        StartingPosition(
          "14-XXVII",
          FEN("W:W19,20,21,22,25,27,28,29,30,31,32:B1,2,3,4,5,7,8,9,10,11,12"),
          "1. gh4 bc5 2. ed4 ce3 3. df4 cb6",
          "gh4 bc5 ed4 ce3 df4 cb6".some
        ),
        StartingPosition(
          "14-XXVIII",
          FEN("W:W18,21,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,6,7,8,9,10,11,12,19"),
          "",
          "g3-d4 a7-f4".some
        ),
        StartingPosition(
          "14-XXIX",
          FEN("W:W18,20,21,22,24,25,26,28,29,30,31,32:B1,2,3,4,5,6,7,9,11,12,14,15"),
          "1. gh4 fe5 2. ed4 gf6 3. fg3 dc5",
          "gh4 fe5 ed4 gf6 fg3 dc5".some
        ),
        StartingPosition(
          "14-XXX",
          FEN("W:W21,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,13"),
          "",
          "--- g7-a5".some
        ),
        StartingPosition(
          "14-XXXI",
          FEN("W:W17,21,22,23,24,25,26,27,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,14"),
          "",
          "h2-b4 b6-c5".some
        ),
        StartingPosition(
          "14-XXXII",
          FEN("W:W19,21,22,23,24,25,26,27,29,30,31,32:B1,2,3,4,6,7,8,9,10,11,12,28"),
          "",
          "h2-f4 a7-h2".some
        )
      )
    ),
    Category(
      "15",
      List(
        StartingPosition(
          "15-I",
          FEN("W:W17,18,22,23,24,25,26,28,29,30,31,32:B1,2,3,4,6,7,8,9,10,11,13,16"),
          "1. ab4 ba5 2. ed4 ab6 3. fe3 hg5",
          "ab4 ba5 ed4 ab6 fe3 hg5".some
        ),
        StartingPosition(
          "15-II",
          FEN("W:W17,21,22,23,24,25,26,27,28,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,16"),
          "",
          "a1-b4 h6-g5".some
        ),
        StartingPosition(
          "15-III",
          FEN("W:W17,21,22,23,24,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,12,15,16"),
          "1. ab4 de5 2. ba3 fg5",
          "ab4 de5 ba3 fg5".some
        ),
        StartingPosition(
          "15-IV",
          FEN("W:W13,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,17"),
          "",
          "a3-a5 h6-b4".some
        ),
        StartingPosition(
          "15-V",
          FEN("W:W17,20,21,22,23,25,27,28,29,30,31,32:B1,2,3,4,5,7,8,9,10,12,13,15"),
          "1. cb4 ba5 2. dc3 fe5 3. gh4 cb6",
          "cb4 ba5 dc3 fe5 gh4 cb6".some
        ),
        StartingPosition(
          "15-VI",
          FEN("W:W15,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,21"),
          "",
          "a3-e5 g7-a3".some
        ),
        StartingPosition(
          "15-VII",
          FEN("W:W17,19,21,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,11,12,13,15"),
          "1. cb4 de5 2. ef4 ba5",
          "cb4 de5 ef4 ba5".some
        ),
        StartingPosition(
          "15-VIII",
          FEN("W:W20,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,15"),
          "",
          "a3-h4 f6-e5".some
        ),
        StartingPosition(
          "15-IX",
          FEN("W:W17,20,21,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,13,16"),
          "1. cb4 hg5 2. gh4 ba5",
          "cb4 hg5 gh4 ba5".some
        ),
        StartingPosition(
          "15-X",
          FEN("W:W18,21,22,23,24,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,12"),
          "",
          "b2-d4 ---".some
        ),
        StartingPosition(
          "15-XI",
          FEN("W:W18,19,21,23,24,25,26,28,29,30,31,32:B1,2,3,4,5,7,8,9,10,11,12,17"),
          "1. cd4 ba5 2. ef4 cb6 3. fe3 ab4",
          "cd4 ba5 ef4 cb6 fe3 ab4".some
        ),
        StartingPosition(
          "15-XII",
          FEN("W:W13,21,22,23,24,25,26,27,28,29,31,32:B1,2,4,5,6,7,8,9,10,11,12,20"),
          "",
          "c1-a5 f8-h4".some
        ),
        StartingPosition(
          "15-XIII",
          FEN("W:W18,21,22,24,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,12,20"),
          "1. cd4 de5 2. bc3 ef4 3. eg5 fh4",
          "cd4 de5 bc3 ef4 eg5 fh4".some
        ),
        StartingPosition(
          "15-XIV",
          FEN("W:W15,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,13"),
          "",
          "c3-e5 g7-a5".some
        ),
        StartingPosition(
          "15-XV",
          FEN("W:W18,20,21,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,12,14,16"),
          "1. cd4 fg5 2. gh4 dc5",
          "cd4 fg5 gh4 dc5".some
        ),
        StartingPosition(
          "15-XVI",
          FEN("W:W12,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,13"),
          "",
          "c3-h6 h6-a5".some
        ),
        StartingPosition(
          "15-XVII",
          FEN("W:W18,19,21,22,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,12,13,16"),
          "1. ed4 ba5 2. gf4 fg5",
          "ed4 ba5 gf4 fg5".some
        ),
        StartingPosition(
          "15-XVIII",
          FEN("W:W18,21,22,23,24,25,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,16"),
          "",
          "d2-d4 b6-g5".some
        ),
        StartingPosition(
          "15-XIX",
          FEN("W:W18,21,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,15"),
          "1. ed4 fe5",
          "ed4 fe5".some
        ),
        StartingPosition(
          "15-XX",
          FEN("W:W13,21,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,6,7,8,9,10,11,12,16"),
          "",
          "e3-a5 a7-g5".some
        ),
        StartingPosition(
          "15-XXI",
          FEN("W:W19,21,22,23,24,25,26,27,28,29,30,31:B1,2,3,4,6,7,8,9,10,12,13,16"),
          "1. ef4 ba5 2. fe3 ab6 3. gf2 fg5",
          "ef4 ba5 fe3 ab6 gf2 fg5".some
        ),
        StartingPosition(
          "15-XXII",
          FEN("W:W18,21,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,13"),
          "",
          "e3-d4 d6-a5".some
        ),
        StartingPosition(
          "15-XXIII",
          FEN("B:W19,21,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,12"),
          "1. gf4",
          "gf4".some
        ),
        StartingPosition(
          "15-XXIV",
          FEN("W:W17,21,22,23,24,25,26,28,29,30,31,32:B1,2,3,4,5,6,8,9,10,11,12,15"),
          "",
          "f2-b4 e7-e5".some
        ),
        StartingPosition(
          "15-XXV",
          FEN("B:W18,19,21,22,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,15"),
          "1. gf4 fe5 2. ed4",
          "gf4 fe5 ed4".some
        ),
        StartingPosition(
          "15-XXVI",
          FEN("W:W17,21,22,23,24,25,26,27,28,29,30,31:B1,2,3,4,5,6,8,9,10,11,12,14"),
          "",
          "g1-b4 e7-c5".some
        ),
        StartingPosition(
          "15-XXVII",
          FEN("W:W18,20,21,22,25,26,28,29,30,31,32:B1,2,3,4,5,6,7,8,11,12,15"),
          "1. gh4 bc5 2. ed4 ce3 3. fd4 de5",
          "gh4 bc5 ed4 ce3 fd4 de5".some
        ),
        StartingPosition(
          "15-XXVIII",
          FEN("W:W18,21,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,14"),
          "",
          "g3-d4 b6-c5".some
        ),
        StartingPosition(
          "15-XXIX",
          FEN("W:W19,20,21,22,25,26,27,29,30,31,32:B1,2,3,4,5,6,7,8,10,12,13"),
          "1. gh4 fe5 2. ef4 eg3 3. hf4 ba5",
          "gh4 fe5 ef4 eg3 hf4 ba5".some
        ),
        StartingPosition(
          "15-XXX",
          FEN("W:W21,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,15"),
          "",
          "--- g7-e5".some
        ),
        StartingPosition(
          "15-XXXI",
          FEN("W:W17,21,22,23,24,25,26,27,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,28"),
          "",
          "h2-b4 b6-h2".some
        ),
        StartingPosition(
          "15-XXXII",
          FEN("W:W19,21,22,23,24,25,26,27,29,30,31,32:B1,2,3,4,5,7,8,9,10,11,12,20"),
          "",
          "h2-f4 c7-h4".some
        )
      )
    ),
    Category(
      "16",
      List(
        StartingPosition(
          "16-I",
          FEN("W:W17,18,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,11,12,13,14"),
          "1. ab4 ba5 2. ed4 dc5",
          "ab4 ba5 ed4 dc5".some
        ),
        StartingPosition(
          "16-II",
          FEN("W:W18,21,22,23,24,25,26,27,28,30,31,32:B1,2,3,4,6,7,8,9,10,11,12,14"),
          "",
          "a1-d4 a7-c5".some
        ),
        StartingPosition(
          "16-III",
          FEN("B:W17,19,21,22,23,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,12,15,16"),
          "1. ab4 de5 2. ba3 fg5 3. gf4",
          "ab4 de5 ba3 fg5 gf4".some
        ),
        StartingPosition(
          "16-IV",
          FEN("W:W13,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,15"),
          "",
          "a3-a5 h6-e5".some
        ),
        StartingPosition(
          "16-V",
          FEN("W:W17,21,22,23,24,25,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,13,16"),
          "1. cb4 ba5 2. dc3 hg5",
          "cb4 ba5 dc3 hg5".some
        ),
        StartingPosition(
          "16-VI",
          FEN("W:W15,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,5,6,7,8,9,10,11,12,21"),
          "",
          "a3-e5 h8-a3".some
        ),
        StartingPosition(
          "16-VII",
          FEN("B:W17,19,21,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,15"),
          "1. cb4 de5 2. gf4",
          "cb4 de5 gf4".some
        ),
        StartingPosition(
          "16-VIII",
          FEN("W:W20,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,21"),
          "",
          "a3-h4 g7-a3".some
        ),
        StartingPosition(
          "16-IX",
          FEN("W:W17,20,21,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,15,16"),
          "1. cb4 hg5 2. gh4 de5",
          "cb4 hg5 gh4 de5".some
        ),
        StartingPosition(
          "16-X",
          FEN("W:W18,21,22,23,24,26,27,28,29,30,31,32:B1,2,3,4,6,7,8,9,10,11,12,16"),
          "",
          "b2-d4 a7-g5".some
        ),
        StartingPosition(
          "16-XI",
          FEN("W:W18,19,21,23,24,25,26,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,12,16,17"),
          "1. cd4 ba5 2. ef4 fg5 3. fe3 ab4",
          "cd4 ba5 ef4 fg5 fe3 ab4".some
        ),
        StartingPosition(
          "16-XII",
          FEN("W:W20,21,22,23,24,25,26,27,28,29,31,32:B1,2,3,4,5,6,7,8,9,10,12,16"),
          "",
          "c1-h4 f6-g5".some
        ),
        StartingPosition(
          "16-XIII",
          FEN("B:W18,21,22,23,24,25,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,15"),
          "1. cd4 de5 2. dc3",
          "cd4 de5 dc3".some
        ),
        StartingPosition(
          "16-XIV",
          FEN("W:W15,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,16"),
          "",
          "c3-e5 g7-g5".some
        ),
        StartingPosition(
          "16-XV",
          FEN("W:W18,21,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,8,9,12,14,16"),
          "1. cd4 fg5 2. gh4 dc5 3. hf6 eg5",
          "cd4 fg5 gh4 dc5 hf6 eg5".some
        ),
        StartingPosition(
          "16-XVI",
          FEN("W:W21,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,7,8,9,10,11,12,13"),
          "",
          "--- c7-a5".some
        ),
        StartingPosition(
          "16-XVII",
          FEN("W:W18,21,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,14"),
          "1. ed4 bc5",
          "ed4 bc5".some
        ),
        StartingPosition(
          "16-XVIII",
          FEN("W:W18,21,22,23,24,25,27,28,29,30,31,32:B1,2,3,4,5,7,8,9,10,11,12,20"),
          "",
          "d2-d4 c7-h4".some
        ),
        StartingPosition(
          "16-XIX",
          FEN("W:W17,22,24,25,26,27,28,29,30,31,32:B1,2,3,5,6,7,8,9,10,12,15"),
          "1. ed4 fe5 2. df6 ge5 3. ab4 hg7",
          "ed4 fe5 df6 ge5 ab4 hg7".some
        ),
        StartingPosition(
          "16-XX",
          FEN("W:W13,21,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,14"),
          "",
          "e3-a5 b6-c5".some
        ),
        StartingPosition(
          "16-XXI",
          FEN("B:W18,19,21,23,24,25,26,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,12,13,15"),
          "1. ef4 ba5 2. fe3 fe5 3. cd4",
          "ef4 ba5 fe3 fe5 cd4".some
        ),
        StartingPosition(
          "16-XXII",
          FEN("W:W18,21,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,20"),
          "",
          "e3-d4 d6-h4".some
        ),
        StartingPosition(
          "16-XXIII",
          FEN("W:W19,21,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,13"),
          "1. gf4 ba5",
          "gf4 ba5".some
        ),
        StartingPosition(
          "16-XXIV",
          FEN("W:W17,21,22,23,24,25,26,28,29,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,15"),
          "",
          "f2-b4 g7-e5".some
        ),
        StartingPosition(
          "16-XXV",
          FEN("W:W18,19,21,22,25,26,27,29,30,31,32:B1,2,3,4,5,6,7,8,10,12,13"),
          "1. gf4 fe5 2. ed4 eg3 3. hf4 ba5",
          "gf4 fe5 ed4 eg3 hf4 ba5".some
        ),
        StartingPosition(
          "16-XXVI",
          FEN("W:W17,21,22,23,24,25,26,27,28,29,30,31:B1,2,3,5,6,7,8,9,10,11,12,14"),
          "",
          "g1-b4 h8-c5".some
        ),
        StartingPosition(
          "16-XXVII",
          FEN("W:W18,20,21,23,24,25,26,28,29,30,31,32:B1,2,3,4,6,7,8,9,10,12,14,16"),
          "1. gh4 bc5 2. fg3 ab6 3. cd4 fg5",
          "gh4 bc5 fg3 ab6 cd4 fg5".some
        ),
        StartingPosition(
          "16-XXVIII",
          FEN("W:W18,21,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,13"),
          "",
          "g3-d4 d6-a5".some
        ),
        StartingPosition(
          "16-XXIX",
          FEN("B:W20,21,22,23,24,25,26,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,15"),
          "1. gh4 fe5 2. fg3",
          "gh4 fe5 fg3".some
        ),
        StartingPosition(
          "16-XXX",
          FEN("W:W21,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,16"),
          "",
          "--- g7-g5".some
        ),
        StartingPosition(
          "16-XXXI",
          FEN("W:W17,21,22,23,24,25,26,27,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,16"),
          "",
          "h2-b4 d6-g5".some
        ),
        StartingPosition(
          "16-XXXII",
          FEN("W:W19,21,22,23,24,25,26,27,29,30,31,32:B1,2,3,4,5,7,8,9,10,11,12,28"),
          "",
          "h2-f4 c7-h2".some
        )
      )
    ),
    Category(
      "17",
      List(
        StartingPosition(
          "17-I",
          FEN("W:W18,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,7,8,11,12,13,15"),
          "1. ab4 ba5 2. ed4 dc5 3. bd6 ce5",
          "ab4 ba5 ed4 dc5 bd6 ce5".some
        ),
        StartingPosition(
          "17-II",
          FEN("W:W18,21,22,23,24,25,26,27,28,30,31,32:B1,2,3,4,6,7,8,9,10,11,12,19"),
          "",
          "a1-d4 a7-f4".some
        ),
        StartingPosition(
          "17-III",
          FEN("B:W13,18,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,15,16"),
          "1. ab4 de5 2. ed4 hg5 3. ba5",
          "ab4 de5 ed4 hg5 ba5".some
        ),
        StartingPosition(
          "17-IV",
          FEN("W:W17,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,21"),
          "",
          "a3-b4 b6-a3".some
        ),
        StartingPosition(
          "17-V",
          FEN("B:W17,19,21,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,13"),
          "1. cb4 ba5 2. ef4",
          "cb4 ba5 ef4".some
        ),
        StartingPosition(
          "17-VI",
          FEN("W:W19,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,12"),
          "",
          "a3-f4 ---".some
        ),
        StartingPosition(
          "17-VII",
          FEN("W:W17,19,21,23,25,26,27,29,30,31,32:B1,2,3,4,5,6,7,8,9,12,15"),
          "1. cb4 de5 2. gf4 eg3 3. hf4 fe5",
          "cb4 de5 gf4 eg3 hf4 fe5".some
        ),
        StartingPosition(
          "17-VIII",
          FEN("W:W20,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,13"),
          "",
          "a3-h4 g7-a5".some
        ),
        StartingPosition(
          "17-IX",
          FEN("B:W14,20,21,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,16"),
          "1. cb4 hg5 2. gh4 gh6 3. bc5",
          "cb4 hg5 gh4 gh6 bc5".some
        ),
        StartingPosition(
          "17-X",
          FEN("W:W18,21,22,23,24,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,14"),
          "",
          "b2-d4 b6-c5".some
        ),
        StartingPosition(
          "17-XI",
          FEN("W:W18,19,21,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,11,12,13,15"),
          "1. cd4 ba5 2. gf4 de5",
          "cd4 ba5 gf4 de5".some
        ),
        StartingPosition(
          "17-XII",
          FEN("W:W13,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,6,7,8,9,10,11,12,16"),
          "",
          "c3-a5 a7-g5".some
        ),
        StartingPosition(
          "17-XIII",
          FEN("W:W19,21,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,22"),
          "1. cd4 de5 2. gf4 ec3",
          "cd4 de5 gf4 ec3".some
        ),
        StartingPosition(
          "17-XIV",
          FEN("W:W15,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,20"),
          "",
          "c3-e5 g7-h4".some
        ),
        StartingPosition(
          "17-XV",
          FEN("W:W18,20,21,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,8,9,10,11,12,16"),
          "1. cd4 fg5 2. gh4 ef6",
          "cd4 fg5 gh4 ef6".some
        ),
        StartingPosition(
          "17-XVI",
          FEN("W:W21,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,7,8,9,10,11,12,16"),
          "",
          "--- c7-g5".some
        ),
        StartingPosition(
          "17-XVII",
          FEN("W:W21,22,23,24,25,27,28,29,30,31,32:B1,2,3,4,6,7,8,10,11,14,16"),
          "1. ed4 bc5 2. db6 ac5 3. de3 hg5",
          "ed4 bc5 db6 ac5 de3 hg5".some
        ),
        StartingPosition(
          "17-XVIII",
          FEN("W:W18,21,22,23,24,25,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,20"),
          "",
          "d2-d4 d6-h4".some
        ),
        StartingPosition(
          "17-XIX",
          FEN("W:W17,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,9,10,15,16"),
          "1. ed4 fe5 2. df6 ge5 3. ab4 hg5",
          "ed4 fe5 df6 ge5 ab4 hg5".some
        ),
        StartingPosition(
          "17-XX",
          FEN("W:W13,21,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,15"),
          "",
          "e3-a5 d6-e5".some
        ),
        StartingPosition(
          "17-XXI",
          FEN("B:W19,20,21,22,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,13"),
          "1. ef4 ba5 2. gh4",
          "ef4 ba5 gh4".some
        ),
        StartingPosition(
          "17-XXII",
          FEN("W:W19,21,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,12"),
          "",
          "e3-f4 ---".some
        ),
        StartingPosition(
          "17-XXIII",
          FEN("W:W18,19,21,23,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,12,16"),
          "1. gf4 ba5 2. cb4 ac3 3. bd4 fg5",
          "gf4 ba5 cb4 ac3 bd4 fg5".some
        ),
        StartingPosition(
          "17-XXIV",
          FEN("W:W17,21,22,23,24,25,26,28,29,30,31,32:B1,2,3,5,6,7,8,9,10,11,12,14"),
          "",
          "f2-b4 h8-c5".some
        ),
        StartingPosition(
          "17-XXV",
          FEN("B:W19,21,22,23,24,25,26,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,15"),
          "1. gf4 fe5 2. fg3",
          "gf4 fe5 fg3".some
        ),
        StartingPosition(
          "17-XXVI",
          FEN("W:W18,21,22,23,24,25,26,27,28,29,30,31:B1,2,3,4,5,6,7,8,9,11,12,16"),
          "",
          "g1-d4 d6-g5".some
        ),
        StartingPosition(
          "17-XXVII",
          FEN("W:W20,21,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,14"),
          "1. gh4 dc5",
          "gh4 dc5".some
        ),
        StartingPosition(
          "17-XXVIII",
          FEN("W:W18,21,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,15"),
          "",
          "g3-d4 d6-e5".some
        ),
        StartingPosition(
          "17-XXIX",
          FEN("W:W20,21,22,23,24,25,26,28,29,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,15"),
          "1. gh4 fe5 2. fg3 gf6",
          "gh4 fe5 fg3 gf6".some
        ),
        StartingPosition(
          "17-XXX",
          FEN("W:W21,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,20"),
          "",
          "--- g7-h4".some
        ),
        StartingPosition(
          "17-XXXI",
          FEN("W:W17,21,22,23,24,25,26,27,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,28"),
          "",
          "h2-b4 d6-h2".some
        ),
        StartingPosition(
          "17-XXXII",
          FEN("W:W19,21,22,23,24,25,26,27,29,30,31,32:B1,2,3,4,5,6,8,9,10,11,12,28"),
          "",
          "h2-f4 e7-h2".some
        )
      )
    ),
    Category(
      "18",
      List(
        StartingPosition(
          "18-I",
          FEN("W:W17,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,10,12,13,15"),
          "1. ab4 ba5 2. ed4 fe5 3. df6 ge5",
          "ab4 ba5 ed4 fe5 df6 ge5".some
        ),
        StartingPosition(
          "18-II",
          FEN("W:W18,21,22,23,24,25,26,27,28,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,13"),
          "",
          "a1-d4 b6-a5".some
        ),
        StartingPosition(
          "18-III",
          FEN("B:W17,20,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,15"),
          "1. ab4 de5 2. gh4",
          "ab4 de5 gh4".some
        ),
        StartingPosition(
          "18-IV",
          FEN("W:W17,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,21"),
          "",
          "a3-b4 d6-a3".some
        ),
        StartingPosition(
          "18-V",
          FEN("B:W17,19,21,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,13"),
          "1. cb4 ba5 2. gf4",
          "cb4 ba5 gf4".some
        ),
        StartingPosition(
          "18-VI",
          FEN("W:W19,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,6,7,8,9,10,11,12,14"),
          "",
          "a3-f4 a7-c5".some
        ),
        StartingPosition(
          "18-VII",
          FEN("W:W17,20,21,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,7,8,9,10,11,12,15"),
          "1. cb4 de5 2. gh4 cd6",
          "cb4 de5 gh4 cd6".some
        ),
        StartingPosition(
          "18-VIII",
          FEN("W:W20,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,14"),
          "",
          "a3-h4 g7-c5".some
        ),
        StartingPosition(
          "18-IX",
          FEN("B:W17,20,21,23,24,25,26,27,29,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,16"),
          "1. cb4 hg5 2. gh4 gh6 3. hg3",
          "cb4 hg5 gh4 gh6 hg3".some
        ),
        StartingPosition(
          "18-X",
          FEN("W:W18,21,22,23,24,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,14"),
          "",
          "b2-d4 f6-c5".some
        ),
        StartingPosition(
          "18-XI",
          FEN("W:W19,21,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,8,10,12,13,16"),
          "1. cd4 ba5 2. gf4 fe5 3. df6 eg5",
          "cd4 ba5 gf4 fe5 df6 eg5".some
        ),
        StartingPosition(
          "18-XII",
          FEN("W:W13,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,16"),
          "",
          "c3-a5 d6-g5".some
        ),
        StartingPosition(
          "18-XIII",
          FEN("W:W18,21,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,24"),
          "1. cd4 de5 2. gf4 eg3",
          "cd4 de5 gf4 eg3".some
        ),
        StartingPosition(
          "18-XIV",
          FEN("W:W19,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,12"),
          "",
          "c3-f4 ---".some
        ),
        StartingPosition(
          "18-XV",
          FEN("W:W18,20,21,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,19"),
          "1. cd4 fg5 2. gh4 gf4",
          "cd4 fg5 gh4 gf4".some
        ),
        StartingPosition(
          "18-XVI",
          FEN("W:W13,21,22,23,24,25,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,12"),
          "",
          "d2-a5 ---".some
        ),
        StartingPosition(
          "18-XVII",
          FEN("W:W19,21,22,25,26,27,28,29,30,31,32:B1,2,3,4,6,7,8,10,12,14,15"),
          "1. ed4 bc5 2. db6 ac5 3. gf4 fe5",
          "ed4 bc5 db6 ac5 gf4 fe5".some
        ),
        StartingPosition(
          "18-XVIII",
          FEN("W:W18,21,22,23,24,25,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,15"),
          "",
          "d2-d4 f6-e5".some
        ),
        StartingPosition(
          "18-XIX",
          FEN("W:W20,21,22,25,26,27,28,29,30,31,32:B1,2,3,5,6,7,8,9,10,12,15"),
          "1. ed4 fe5 2. df6 ge5 3. gh4 hg7",
          "ed4 fe5 df6 ge5 gh4 hg7".some
        ),
        StartingPosition(
          "18-XX",
          FEN("B:W13,21,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,8,9,10,11,12,16"),
          "",
          "e3-a5 e7-g5".some
        ),
        StartingPosition(
          "18-XXI",
          FEN("B:W17,19,21,23,24,25,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,12,14,16"),
          "1. ef4 bc5 2. cb4 fg5 3. de3",
          "ef4 bc5 cb4 fg5 de3".some
        ),
        StartingPosition(
          "18-XXII",
          FEN("W:W19,21,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,6,7,8,9,10,11,12,15"),
          "",
          "e3-f4 a7-e5".some
        ),
        StartingPosition(
          "18-XXIII",
          FEN("W:W17,19,21,23,25,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,11,12,15"),
          "1. gf4 ba5 2. cb4 ac3 3. db4 de5",
          "gf4 ba5 cb4 ac3 db4 de5".some
        ),
        StartingPosition(
          "18-XXIV",
          FEN("W:W17,21,22,23,24,25,26,28,29,30,31,32:B1,2,3,5,6,7,8,9,10,11,12,16"),
          "",
          "f2-b4 h8-g5".some
        ),
        StartingPosition(
          "18-XXV",
          FEN("B:W17,19,22,23,24,25,26,28,29,30,31,32:B1,2,3,4,5,6,8,9,10,11,12,15"),
          "1. gf4 fe5 2. fg3 ef6 3. ab4",
          "gf4 fe5 fg3 ef6 ab4".some
        ),
        StartingPosition(
          "18-XXVI",
          FEN("W:W18,21,22,23,24,25,26,27,28,29,30,31:B1,2,3,4,5,6,7,8,9,10,11,13"),
          "",
          "g1-d4 h6-a5".some
        ),
        StartingPosition(
          "18-XXVII",
          FEN("W:W17,20,21,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,18"),
          "1. gh4 dc5 1. cb4 cd4",
          "gh4 dc5 cb4 cd4".some
        ),
        StartingPosition(
          "18-XXVIII",
          FEN("W:W18,21,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,13"),
          "",
          "g3-d4 g7-a5".some
        ),
        StartingPosition(
          "18-XXIX",
          FEN("W:W20,21,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,16"),
          "1. gh4 fg5",
          "gh4 fg5".some
        ),
        StartingPosition(
          "18-XXX",
          FEN("W:W13,21,22,23,24,25,26,27,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,12"),
          "",
          "h2-a5 ---".some
        ),
        StartingPosition(
          "18-XXXI",
          FEN("W:W17,21,22,23,24,25,26,27,29,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,28"),
          "",
          "h2-b4 g7-h2".some
        ),
        StartingPosition(
          "18-XXXII",
          FEN("W:W19,21,22,23,24,25,26,27,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,15"),
          "",
          "h2-f4 f6-e5".some
        )
      )
    ),
    Category(
      "19",
      List(
        StartingPosition(
          "19-I",
          FEN("W:W17,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,8,10,12,13,16"),
          "1. ab4 ba5 2. ed4 fe5 3. df6 eg5",
          "ab4 ba5 ed4 fe5 df6 eg5".some
        ),
        StartingPosition(
          "19-II",
          FEN("W:W18,21,22,23,24,25,26,27,28,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,14"),
          "",
          "a1-d4 b6-c5".some
        ),
        StartingPosition(
          "19-III",
          FEN("W:W17,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,15"),
          "1. ab4 fe5",
          "ab4 fe5".some
        ),
        StartingPosition(
          "19-IV",
          FEN("W:W17,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,8,9,10,11,12,13"),
          "",
          "a3-b4 e7-a5".some
        ),
        StartingPosition(
          "19-V",
          FEN("W:W18,20,21,23,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,11,12,15"),
          "1. cb4 ba5 2. gh4 ac3 3. bd4 de5",
          "cb4 ba5 gh4 ac3 bd4 de5".some
        ),
        StartingPosition(
          "19-VI",
          FEN("W:W19,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,7,8,9,10,11,12,14"),
          "",
          "a3-f4 c7-c5".some
        ),
        StartingPosition(
          "19-VII",
          FEN("W:W13,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,12,14,15"),
          "1. cb4 fe5 2. ba5 bc5",
          "cb4 fe5 ba5 bc5".some
        ),
        StartingPosition(
          "19-VIII",
          FEN("W:W20,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,15"),
          "",
          "a3-h4 g7-e5".some
        ),
        StartingPosition(
          "19-IX",
          FEN("B:W18,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,12"),
          "1. cd4",
          "cd4".some
        ),
        StartingPosition(
          "19-X",
          FEN("W:W18,21,22,23,24,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,15"),
          "",
          "b2-d4 f6-e5".some
        ),
        StartingPosition(
          "19-XI",
          FEN("B:W14,20,21,23,25,26,27,28,29,30,31,32:B1,2,3,4,6,7,8,9,10,11,12,13"),
          "1. cd4 ba5 2. gh4 ab6 3. dc5",
          "cd4 ba5 gh4 ab6 dc5".some
        ),
        StartingPosition(
          "19-XII",
          FEN("W:W13,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,20"),
          "",
          "c3-a5 d6-h4".some
        ),
        StartingPosition(
          "19-XIII",
          FEN("W:W18,20,21,23,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,12,15"),
          "1. cd4 de5 2. gh4 ec3 3. bd4 fe5",
          "cd4 de5 gh4 ec3 bd4 fe5".some
        ),
        StartingPosition(
          "19-XIV",
          FEN("W:W19,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,6,7,8,9,10,11,12,15"),
          "",
          "c3-f4 a7-e5".some
        ),
        StartingPosition(
          "19-XV",
          FEN("W:W18,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,16"),
          "1. cd4 hg5",
          "cd4 hg5".some
        ),
        StartingPosition(
          "19-XVI",
          FEN("W:W13,21,22,23,24,25,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,20"),
          "",
          "d2-a5 b6-h4".some
        ),
        StartingPosition(
          "19-XVII",
          FEN("W:W20,21,22,25,26,27,28,29,30,31,32:B1,2,3,4,6,7,8,11,12,14,15"),
          "1. ed4 bc5 2. db6 ac5 3. gh4 de5",
          "ed4 bc5 db6 ac5 gh4 de5".some
        ),
        StartingPosition(
          "19-XVIII",
          FEN("W:W18,21,22,23,24,25,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,16"),
          "",
          "d2-d4 f6-g5".some
        ),
        StartingPosition(
          "19-XIX",
          FEN("B:W21,22,23,24,25,26,28,29,30,31,32:B1,2,3,4,5,6,8,9,10,12,16"),
          "1. ed4 fe5 2. df6 eg5 3. fe3",
          "ed4 fe5 df6 eg5 fe3".some
        ),
        StartingPosition(
          "19-XX",
          FEN("W:W13,21,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,17"),
          "",
          "e3-a5 f6-b4".some
        ),
        StartingPosition(
          "19-XXI",
          FEN("W:W18,19,21,23,24,25,27,28,29,30,31,32:B1,2,3,4,6,7,8,9,10,12,14,16"),
          "1. ef4 bc5 2. de3 ab6 3. cd4 fg5",
          "ef4 bc5 de3 ab6 cd4 fg5".some
        ),
        StartingPosition(
          "19-XXII",
          FEN("W:W19,21,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,13"),
          "",
          "e3-f4 d6-a5".some
        ),
        StartingPosition(
          "19-XXIII",
          FEN("W:W17,19,21,23,25,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,12,16"),
          "1. gf4 ba5 2. cb4 ac3 3. db4 fg5",
          "gf4 ba5 cb4 ac3 db4 fg5".some
        ),
        StartingPosition(
          "19-XXIV",
          FEN("W:W14,21,22,23,24,25,26,28,29,30,31,32:B1,2,3,4,6,7,8,9,10,11,12,27"),
          "",
          "f2-c5 a7-f2".some
        ),
        StartingPosition(
          "19-XXV",
          FEN("W:W17,19,21,23,24,25,26,27,29,30,31,32:B1,2,3,4,6,7,8,9,10,12,14,15"),
          "1. gf4 fe5 2. hg3 bc5 3. cb4 ab6",
          "gf4 fe5 hg3 bc5 cb4 ab6".some
        ),
        StartingPosition(
          "19-XXVI",
          FEN("W:W16,21,22,23,24,25,26,27,28,29,30,31:B1,2,3,4,5,6,7,9,10,11,12,13"),
          "",
          "g1-g5 g7-a5".some
        ),
        StartingPosition(
          "19-XXVII",
          FEN("W:W17,20,21,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,12,14,16"),
          "1. gh4 dc5 2. cb4 fg5",
          "gh4 dc5 cb4 fg5".some
        ),
        StartingPosition(
          "19-XXVIII",
          FEN("W:W15,21,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,20"),
          "",
          "g3-e5 g7-h4".some
        ),
        StartingPosition(
          "19-XXIX",
          FEN("W:W17,22,23,25,26,27,28,29,30,31,32:B1,2,4,5,6,7,8,9,10,12,15"),
          "1. gh4 fg5 2. hf6 ge5 3. ab4 fg7",
          "gh4 fg5 hf6 ge5 ab4 fg7".some
        ),
        StartingPosition(
          "19-XXX",
          FEN("W:W13,21,22,23,24,25,26,27,29,30,31,32:B1,2,3,4,6,7,8,9,10,11,12,16"),
          "",
          "h2-a5 a7-g5".some
        ),
        StartingPosition(
          "19-XXXI",
          FEN("W:W17,21,22,23,24,25,26,27,29,30,31,32:B1,2,3,5,6,7,8,9,10,11,12,28"),
          "",
          "h2-b4 h8-h2".some
        ),
        StartingPosition(
          "19-XXXII",
          FEN("W:W19,21,22,23,24,25,26,27,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,14"),
          "",
          "h2-f4 h6-c5".some
        )
      )
    ),
    Category(
      "20",
      List(
        StartingPosition(
          "20-I",
          FEN("W:W17,18,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,12,13,16"),
          "1. ab4 ba5 2. ed4 fg5",
          "ab4 ba5 ed4 fg5".some
        ),
        StartingPosition(
          "20-II",
          FEN("W:W18,21,22,23,24,25,26,27,28,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,15"),
          "",
          "a1-d4 f6-e5".some
        ),
        StartingPosition(
          "20-III",
          FEN("W:W13,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,19"),
          "1. ab4 fe5 2. ba5 ef4",
          "ab4 fe5 ba5 ef4".some
        ),
        StartingPosition(
          "20-IV",
          FEN("W:W17,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,21"),
          "",
          "a3-b4 h6-a3".some
        ),
        StartingPosition(
          "20-V",
          FEN("W:W17,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,14"),
          "1. cb4 bc5",
          "cb4 bc5".some
        ),
        StartingPosition(
          "20-VI",
          FEN("W:W16,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,6,7,8,9,10,11,12,21"),
          "",
          "a3-g5 a7-a3".some
        ),
        StartingPosition(
          "20-VII",
          FEN("B:W17,21,22,23,24,25,26,27,28,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,15"),
          "1. cb4 fe5 2. bc3 gf6 3. ab2",
          "cb4 fe5 bc3 gf6 ab2".some
        ),
        StartingPosition(
          "20-VIII",
          FEN("W:W20,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,21"),
          "",
          "a3-h4 h6-a3".some
        ),
        StartingPosition(
          "20-IX",
          FEN("B:W18,21,22,23,24,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,13"),
          "1. cd4 ba5 2. bc3",
          "cd4 ba5 bc3".some
        ),
        StartingPosition(
          "20-X",
          FEN("W:W18,21,22,23,24,26,27,28,29,30,31,32:B1,2,3,5,6,7,8,9,10,11,12,20"),
          "",
          "b2-d4 h8-h4".some
        ),
        StartingPosition(
          "20-XI",
          FEN("W:W18,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,14"),
          "1. cd4 bc5",
          "cd4 bc5".some
        ),
        StartingPosition(
          "20-XII",
          FEN("W:W13,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,15"),
          "",
          "c3-a5 f6-e5".some
        ),
        StartingPosition(
          "20-XIII",
          FEN("W:W18,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,15"),
          "1. cd4 fe5",
          "cd4 fe5".some
        ),
        StartingPosition(
          "20-XIV",
          FEN("W:W19,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,17"),
          "",
          "c3-f4 f6-b4".some
        ),
        StartingPosition(
          "20-XV",
          FEN("W:W18,21,22,23,24,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,19"),
          "1. cd4 hg5 2. bc3 gf4",
          "cd4 hg5 bc3 gf4".some
        ),
        StartingPosition(
          "20-XVI",
          FEN("W:W13,21,22,23,24,25,27,28,29,30,31,32:B1,2,3,4,5,6,8,9,10,11,12,16"),
          "",
          "d2-a5 e7-g5".some
        ),
        StartingPosition(
          "20-XVII",
          FEN("W:W21,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,7,8,10,11,12,13"),
          "1. ed4 bc5 2. db6 ca5",
          "ed4 bc5 db6 ca5".some
        ),
        StartingPosition(
          "20-XVIII",
          FEN("W:W18,21,22,23,24,25,27,28,29,30,31,32:B1,2,3,5,6,7,8,9,10,11,12,20"),
          "",
          "d2-d4 h8-h4".some
        ),
        StartingPosition(
          "20-XIX",
          FEN("W:W18,21,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,16"),
          "1. ed4 fg5",
          "ed4 fg5".some
        ),
        StartingPosition(
          "20-XX",
          FEN("W:W13,21,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,17"),
          "",
          "e3-a5 g7-b4".some
        ),
        StartingPosition(
          "20-XXI",
          FEN("W:W18,19,21,23,24,25,26,28,29,30,31,32:B1,2,3,4,5,7,8,9,10,12,14,15"),
          "1. ef4 bc5 2. fe3 cb6 3. cd4 fe5",
          "ef4 bc5 fe3 cb6 cd4 fe5".some
        ),
        StartingPosition(
          "20-XXII",
          FEN("W:W19,21,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,14"),
          "",
          "e3-f4 f6-c5".some
        ),
        StartingPosition(
          "20-XXIII",
          FEN("W:W19,21,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,8,10,12,13,16"),
          "1. gf4 ba5 2. cd4 fe5 3. df6 eg5",
          "gf4 ba5 cd4 fe5 df6 eg5".some
        ),
        StartingPosition(
          "20-XXIV",
          FEN("W:W14,21,22,23,24,25,26,28,29,30,31,32:B1,2,3,5,6,7,8,9,10,11,12,15"),
          "",
          "f2-c5 h8-e5".some
        ),
        StartingPosition(
          "20-XXV",
          FEN("B:W14,19,21,22,24,25,26,27,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12"),
          "1. gf4 fe5 2. hg3 ed4 3. ec5",
          "gf4 fe5 hg3 ed4 ec5".some
        ),
        StartingPosition(
          "20-XXVI",
          FEN("W:W16,21,22,23,24,25,26,27,28,29,30,31:B1,2,3,4,5,6,7,9,10,11,12,15"),
          "",
          "g1-g5 g7-e5".some
        ),
        StartingPosition(
          "20-XXVII",
          FEN("W:W19,20,21,22,25,26,27,28,29,30,31,32:B1,2,3,4,5,7,8,9,10,11,12,14"),
          "1. gh4 dc5 2. ef4 cd6",
          "gh4 dc5 ef4 cd6".some
        ),
        StartingPosition(
          "20-XXVIII",
          FEN("W:W15,21,22,23,25,26,27,28,29,30,31,32:B1,2,3,5,6,7,8,9,10,11,12,13"),
          "",
          "g3-e5 h8-a5".some
        ),
        StartingPosition(
          "20-XXIX",
          FEN("W:W18,21,22,25,26,27,28,29,30,31,32:B1,2,4,5,6,7,8,9,10,12,15"),
          "1. gh4 fg5 2. hf6 ge5 3. ed4 fg7",
          "gh4 fg5 hf6 ge5 ed4 fg7".some
        ),
        StartingPosition(
          "20-XXX",
          FEN("W:W13,21,22,23,24,25,26,27,29,30,31,32:B1,2,3,4,6,7,8,9,10,11,12,28"),
          "",
          "h2-a5 a7-h2".some
        ),
        StartingPosition(
          "20-XXXI",
          FEN("W:W14,21,22,23,24,25,26,27,29,30,31,32:B1,2,3,4,6,7,8,9,10,11,12,19"),
          "",
          "h2-c5 a7-f4".some
        ),
        StartingPosition(
          "20-XXXII",
          FEN("W:W16,21,22,23,24,25,26,27,29,30,31,32:B1,2,3,4,5,7,8,9,10,11,12,13"),
          "",
          "h2-g5 c7-a5".some
        )
      )
    ),
    Category(
      "21",
      List(
        StartingPosition(
          "21-I",
          FEN("W:W17,18,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,13,16"),
          "1. ab4 ba5 2. ed4 hg5",
          "ab4 ba5 ed4 hg5".some
        ),
        StartingPosition(
          "21-II",
          FEN("W:W18,21,22,23,24,25,26,27,28,30,31,32:B1,2,3,5,6,7,8,9,10,11,12,15"),
          "",
          "a1-d4 h8-e5".some
        ),
        StartingPosition(
          "21-III",
          FEN("W:W13,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,15"),
          "1. ab4 fe5 2. ba5 gf6",
          "ab4 fe5 ba5 gf6".some
        ),
        StartingPosition(
          "21-IV",
          FEN("W:W17,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,15"),
          "",
          "a3-b4 h6-e5".some
        ),
        StartingPosition(
          "21-V",
          FEN("B:W17,18,21,22,24,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,12,14,15"),
          "1. cb4 bc5 2. bc3 fe5 3. ed4",
          "cb4 bc5 bc3 fe5 ed4".some
        ),
        StartingPosition(
          "21-VI",
          FEN("W:W16,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,21"),
          "",
          "a3-g5 b6-a3".some
        ),
        StartingPosition(
          "21-VII",
          FEN("W:W17,21,22,23,24,25,26,27,28,29,31,32:B1,2,3,4,5,6,7,9,10,12,15,16"),
          "1. cb4 fe5 2. bc3 gf6 3. cb2 fg5",
          "cb4 fe5 bc3 gf6 cb2 fg5".some
        ),
        StartingPosition(
          "21-VIII",
          FEN("W:W20,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,13"),
          "",
          "a3-h4 h6-a5".some
        ),
        StartingPosition(
          "21-IX",
          FEN("W:W18,21,22,23,24,26,27,28,29,30,31,32:B1,2,3,4,6,7,8,9,10,11,12,13"),
          "1. cd4 ba5 2. bc3 ab6",
          "cd4 ba5 bc3 ab6".some
        ),
        StartingPosition(
          "21-X",
          FEN("W:W15,21,22,23,24,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,13"),
          "",
          "b2-e5 g7-a5".some
        ),
        StartingPosition(
          "21-XI",
          FEN("W:W21,22,23,24,26,27,28,29,30,31,32:B1,2,3,4,6,7,8,11,12,14,15"),
          "1. cd4 bc5 2. db6 ac5 3. bc3 de5",
          "cd4 bc5 db6 ac5 bc3 de5".some
        ),
        StartingPosition(
          "21-XII",
          FEN("W:W13,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,5,6,7,8,9,10,11,12,16"),
          "",
          "c3-a5 h8-g5".some
        ),
        StartingPosition(
          "21-XIII",
          FEN("B:W21,22,23,24,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,9,10,12,15"),
          "1. cd4 fe5 2. df6 ge5 3. bc3",
          "cd4 fe5 df6 ge5 bc3".some
        ),
        StartingPosition(
          "21-XIV",
          FEN("W:W19,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,15"),
          "",
          "c3-f4 f6-e5".some
        ),
        StartingPosition(
          "21-XV",
          FEN("W:W18,21,22,23,24,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,20"),
          "1. cd4 hg5 2. bc3 gh4",
          "cd4 hg5 bc3 gh4".some
        ),
        StartingPosition(
          "21-XVI",
          FEN("W:W13,21,22,23,24,25,27,28,29,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,26"),
          "",
          "d2-a5 g7-d2".some
        ),
        StartingPosition(
          "21-XVII",
          FEN("W:W17,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,7,8,9,10,11,12,13"),
          "1. ed4 bc5 2. db6 ca5 3. ab4 ab6",
          "ed4 bc5 db6 ca5 ab4 ab6".some
        ),
        StartingPosition(
          "21-XVIII",
          FEN("W:W18,21,22,23,24,25,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,16"),
          "",
          "d2-d4 h6-g5".some
        ),
        StartingPosition(
          "21-XIX",
          FEN("W:W18,21,22,23,24,25,26,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,12,14,16"),
          "1. ed4 fg5 2. fe3 bc5",
          "ed4 fg5 fe3 bc5".some
        ),
        StartingPosition(
          "21-XX",
          FEN("W:W13,21,22,24,25,26,27,28,29,30,31,32:B1,2,3,5,6,7,8,9,10,11,12,15"),
          "",
          "e3-a5 h8-e5".some
        ),
        StartingPosition(
          "21-XXI",
          FEN("W:W19,20,21,22,24,25,26,27,29,30,31,32:B1,2,3,4,6,7,8,9,10,12,14,15"),
          "1. ef4 bc5 2. gh4 ab6 3. hg3 fe5",
          "ef4 bc5 gh4 ab6 hg3 fe5".some
        ),
        StartingPosition(
          "21-XXII",
          FEN("W:W19,21,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,13"),
          "",
          "e3-f4 g7-a5".some
        ),
        StartingPosition(
          "21-XXIII",
          FEN("W:W19,20,21,22,23,25,26,27,29,30,31,32:B1,2,3,4,6,7,8,9,11,12,13,15"),
          "1. gf4 ba5 2. hg3 ab6 3. gh4 de5",
          "gf4 ba5 hg3 ab6 gh4 de5".some
        ),
        StartingPosition(
          "21-XXIV",
          FEN("W:W18,21,22,23,24,25,26,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,14"),
          "",
          "f2-d4 b6-c5".some
        ),
        StartingPosition(
          "21-XXV",
          FEN("B:W17,19,21,23,24,25,26,27,29,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,15"),
          "1. gf4 fe5 2. hg3 gf6 3. cb4",
          "gf4 fe5 hg3 gf6 cb4".some
        ),
        StartingPosition(
          "21-XXVI",
          FEN("W:W16,21,22,23,24,25,26,27,28,29,30,31:B1,2,3,5,6,7,8,9,10,11,12,13"),
          "",
          "g1-g5 h8-a5".some
        ),
        StartingPosition(
          "21-XXVII",
          FEN("B:W18,20,21,23,24,25,26,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,12,14,15"),
          "1. gh4 dc5 2. fg3 fe5 3. cd4",
          "gh4 dc5 fg3 fe5 cd4".some
        ),
        StartingPosition(
          "21-XXVIII",
          FEN("W:W19,21,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,12"),
          "",
          "g3-f4 ---".some
        ),
        StartingPosition(
          "21-XXIX",
          FEN("B:W18,21,22,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,8,9,10,12,16"),
          "1. gh4 fg5 2. hf6 eg5 3. ed4",
          "gh4 fg5 hf6 eg5 ed4".some
        ),
        StartingPosition(
          "21-XXX",
          FEN("W:W13,21,22,23,24,25,26,27,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,28"),
          "",
          "h2-a5 b6-h2".some
        ),
        StartingPosition(
          "21-XXXI",
          FEN("W:W14,21,22,23,24,25,26,27,29,30,31,32:B1,2,3,4,6,7,8,9,10,11,12,28"),
          "",
          "h2-c5 a7-h2".some
        ),
        StartingPosition(
          "21-XXXII",
          FEN("W:W16,21,22,23,24,25,26,27,29,30,31,32:B1,2,3,4,5,7,8,9,10,11,12,15"),
          "",
          "h2-g5 c7-e5".some
        )
      )
    ),
    Category(
      "22",
      List(
        StartingPosition(
          "22-I",
          FEN("B:W17,20,22,23,24,25,26,27,29,30,31,32:B1,2,3,4,5,6,7,8,10,12,13,15"),
          "1. ab4 ba5 2. gh4 fe5 3. hg3",
          "ab4 ba5 gh4 fe5 hg3".some
        ),
        StartingPosition(
          "22-II",
          FEN("W:W18,21,22,23,24,25,26,27,28,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,16"),
          "",
          "a1-d4 h6-g5".some
        ),
        StartingPosition(
          "22-III",
          FEN("B:W17,19,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,15"),
          "1. ab4 fe5 2. ef4",
          "ab4 fe5 ef4".some
        ),
        StartingPosition(
          "22-IV",
          FEN("W:W17,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,5,6,7,8,9,10,11,12,21"),
          "",
          "a3-b4 h8-a3".some
        ),
        StartingPosition(
          "22-V",
          FEN("W:W17,18,21,23,24,26,27,28,29,30,31,32:B1,2,3,4,6,7,8,9,10,12,14,16"),
          "1. cb4 bc5 2. bc3 fg5 3. cd4 ab6",
          "cb4 bc5 bc3 fg5 cd4 ab6".some
        ),
        StartingPosition(
          "22-VI",
          FEN("W:W16,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,7,8,9,10,11,12,21"),
          "",
          "a3-g5 c7-a3".some
        ),
        StartingPosition(
          "22-VII",
          FEN("B:W17,18,21,22,24,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,15"),
          "1. cb4 fe5 2. bc3 gf6 3. ed4",
          "cb4 fe5 bc3 gf6 ed4".some
        ),
        StartingPosition(
          "22-VIII",
          FEN("W:W12,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,14"),
          "",
          "a3-h6 h6-c5".some
        ),
        StartingPosition(
          "22-IX",
          FEN("W:W18,21,22,23,24,25,26,27,28,30,31,32:B1,2,3,4,6,7,8,9,11,12,13,14"),
          "1. cd4 ba5 2. bc3 ab6 3. ab2 dc5",
          "cd4 ba5 bc3 ab6 ab2 dc5".some
        ),
        StartingPosition(
          "22-X",
          FEN("W:W15,21,22,23,24,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,20"),
          "",
          "b2-e5 g7-h4".some
        ),
        StartingPosition(
          "22-XI",
          FEN("W:W21,22,23,24,26,27,28,29,30,31,32:B1,2,3,4,6,7,8,10,12,14,15"),
          "1. cd4 bc5 2. db6 ac5 3. bc3 fe5",
          "cd4 bc5 db6 ac5 bc3 fe5".some
        ),
        StartingPosition(
          "22-XII",
          FEN("W:W13,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,15"),
          "",
          "c3-a5 h6-e5".some
        ),
        StartingPosition(
          "22-XIII",
          FEN("B:W20,21,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,9,10,12,15"),
          "1. cd4 fe5 2. df6 ge5 3. gh4",
          "cd4 fe5 df6 ge5 gh4".some
        ),
        StartingPosition(
          "22-XIV",
          FEN("W:W19,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,5,6,7,8,9,10,11,12,20"),
          "",
          "c3-f4 h8-h4".some
        ),
        StartingPosition(
          "22-XV",
          FEN("W:W17,18,21,23,24,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,15,20"),
          "1. cd4 hg5 2. bc3 gh4 3. cb4 de5",
          "cd4 hg5 bc3 gh4 cb4 de5".some
        ),
        StartingPosition(
          "22-XVI",
          FEN("W:W13,21,22,23,24,25,27,28,29,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,15"),
          "",
          "d2-a5 g7-e5".some
        ),
        StartingPosition(
          "22-XVII",
          FEN("W:W17,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,7,8,10,11,13,16"),
          "1. ed4 bc5 2. db6 ca5 3. ab4 hg5",
          "ed4 bc5 db6 ca5 ab4 hg5".some
        ),
        StartingPosition(
          "22-XVIII",
          FEN("W:W15,21,22,23,24,25,27,28,29,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,20"),
          "",
          "d2-e5 g7-h4".some
        ),
        StartingPosition(
          "22-XIX",
          FEN("W:W18,21,22,23,24,25,26,28,29,30,31,32:B1,2,3,4,5,6,8,9,10,11,12,16"),
          "1. ed4 fg5 2. fe3 ef6",
          "ed4 fg5 fe3 ef6".some
        ),
        StartingPosition(
          "22-XX",
          FEN("W:W13,21,22,24,25,26,27,28,29,30,31,32:B1,2,3,5,6,7,8,9,10,11,12,20"),
          "",
          "e3-a5 h8-h4".some
        ),
        StartingPosition(
          "22-XXI",
          FEN("W:W17,19,21,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,8,9,10,11,12,14"),
          "1. ef4 dc5 2. cb4 ed6",
          "ef4 dc5 cb4 ed6".some
        ),
        StartingPosition(
          "22-XXII",
          FEN("W:W11,21,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,17"),
          "",
          "e3-f6 f6-b4".some
        ),
        StartingPosition(
          "22-XXIII",
          FEN("W:W19,21,22,23,24,25,26,27,29,30,31,32:B1,2,3,4,5,6,7,8,10,12,13,16"),
          "1. gf4 ba5 2. hg3 fg5",
          "gf4 ba5 hg3 fg5".some
        ),
        StartingPosition(
          "22-XXIV",
          FEN("W:W18,21,22,23,24,25,26,28,29,30,31,32:B1,2,3,4,5,7,8,9,10,11,12,16"),
          "",
          "f2-d4 c7-g5".some
        ),
        StartingPosition(
          "22-XXV",
          FEN("B:W18,19,21,22,24,25,26,27,29,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,15"),
          "1. gf4 fe5 2. hg3 gf6 3. ed4",
          "gf4 fe5 hg3 gf6 ed4".some
        ),
        StartingPosition(
          "22-XXVI",
          FEN("W:W20,21,22,23,24,25,26,27,28,29,30,31:B1,2,3,4,5,6,7,8,9,11,12,14"),
          "",
          "g1-h4 d6-c5".some
        ),
        StartingPosition(
          "22-XXVII",
          FEN("B:W18,20,21,22,24,25,26,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,12,14,15"),
          "1. gh4 dc5 2. fg3 fe5 3. ed4",
          "gh4 dc5 fg3 fe5 ed4".some
        ),
        StartingPosition(
          "22-XXVIII",
          FEN("W:W19,21,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,16"),
          "",
          "g3-f4 d6-g5".some
        ),
        StartingPosition(
          "22-XXIX",
          FEN("W:W18,20,21,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,14,16"),
          "1. gh4 hg5 2. cd4 dc5",
          "gh4 hg5 cd4 dc5".some
        ),
        StartingPosition(
          "22-XXX",
          FEN("W:W13,21,22,23,24,25,26,27,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,28"),
          "",
          "h2-a5 d6-h2".some
        ),
        StartingPosition(
          "22-XXXI",
          FEN("W:W14,21,22,23,24,25,26,27,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,28"),
          "",
          "h2-c5 b6-h2".some
        ),
        StartingPosition(
          "22-XXXII",
          FEN("W:W16,21,22,23,24,25,26,27,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,28"),
          "",
          "h2-g5 d6-h2".some
        )
      )
    ),
    Category(
      "23",
      List(
        StartingPosition(
          "23-I",
          FEN("W:W17,20,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,12,13,16"),
          "1. ab4 ba5 2. gh4 fg5",
          "ab4 ba5 gh4 fg5".some
        ),
        StartingPosition(
          "23-II",
          FEN("W:W14,21,22,23,24,25,26,27,28,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,16"),
          "",
          "a1-c5 g7-g5".some
        ),
        StartingPosition(
          "23-III",
          FEN("W:W17,20,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,15"),
          "1. ab4 fe5 2. gh4 gf6",
          "ab4 fe5 gh4 gf6".some
        ),
        StartingPosition(
          "23-IV",
          FEN("W:W14,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,21"),
          "",
          "a3-c5 b6-a3".some
        ),
        StartingPosition(
          "23-V",
          FEN("B:W17,18,21,22,24,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,12,14,16"),
          "1. cb4 bc5 2. bc3 fg5 3. ed4",
          "cb4 bc5 bc3 fg5 ed4".some
        ),
        StartingPosition(
          "23-VI",
          FEN("W:W16,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,21"),
          "",
          "a3-g5 d6-a3".some
        ),
        StartingPosition(
          "23-VII",
          FEN("W:W17,20,21,22,23,26,27,28,29,30,31,32:B1,2,3,5,6,7,8,9,10,11,12,15"),
          "1. cb4 fe5 2. bc3 gf6 3. gh4 hg7",
          "cb4 fe5 bc3 gf6 gh4 hg7".some
        ),
        StartingPosition(
          "23-VIII",
          FEN("W:W21,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,6,7,8,9,10,11,12,13"),
          "",
          "--- a7-a5".some
        ),
        StartingPosition(
          "23-IX",
          FEN("B:W18,19,21,22,24,26,27,28,29,30,31,32:B1,2,3,4,6,7,8,9,10,11,12,13"),
          "1. cd4 ba5 2. bc3 ab6 3. ef4",
          "cd4 ba5 bc3 ab6 ef4".some
        ),
        StartingPosition(
          "23-X",
          FEN("W:W19,21,22,23,24,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,14"),
          "",
          "b2-f4 d6-c5".some
        ),
        StartingPosition(
          "23-XI",
          FEN("W:W21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,7,8,10,11,12,13"),
          "1. cd4 bc5 2. db6 ca5",
          "cd4 bc5 db6 ca5".some
        ),
        StartingPosition(
          "23-XII",
          FEN("W:W17,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,6,7,8,9,10,11,12,13"),
          "",
          "c3-b4 a7-a5".some
        ),
        StartingPosition(
          "23-XIII",
          FEN("W:W19,21,23,25,26,27,28,29,30,31,32:B1,3,4,5,6,7,8,9,10,12,16"),
          "1. cd4 fe5 2. df6 eg5 3. gf4 de7",
          "cd4 fe5 df6 eg5 gf4 de7".some
        ),
        StartingPosition(
          "23-XIV",
          FEN("W:W19,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,14"),
          "",
          "c3-f4 h6-c5".some
        ),
        StartingPosition(
          "23-XV",
          FEN("B:W15,18,21,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,20"),
          "1. cd4 hg5 2. gf4 gh4 3. fe5",
          "cd4 hg5 gf4 gh4 fe5".some
        ),
        StartingPosition(
          "23-XVI",
          FEN("W:W17,21,22,23,24,25,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,12"),
          "",
          "d2-b4 ---".some
        ),
        StartingPosition(
          "23-XVII",
          FEN("W:W17,18,21,24,25,26,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,12,16"),
          "1. ed4 dc5 2. cb4 ce3 3. fd4 fg5",
          "ed4 dc5 cb4 ce3 fd4 fg5".some
        ),
        StartingPosition(
          "23-XVIII",
          FEN("W:W19,21,22,23,24,25,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,12"),
          "",
          "d2-f4 ---".some
        ),
        StartingPosition(
          "23-XIX",
          FEN("W:W18,21,22,23,24,25,26,27,28,29,30,31:B1,2,3,4,5,6,8,10,11,12,14,16"),
          "1. ed4 fg5 2. fe3 ef6 3. gf2 bc5",
          "ed4 fg5 fe3 ef6 gf2 bc5".some
        ),
        StartingPosition(
          "23-XX",
          FEN("W:W17,21,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,12"),
          "",
          "e3-b4 ---".some
        ),
        StartingPosition(
          "23-XXI",
          FEN("W:W17,19,20,21,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,8,9,11,12,14,15"),
          "1. ef4 dc5 2. cb4 ed6 3. gh4 de5",
          "ef4 dc5 cb4 ed6 gh4 de5".some
        ),
        StartingPosition(
          "23-XXII",
          FEN("W:W16,21,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,15"),
          "",
          "e3-g5 h6-e5".some
        ),
        StartingPosition(
          "23-XXIII",
          FEN("W:W19,20,21,22,23,25,26,27,29,30,31,32:B1,2,3,4,6,7,8,10,11,12,13,14"),
          "1. gf4 bc5 2. hg3 ab6 3. gh4 ba5",
          "gf4 bc5 hg3 ab6 gh4 ba5".some
        ),
        StartingPosition(
          "23-XXIV",
          FEN("W:W18,21,22,23,24,25,26,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,13"),
          "",
          "f2-d4 d6-a5".some
        ),
        StartingPosition(
          "23-XXV",
          FEN("W:W17,19,21,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,12,15,16"),
          "1. gf4 fg5 2. cb4 de5",
          "gf4 fg5  cb4 de5".some
        ),
        StartingPosition(
          "23-XXVI",
          FEN("W:W20,21,22,23,24,25,26,27,28,29,30,31:B1,2,3,4,5,6,8,9,10,11,12,16"),
          "",
          "g1-h4 e7-g5".some
        ),
        StartingPosition(
          "23-XXVII",
          FEN("W:W20,21,22,23,24,25,26,27,29,30,31,32:B1,2,3,4,5,6,7,8,9,12,14,15"),
          "1. gh4 dc5 2. hg3 fe5",
          "gh4 dc5 hg3 fe5".some
        ),
        StartingPosition(
          "23-XXVIII",
          FEN("W:W19,21,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,13"),
          "",
          "g3-f4 g7-a5".some
        ),
        StartingPosition(
          "23-XXIX",
          FEN("B:W18,20,21,23,24,25,26,28,29,30,31,32:B1,2,3,4,5,6,7,9,10,11,12,16"),
          "1. gh4 hg5 2. cd4 gh6 3. fg3",
          "gh4 hg5  cd4 gh6 fg3".some
        ),
        StartingPosition(
          "23-XXX",
          FEN("W:W13,21,22,23,24,25,26,27,29,30,31,32:B1,2,3,4,5,6,8,9,10,11,12,16"),
          "",
          "h2-a5 e7-g5".some
        ),
        StartingPosition(
          "23-XXXI",
          FEN("W:W14,21,22,23,24,25,26,27,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,28"),
          "",
          "h2-c5 d6-h2".some
        ),
        StartingPosition(
          "23-XXXII",
          FEN("W:W20,21,22,23,24,25,26,27,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,12"),
          "",
          "h2-h4 ---".some
        )
      )
    )
  )

  val categoriesIDFBrazilian = categoriesIDF.map {
    case cat if cat.name == "3"  =>
      cat.copy(positions = cat.positions.filterNot(_.code == "3-XIII"))
    case cat if cat.name == "5"  =>
      cat.copy(positions = cat.positions.filterNot(_.code == "5-XXVI"))
    case cat if cat.name == "12" =>
      cat.copy(positions = cat.positions.filterNot(_.code == "12-XV"))
    case cat if cat.name == "16" =>
      cat.copy(positions = cat.positions.filterNot(_.code == "16-XV"))
    case cat if cat.name == "17" =>
      cat.copy(positions = cat.positions.filterNot(_.code == "17-I"))
    case cat if cat.name == "18" =>
      cat.copy(positions = cat.positions.filterNot(p => p.code == "18-XI" || p.code == "18-XV"))
    case cat if cat.name == "20" =>
      cat.copy(positions = cat.positions.filterNot(_.code == "20-XXIII"))
    case cat if cat.name == "22" =>
      cat.copy(positions = cat.positions.filterNot(_.code == "22-XIV"))
    case cat                     => cat
  }

  val categoriesIDFBasic = List(
    Category(
      "1",
      List(
        StartingPosition(
          "1-I",
          FEN("B:W14,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,13"),
          "1. cd4 ba5 2. dc5",
          "cd4 ba5 dc5".some
        ),
        StartingPosition(
          "1-II",
          FEN("B:W19,21,22,23,24,25,26,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,14"),
          "1. ef4 dc5 2. fe3",
          "ef4 dc5 fe3".some
        ),
        StartingPosition(
          "1-III",
          FEN("W:W20,21,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,16"),
          "1. gh4 hg5",
          "gh4 hg5".some
        ),
        StartingPosition(
          "1-IV",
          FEN("W:W19,21,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,15"),
          "1. gf4 de5",
          "gf4 de5".some
        ),
        StartingPosition(
          "1-V",
          FEN("B:W18,21,22,23,24,25,26,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,16"),
          "1. ed4 fg5 2. fe3",
          "ed4 fg5 fe3".some
        ),
        StartingPosition(
          "1-VI",
          FEN("W:W17,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,15"),
          "1. cb4 de5",
          "cb4 de5".some
        ),
        StartingPosition(
          "1-VII",
          FEN("W:W20,21,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,16"),
          "1. gh4 fg5",
          "gh4 fg5".some
        ),
        StartingPosition(
          "1-VIII",
          FEN("B:W18,19,21,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,14"),
          "1. gf4 bc5 2. cd4",
          "gf4 bc5 cd4".some
        ),
        StartingPosition(
          "1-IX",
          FEN("B:W13,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,16"),
          "1. ab4 hg5 2. ba5",
          "ab4 hg5 ba5".some
        ),
        StartingPosition(
          "1-X",
          FEN("B:W17,20,21,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,16"),
          "1. gh4 hg5 2. cb4",
          "gh4 hg5 cb4".some
        )
      )
    ),
    Category(
      "2",
      List(
        StartingPosition(
          "2-I",
          FEN("B:W20,21,22,23,24,25,26,27,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,13"),
          "1. gh4 ba5 2. hg3",
          "gh4 ba5 hg3".some
        ),
        StartingPosition(
          "2-II",
          FEN("B:W18,20,21,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,16"),
          "1. cd4 hg5 2. gh4",
          "cd4 hg5 gh4".some
        ),
        StartingPosition(
          "2-III",
          FEN("B:W17,19,21,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,16"),
          "1. ef4 fg5 2. cb4",
          "ef4 fg5 cb4".some
        ),
        StartingPosition(
          "2-IV",
          FEN("B:W13,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,15"),
          "1. ef4 fe5 2. ab4",
          "ef4 fe5 ab4".some
        ),
        StartingPosition(
          "2-V",
          FEN("B:W13,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,15"),
          "1. ab4 fe5 2. ba5",
          "ab4 fe5 ba5".some
        ),
        StartingPosition(
          "2-VI",
          FEN("W:W19,21,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,14"),
          "1. gf4 bc5",
          "gf4 bc5".some
        ),
        StartingPosition(
          "2-VII",
          FEN("B:W17,19,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,16"),
          "1. ab4 hg5 2. gf4",
          "ab4 hg5 gf4".some
        ),
        StartingPosition(
          "2-VIII",
          FEN("W:W18,21,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,15"),
          "1. ed4 fe5",
          "ed4 fe5".some
        ),
        StartingPosition(
          "2-IX",
          FEN("B:W17,20,21,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,14"),
          "1. gh4 dc5 2. cb4",
          "gh4 dc5 cb4".some
        ),
        StartingPosition(
          "2-X",
          FEN("B:W18,19,21,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,13"),
          "1. gf4 ba5 2. cd4",
          "gf4 ba5 cd4".some
        )
      )
    ),
    Category(
      "3",
      List(
        StartingPosition(
          "3-I",
          FEN("B:W14,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,16"),
          "1. cd4 fg5 2. dc5",
          "cd4 fg5 dc5".some
        ),
        StartingPosition(
          "3-II",
          FEN("B:W20,21,22,23,24,25,26,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,14"),
          "1. gh4 dc5 2. fg3",
          "gh4 dc5 fg3".some
        ),
        StartingPosition(
          "3-III",
          FEN("B:W19,21,22,23,24,25,26,27,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,15"),
          "1. gf4 fe5 2. hg3",
          "gf4 fe5 hg3".some
        ),
        StartingPosition(
          "3-IV",
          FEN("W:W17,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,16"),
          "1. cb4 hg5",
          "cb4 hg5".some
        ),
        StartingPosition(
          "3-V",
          FEN("W:W17,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,15"),
          "1. ab4 de5",
          "ab4 de5".some
        ),
        StartingPosition(
          "3-VI",
          FEN("B:W17,20,21,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,15"),
          "1. gh4 de5 2. cb4",
          "gh4 de5 cb4".some
        ),
        StartingPosition(
          "3-VII",
          FEN("W:W19,21,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,13"),
          "1. ef4 ba5",
          "ef4 ba5".some
        ),
        StartingPosition(
          "3-VIII",
          FEN("W:W17,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,16"),
          "1. cb4 fg5",
          "cb4 fg5".some
        ),
        StartingPosition(
          "3-IX",
          FEN("B:W20,21,22,23,24,25,26,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,14"),
          "1. gh4 bc5 2. fg3",
          "gh4 bc5 fg3".some
        ),
        StartingPosition(
          "3-X",
          FEN("B:W19,21,22,23,24,25,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,14"),
          "1. ef4 dc5 2. de3",
          "ef4 dc5 de3".some
        )
      )
    ),
    Category(
      "4",
      List(
        StartingPosition(
          "4-I",
          FEN("B:W17,21,22,23,24,25,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,15"),
          "1. cb4 fe5 2. dc3",
          "cb4 fe5 dc3".some
        ),
        StartingPosition(
          "4-II",
          FEN("B:W18,20,21,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,14"),
          "1. gh4 bc5 2. cd4",
          "gh4 bc5 cd4".some
        ),
        StartingPosition(
          "4-III",
          FEN("B:W14,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,16"),
          "1. ab4 fg5 2. bc5",
          "ab4 fg5 bc5".some
        ),
        StartingPosition(
          "4-IV",
          FEN("B:W18,20,21,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,14"),
          "1. cd4 dc5 2. gh4",
          "cd4 dc5 gh4".some
        ),
        StartingPosition(
          "4-V",
          FEN("B:W17,20,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,15"),
          "1. ab4 fe5 2. gh4",
          "ab4 fe5 gh4".some
        ),
        StartingPosition(
          "4-VI",
          FEN("W:W20,21,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,13"),
          "1. gh4 ba5",
          "gh4 ba5".some
        ),
        StartingPosition(
          "4-VII",
          FEN("B:W14,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,15"),
          "1. ab4 fe5 2. bc5",
          "ab4 fe5 bc5".some
        ),
        StartingPosition(
          "4-VIII",
          FEN("B:W19,21,22,23,24,25,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,15"),
          "1. ef4 fe5 2. de3",
          "ef4 fe5 de3".some
        ),
        StartingPosition(
          "4-IX",
          FEN("B:W17,20,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,13"),
          "1. ab4 ba5 2. gh4",
          "ab4 ba5 gh4".some
        ),
        StartingPosition(
          "4-X",
          FEN("B:W20,21,22,23,24,25,26,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,15"),
          "1. gh4 de5 2. fg3",
          "gh4 de5 fg3".some
        )
      )
    ),
    Category(
      "5",
      List(
        StartingPosition(
          "5-I",
          FEN("W:W19,21,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,16"),
          "1. gf4 fg5",
          "gf4 fg5".some
        ),
        StartingPosition(
          "5-II",
          FEN("W:W20,21,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,14"),
          "1. gh4 bc5",
          "gh4 bc5".some
        ),
        StartingPosition(
          "5-III",
          FEN("B:W17,21,22,23,24,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,13"),
          "1. ab4 ba5 2. ba3",
          "ab4 ba5 ba3".some
        ),
        StartingPosition(
          "5-IV",
          FEN("B:W13,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,15"),
          "1. ab4 de5 2. ba5",
          "ab4 de5 ba5".some
        ),
        StartingPosition(
          "5-V",
          FEN("W:W18,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,16"),
          "1. cd4 hg5",
          "cd4 hg5".some
        ),
        StartingPosition(
          "5-VI",
          FEN("W:W18,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,16"),
          "1. cd4 fg5",
          "cd4 fg5".some
        ),
        StartingPosition(
          "5-VII",
          FEN("W:W21,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,12"),
          "1. Free",
          "Free".some
        ),
        StartingPosition(
          "5-VIII",
          FEN("B:W20,21,22,23,24,25,26,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,16"),
          "1. gh4 hg5 2. fg3",
          "gh4 hg5 fg3".some
        ),
        StartingPosition(
          "5-IX",
          FEN("B:W17,21,22,23,24,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,16"),
          "1. ab4 fg5 2. ba3",
          "ab4 fg5 ba3".some
        ),
        StartingPosition(
          "5-X",
          FEN("B:W20,21,22,23,24,25,26,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,13"),
          "1. gh4 ba5 2. fg3",
          "gh4 ba5 fg3".some
        )
      )
    ),
    Category(
      "6",
      List(
        StartingPosition(
          "6-I",
          FEN("W:W17,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,14"),
          "1. cb4 bc5",
          "cb4 bc5".some
        ),
        StartingPosition(
          "6-II",
          FEN("B:W19,21,22,23,24,25,26,27,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,13"),
          "1. gf4 ba5 2. hg3",
          "gf4 ba5 hg3".some
        ),
        StartingPosition(
          "6-III",
          FEN("B:W17,21,22,23,24,25,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,13"),
          "1. cb4 ba5 2. dc3",
          "cb4 ba5 dc3".some
        ),
        StartingPosition(
          "6-IV",
          FEN("W:W18,21,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,16"),
          "1. ed4 fg5",
          "ed4 fg5".some
        ),
        StartingPosition(
          "6-V",
          FEN("B:W17,19,21,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,14"),
          "1. ef4 dc5 2. cb4",
          "ef4 dc5 cb4".some
        ),
        StartingPosition(
          "6-VI",
          FEN("B:W18,21,22,23,24,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,15"),
          "1. cd4 de5 2. bc3",
          "cd4 de5 bc3".some
        ),
        StartingPosition(
          "6-VII",
          FEN("W:W19,21,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,16"),
          "1. ef4 fg5",
          "ef4 fg5".some
        ),
        StartingPosition(
          "6-VIII",
          FEN("B:W17,20,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,15"),
          "1. ab4 de5 2. gh4",
          "ab4 de5 gh4".some
        ),
        StartingPosition(
          "6-IX",
          FEN("B:W17,18,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,15"),
          "1. ed4 de5 2. ab4",
          "ed4 de5 ab4".some
        ),
        StartingPosition(
          "6-X",
          FEN("B:W17,21,22,23,24,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,15"),
          "1. cb4 fe5 2. bc3",
          "cb4 fe5 bc3".some
        )
      )
    ),
    Category(
      "7",
      List(
        StartingPosition(
          "7-I",
          FEN("B:W18,21,22,23,24,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,16"),
          "1. cd4 fg5 2. bc3",
          "cd4 fg5 bc3".some
        ),
        StartingPosition(
          "7-II",
          FEN("W:W18,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,15"),
          "1. cd4 fe5",
          "cd4 fe5".some
        ),
        StartingPosition(
          "7-III",
          FEN("B:W18,21,22,23,24,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,16"),
          "1. cd4 hg5 2. bc3",
          "cd4 hg5 bc3".some
        ),
        StartingPosition(
          "7-IV",
          FEN("W:W19,21,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,14"),
          "1. ef4 bc5",
          "ef4 bc5".some
        ),
        StartingPosition(
          "7-V",
          FEN("B:W20,21,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,12"),
          "1. gh4",
          "gh4".some
        ),
        StartingPosition(
          "7-VI",
          FEN("W:W17,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,14"),
          "1. cb4 dc5",
          "cb4 dc5".some
        ),
        StartingPosition(
          "7-VII",
          FEN("B:W14,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,15"),
          "1. cb4 fe5 2. bc5",
          "cb4 fe5 bc5".some
        ),
        StartingPosition(
          "7-VIII",
          FEN("B:W18,21,22,23,24,25,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,16"),
          "1. ed4 fg5 2. de3",
          "ed4 fg5 de3".some
        ),
        StartingPosition(
          "7-IX",
          FEN("W:W18,21,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,16"),
          "1. ed4 hg5",
          "ed4 hg5".some
        ),
        StartingPosition(
          "7-X",
          FEN("B:W18,21,22,23,24,25,26,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,14"),
          "1. ed4 dc5 2. fe3",
          "ed4 dc5 fe3".some
        )
      )
    ),
    Category(
      "8",
      List(
        StartingPosition(
          "8-I",
          FEN("W:W17,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,16"),
          "1. ab4 fg5",
          "ab4 fg5".some
        ),
        StartingPosition(
          "8-II",
          FEN("W:W18,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,15"),
          "1. cd4 de5",
          "cd4 de5".some
        ),
        StartingPosition(
          "8-III",
          FEN("B:W17,21,22,23,24,25,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,16"),
          "1. cb4 fg5 2. dc3",
          "cb4 fg5 dc3".some
        ),
        StartingPosition(
          "8-IV",
          FEN("B:W19,21,22,23,24,25,26,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,15"),
          "1. ef4 fe5 2. fe3",
          "ef4 fe5 fe3".some
        ),
        StartingPosition(
          "8-V",
          FEN("B:W18,21,22,23,24,25,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,14"),
          "1. ed4 dc5 2. de3",
          "ed4 dc5 de3".some
        ),
        StartingPosition(
          "8-VI",
          FEN("B:W19,21,22,23,24,25,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,14"),
          "1. ef4 bc5 2. de3",
          "ef4 bc5 de3".some
        ),
        StartingPosition(
          "8-VII",
          FEN("W:W18,21,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,14"),
          "1. ed4 bc5",
          "ed4 bc5".some
        ),
        StartingPosition(
          "8-VIII",
          FEN("W:W18,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,14"),
          "1. cd4 dc5",
          "cd4 dc5".some
        ),
        StartingPosition(
          "8-IX",
          FEN("B:W13,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,14"),
          "1. cb4 bc5 2. ba5",
          "cb4 bc5 ba5".some
        ),
        StartingPosition(
          "8-X",
          FEN("B:W17,19,21,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,16"),
          "1. gf4 fg5 2. cb4",
          "gf4 fg5 cb4".some
        )
      )
    ),
    Category(
      "9",
      List(
        StartingPosition(
          "9-I",
          FEN("W:W17,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,15"),
          "1. ab4 fe5",
          "ab4 fe5".some
        ),
        StartingPosition(
          "9-II",
          FEN("B:W17,19,21,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,15"),
          "1. ef4 fe5 2. cb4",
          "ef4 fe5 cb4".some
        ),
        StartingPosition(
          "9-III",
          FEN("B:W13,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,16"),
          "1. ab4 fg5 2. ba5",
          "ab4 fg5 ba5".some
        ),
        StartingPosition(
          "9-IV",
          FEN("B:W17,21,22,23,24,25,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,14"),
          "1. cb4 bc5 2. dc3",
          "cb4 bc5 dc3".some
        ),
        StartingPosition(
          "9-V",
          FEN("W:W18,21,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,14"),
          "1. ed4 dc5",
          "ed4 dc5".some
        ),
        StartingPosition(
          "9-VI",
          FEN("B:W13,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,14"),
          "1. ab4 bc5 2. ba5",
          "ab4 bc5 ba5".some
        ),
        StartingPosition(
          "9-VII",
          FEN("W:W19,21,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,15"),
          "1. gf4 fe5",
          "gf4 fe5".some
        ),
        StartingPosition(
          "9-VIII",
          FEN("B:W19,21,22,23,24,25,26,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,14"),
          "1. gf4 bc5 2. fg3",
          "gf4 bc5 fg3".some
        ),
        StartingPosition(
          "9-IX",
          FEN("W:W20,21,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,14"),
          "1. gh4 dc5",
          "gh4 dc5".some
        ),
        StartingPosition(
          "9-X",
          FEN("B:W19,21,22,23,24,25,26,27,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,14"),
          "1. gf4 bc5 2. hg3",
          "gf4 bc5 hg3".some
        )
      )
    ),
    Category(
      "10",
      List(
        StartingPosition(
          "10-I",
          FEN("W:W18,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,13"),
          "1. cd4 ba5",
          "cd4 ba5".some
        ),
        StartingPosition(
          "10-II",
          FEN("B:W19,21,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,12"),
          "1. gf4",
          "gf4".some
        ),
        StartingPosition(
          "10-III",
          FEN("W:W17,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,15"),
          "1. cb4 fe5",
          "cb4 fe5".some
        ),
        StartingPosition(
          "10-IV",
          FEN("B:W18,19,21,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,15"),
          "1. cd4 de5 2. gf4",
          "cd4 de5 gf4".some
        ),
        StartingPosition(
          "10-V",
          FEN("B:W18,20,21,22,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,16"),
          "1. ed4 hg5 2. gh4",
          "ed4 hg5 gh4".some
        ),
        StartingPosition(
          "10-VI",
          FEN("W:W19,21,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,13"),
          "1. gf4 ba5",
          "gf4 ba5".some
        ),
        StartingPosition(
          "10-VII",
          FEN("B:W18,19,21,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,16"),
          "1. cd4 hg5 2. gf4",
          "cd4 hg5 gf4".some
        ),
        StartingPosition(
          "10-VIII",
          FEN("B:W17,19,21,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,15"),
          "1. cb4 de5 2. ef4",
          "cb4 de5 ef4".some
        ),
        StartingPosition(
          "10-IX",
          FEN("B:W17,20,21,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,14"),
          "1. gh4 bc5 2. cb4",
          "gh4 bc5 cb4".some
        ),
        StartingPosition(
          "10-X",
          FEN("W:W18,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,14"),
          "1. cd4 bc5",
          "cd4 bc5".some
        )
      )
    ),
    Category(
      "11",
      List(
        StartingPosition(
          "11-I",
          FEN("B:W19,21,22,23,24,25,26,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,13"),
          "1. ef4 ba5 2. fe3",
          "ef4 ba5 fe3".some
        ),
        StartingPosition(
          "11-II",
          FEN("W:W19,21,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,15"),
          "1. ef4 de5",
          "ef4 de5".some
        ),
        StartingPosition(
          "11-III",
          FEN("B:W17,18,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,13"),
          "1. ed4 ba5 2. ab4",
          "ed4 ba5 ab4".some
        ),
        StartingPosition(
          "11-IV",
          FEN("B:W13,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,16"),
          "1. cb4 hg5 2. ba5",
          "cb4 hg5 ba5".some
        ),
        StartingPosition(
          "11-V",
          FEN("B:W17,21,22,23,24,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,16"),
          "1. ab4 hg5 2. ba3",
          "ab4 hg5 ba3".some
        ),
        StartingPosition(
          "11-VI",
          FEN("B:W17,20,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,16"),
          "1. ab4 hg5 2. gh4",
          "ab4 hg5 gh4".some
        ),
        StartingPosition(
          "11-VII",
          FEN("B:W14,21,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,13"),
          "1. ed4 ba5 2. dc5",
          "ed4 ba5 dc5".some
        ),
        StartingPosition(
          "11-VIII",
          FEN("W:W17,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,13"),
          "1. ab4 ba5",
          "ab4 ba5".some
        ),
        StartingPosition(
          "11-IX",
          FEN("B:W18,19,21,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,13"),
          "1. ef4 ba5 2. cd4",
          "ef4 ba5 cd4".some
        ),
        StartingPosition(
          "11-X",
          FEN("B:W19,21,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,12"),
          "1. ef4",
          "ef4".some
        )
      )
    ),
    Category(
      "12",
      List(
        StartingPosition(
          "12-I",
          FEN("B:W18,20,21,22,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,15"),
          "1. gh4 fe5 2. ed4",
          "gh4 fe5 ed4".some
        ),
        StartingPosition(
          "12-II",
          FEN("B:W17,19,21,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,13"),
          "1. gf4 ba5 2. cb4",
          "gf4 ba5 cb4".some
        ),
        StartingPosition(
          "12-III",
          FEN("W:W17,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,16"),
          "1. ab4 hg5",
          "ab4 hg5".some
        ),
        StartingPosition(
          "12-IV",
          FEN("W:W17,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,14"),
          "1. ab4 bc5",
          "ab4 bc5".some
        ),
        StartingPosition(
          "12-V",
          FEN("B:W18,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,12"),
          "1. cd4",
          "cd4".some
        ),
        StartingPosition(
          "12-VI",
          FEN("B:W17,20,21,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,16"),
          "1. cb4 fg5 2. gh4",
          "cb4 fg5 gh4".some
        ),
        StartingPosition(
          "12-VII",
          FEN("B:W20,21,22,23,24,25,26,27,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,14"),
          "1. gh4 dc5 2. hg3",
          "gh4 dc5 hg3".some
        ),
        StartingPosition(
          "12-VIII",
          FEN("B:W18,21,22,23,24,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,13"),
          "1. cd4 ba5 2. bc3",
          "cd4 ba5 bc3".some
        ),
        StartingPosition(
          "12-IX",
          FEN("B:W17,21,22,23,24,25,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,15"),
          "1. cb4 de5 2. dc3",
          "cb4 de5 dc3".some
        ),
        StartingPosition(
          "12-X",
          FEN("B:W14,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,16"),
          "1. ab4 hg5 2. bc5",
          "ab4 hg5 bc5".some
        )
      )
    ),
    Category(
      "13",
      List(
        StartingPosition(
          "13-I",
          FEN("B:W18,21,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,12"),
          "1. ed4",
          "ed4".some
        ),
        StartingPosition(
          "13-II",
          FEN("B:W17,19,21,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,14"),
          "1. ef4 bc5 2. cb4",
          "ef4 bc5 cb4".some
        ),
        StartingPosition(
          "13-III",
          FEN("W:W19,21,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,14"),
          "1. gf4 dc5",
          "gf4 dc5".some
        ),
        StartingPosition(
          "13-IV",
          FEN("B:W18,19,21,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,16"),
          "1. gf4 fg5 2. cd4",
          "gf4 fg5 cd4".some
        ),
        StartingPosition(
          "13-V",
          FEN("W:W19,21,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,15"),
          "1. ef4 fe5",
          "ef4 fe5".some
        ),
        StartingPosition(
          "13-VI",
          FEN("B:W13,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,15"),
          "1. cb4 fe5 2. ba5",
          "cb4 fe5 ba5".some
        ),
        StartingPosition(
          "13-VII",
          FEN("W:W18,21,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,13"),
          "1. ed4 ba5",
          "ed4 ba5".some
        ),
        StartingPosition(
          "13-VIII",
          FEN("W:W17,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,13"),
          "1. cb4 ba5",
          "cb4 ba5".some
        ),
        StartingPosition(
          "13-IX",
          FEN("B:W20,21,22,23,24,25,26,27,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,16"),
          "1. gh4 hg5 2. hg3",
          "gh4 hg5 hg3".some
        ),
        StartingPosition(
          "13-X",
          FEN("B:W20,21,22,23,24,25,26,27,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,15"),
          "1. gh4 de5 2. hg3",
          "gh4 de5 hg3".some
        )
      )
    ),
    Category(
      "14",
      List(
        StartingPosition(
          "14-I",
          FEN("B:W17,19,21,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,14"),
          "1. gf4 dc5 2. cb4",
          "gf4 dc5 cb4".some
        ),
        StartingPosition(
          "14-II",
          FEN("B:W17,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,12"),
          "1. ab4",
          "ab4".some
        ),
        StartingPosition(
          "14-III",
          FEN("B:W18,21,22,23,24,25,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,15"),
          "1. cd4 de5 2. dc3",
          "cd4 de5 dc3".some
        ),
        StartingPosition(
          "14-IV",
          FEN("B:W17,18,21,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,16"),
          "1. ed4 fg5 2. cb4",
          "ed4 fg5 cb4".some
        ),
        StartingPosition(
          "14-V",
          FEN("B:W17,21,22,23,24,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,15"),
          "1. cb4 de5 2. bc3",
          "cb4 de5 bc3".some
        ),
        StartingPosition(
          "14-VI",
          FEN("B:W18,19,21,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,14"),
          "1. gf4 dc5 2. cd4",
          "gf4 dc5 cd4".some
        ),
        StartingPosition(
          "14-VII",
          FEN("W:W18,21,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,15"),
          "1. ed4 de5",
          "ed4 de5".some
        ),
        StartingPosition(
          "14-VIII",
          FEN("B:W20,21,22,23,24,25,26,27,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,14"),
          "1. gh4 bc5 2. hg3",
          "gh4 bc5 hg3".some
        ),
        StartingPosition(
          "14-IX",
          FEN("W:W19,21,22,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,14"),
          "1. ef4 dc5",
          "ef4 dc5".some
        ),
        StartingPosition(
          "14-X",
          FEN("B:W17,19,21,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,14"),
          "1. gf4 bc5 2. cb4",
          "gf4 bc5 cb4".some
        )
      )
    ),
    Category(
      "15",
      List(
        StartingPosition(
          "15-I",
          FEN("W:W17,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,14"),
          "1. ab4 dc5",
          "ab4 dc5".some
        ),
        StartingPosition(
          "15-II",
          FEN("W:W20,21,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,15"),
          "1. gh4 fe5",
          "gh4 fe5".some
        ),
        StartingPosition(
          "15-III",
          FEN("B:W13,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,15"),
          "1. cb4 de5 2. ba5",
          "cb4 de5 ba5".some
        ),
        StartingPosition(
          "15-IV",
          FEN("B:W18,20,21,22,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,16"),
          "1. ed4 fg5 2. gh4",
          "ed4 fg5 gh4".some
        ),
        StartingPosition(
          "15-V",
          FEN("W:W20,21,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,15"),
          "1. gh4 de5",
          "gh4 de5".some
        ),
        StartingPosition(
          "15-VI",
          FEN("B:W18,20,21,22,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,12,15"),
          "1. ed4 de5 2. gh4",
          "ed4 de5 gh4".some
        ),
        StartingPosition(
          "15-VII",
          FEN("B:W17,21,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,12"),
          "1. cb4",
          "cb4".some
        ),
        StartingPosition(
          "15-VIII",
          FEN("B:W17,21,22,23,24,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,14"),
          "1. cb4 bc5 2. bc3",
          "cb4 bc5 bc3".some
        ),
        StartingPosition(
          "15-IX",
          FEN("B:W17,19,21,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,16"),
          "1. cb4 hg5 2. gf4",
          "cb4 hg5 gf4".some
        ),
        StartingPosition(
          "15-X",
          FEN("B:W17,20,21,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,12,15"),
          "1. gh4 fe5 2. cb4",
          "gh4 fe5 cb4".some
        )
      )
    )
  )

}
