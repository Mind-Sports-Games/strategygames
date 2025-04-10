package strategygames.dameo

import strategygames.dameo.format.FEN

import org.specs2.matcher.ValidatedMatchers

class DameoActorTest extends DameoTest with ValidatedMatchers {
  "man without captures" should {
    val board = Board(variant.Dameo.initialFen.pieces, variant.Dameo)
    val actors1 = Situation(board, P1).actors
    val actors2 = Situation(board, P2).actors

    "have 3 moves" in {
      val act1 = actors1.find(_.pos == Pos.C3).get
      val act2 = actors2.find(_.pos == Pos.C6).get
      act1.noncaptures.length must_== 3
      act2.noncaptures.length must_== 3

      act1.captures.length must_== 0
      act2.captures.length must_== 0
    }

    "have 2 moves at board edge" in {
      val act1l = actors1.find(_.pos == Pos.A1).get
      val act1r = actors1.find(_.pos == Pos.H1).get
      val act2l = actors2.find(_.pos == Pos.A8).get
      val act2r = actors2.find(_.pos == Pos.H8).get
      act1l.noncaptures.length must_== 2
      act1r.noncaptures.length must_== 2
      act2l.noncaptures.length must_== 2
      act2r.noncaptures.length must_== 2

      act1l.captures.length must_== 0
      act1r.captures.length must_== 0
      act2l.captures.length must_== 0
      act2r.captures.length must_== 0
    }

    "have linear moves" in {
      val act1 = actors1.find(_.pos == Pos.D1).get
      val act2 = actors2.find(_.pos == Pos.D8).get
      act1.noncaptures.length must_== 3
      act2.noncaptures.length must_== 3

      act1.captures.length must_== 0
      act2.captures.length must_== 0
    }

    "have linear moves blocked by board edges" in {
      val board = Board(FEN("W:Wa1,a2,b1,b2,c1,c2,g1,g2,h1,h2:Ba7,a8,b7,b8,g7,g8,h7,h8:H0:F1").pieces, variant.Dameo)
      val actors1 = Situation(board, P1).actors
      val actors2 = Situation(board, P2).actors

      val act1l = actors1.find(_.pos == Pos.B1).get
      val act1r = actors1.find(_.pos == Pos.G1).get
      val act2l = actors2.find(_.pos == Pos.B8).get
      val act2r = actors2.find(_.pos == Pos.G8).get
      act1l.noncaptures.length must_== 2
      act1r.noncaptures.length must_== 2
      act2l.noncaptures.length must_== 2
      act2r.noncaptures.length must_== 2

      act1l.captures.length must_== 0
      act1r.captures.length must_== 0
      act2l.captures.length must_== 0
      act2r.captures.length must_== 0
    }

    "have moves blocked by opponent pieces" in {
      val board = Board(FEN("W:Wc2,c3,d2,d3,e2,e3:Bd4,d5,e4,e5,f4,f5:H0:F1").pieces, variant.Dameo)
      val actors1 = Situation(board, P1).actors
      val actors2 = Situation(board, P2).actors

      actors1.find(_.pos == Pos.C3).get.noncaptures.length must_== 2
      actors1.find(_.pos == Pos.D3).get.noncaptures.length must_== 1
      actors1.find(_.pos == Pos.E3).get.noncaptures.length must_== 0

      actors2.find(_.pos == Pos.F4).get.noncaptures.length must_== 2
      actors2.find(_.pos == Pos.E4).get.noncaptures.length must_== 1
      actors2.find(_.pos == Pos.D4).get.noncaptures.length must_== 0
    }

    "have no linear moves when blocked by own king" in {
      val board = Board(FEN("W:Wa2.k,b1,b2.k,b3,c2,d3.k:Bc6.k,d7,e6,e7.k,e8,f7.k:H0:F1").pieces, variant.Dameo)
      val actors1 = Situation(board, P1).actors
      val actors2 = Situation(board, P2).actors

      actors1.find(_.pos == Pos.B1).get.noncaptures.length must_== 0
      actors2.find(_.pos == Pos.E8).get.noncaptures.length must_== 0
    }
  }

  "king without captures" should {
    "move in all directions" in {
      val board = Board(FEN("W:Wc3.k:Be7.k:H0:F1").pieces, variant.Dameo)
      val actors1 = Situation(board, P1).actors
      val actors2 = Situation(board, P2).actors
      val king1 = actors1.find(_.pos == Pos.C3).get
      val king2 = actors2.find(_.pos == Pos.E7).get

      king1.noncaptures.length must_== 7 + 7 + 4 + 7
      king2.noncaptures.length must_== 7 + 7 + 4 + 5
    }

    "have long range moves blocked by other pieces" in {
      val board = Board(FEN("W:Wa1.k,b2,d1:Ba8.k,e8.k,c6:H0:F1").pieces, variant.Dameo)
      val actors1 = Situation(board, P1).actors
      val actors2 = Situation(board, P2).actors
      val king1 = actors1.find(_.pos == Pos.A1).get
      val king2 = actors2.find(_.pos == Pos.A8).get

      king1.noncaptures.length must_== 6 + 0 + 2
      king2.noncaptures.length must_== 6 + 3 + 1
    }

    "not partake in linear movement" in {
      val board = Board(FEN("W:Wb1.k,b2,b3:Ba8.k,b7,c6:H0:F1").pieces, variant.Dameo)
      val actors1 = Situation(board, P1).actors
      val actors2 = Situation(board, P2).actors
      val king1 = actors1.find(_.pos == Pos.B1).get
      val king2 = actors2.find(_.pos == Pos.A8).get

      king1.noncaptures.length must_== 14
      king2.noncaptures.length must_== 14
    }
  }

  "man with captures" should {
    "capture in all orthogonal directions" in {
      val board = Board(FEN("W:Wd4:Bc4,d3.k,d5,e4:H0:F1").pieces, variant.Dameo)
      val man = Situation(board, P1).actors.find(_.pos == Pos.D4).get
      man.captures.length must_== 4
    }

    "capture correctly near the edges" in {
      val board = Board(FEN("W:Wa2,f8,g7,h8:Ba1,a3,b2,g8:H0:F1").pieces, variant.Dameo)
      val man1 = Situation(board, P1).actors.find(_.pos == Pos.A2).get
      val man2 = Situation(board, P2).actors.find(_.pos == Pos.G8).get
      man1.captures.length must_== 2
      man2.captures.length must_== 2
    }

    "generate a move capturing the right pieces" in {
      val board = Board(FEN("W:Wb2:Bc2,d6:H0:F1").pieces, variant.Dameo)
      val man = Situation(board, P1).actors.find(_.pos == Pos.B2).get

      man.captures(0).capture must_== Some(Pos.C2)
    }

    "capture is blocked by pieces behind captured piece" in {
      val board = Board(FEN("W:Wb2,b4:Bb3,c2,d2:H0:F1").pieces, variant.Dameo)
      val man = Situation(board, P1).actors.find(_.pos == Pos.B2).get
      man.captures.length must_== 0
    }

    "have only the maximal capture sequence" in {
      val board = Board(FEN("W:Wc4:Bc5,d4,e3:H0:F1").pieces, variant.Dameo)
      val man = Situation(board, P1).actors.find(_.pos == Pos.C4).get
      man.captures.length must_== 1
      man.captures(0).capture must_== Some(Pos.D4)
    }

    "find all capture chains" in {
      val board = Board(FEN("W:Wc4:Bc5,d4,e3:H0:F1").pieces, variant.Dameo)
      val man = Situation(board, P1).actors.find(_.pos == Pos.C4).get
      man.allCaptureChains().toSet must_== Set(
        List((Pos.C5, Pos.C6)),
        List((Pos.D4, Pos.E4), (Pos.E3, Pos.E2))
      )
    }

    "have 2 equally long capture sequences" in {
      val board = Board(FEN("W:Wc4:Bc5,d4,d6,e5:H0:F1").pieces, variant.Dameo)
      val man = Situation(board, P1).actors.find(_.pos == Pos.C4).get
      man.captures.toSet.map((move: Move) => (move.dest, move.capture)) must_== Set(
        (Pos.C6, Some(Pos.C5)),
        (Pos.E4, Some(Pos.D4))
      )
    }

    "switch players when no more captures are possible" in {
      val board = Board(FEN("W:Wc4:Bc5:H0:F1").pieces, variant.Dameo)
      val man = Situation(board, P1).actors.find(_.pos == Pos.C4).get
      val move = man.captures(0)
      move.autoEndTurn must_== true
    }

    "don't switch players when more captures are possible" in {
      val board = Board(FEN("W:Wc4:Bc5,d6:H0:F1").pieces, variant.Dameo)
      val man = Situation(board, P1).actors.find(_.pos == Pos.C4).get
      val move = man.captures(0)
      move.autoEndTurn must_== false
    }

    "don't allow capture over ghosts" in {
      val board = Board(FEN("W:Wc4:Bc5.g,d4,d6,e5.g:H0:F1").pieces, variant.Dameo)
      val man = Situation(board, P1).actors.find(_.pos == Pos.C4).get
      man.captures.length must_== 1
    }

    /* These last two should probably be somewhere else i.e. in the tests
    for the move application, not move generation */
    // "leave ghosts when capture sequence is incomplete" in {}
    // "remove ghosts after full capture sequence" in {}
  }

  "king with captures" should {
    /* Very similar tests as for 'man with captures' but with a king piece */

    "capture in all orthogonal directions" in {
      val board = Board(FEN("W:Wd4.k:Bc4,d3.k,d5,e4:H0:F1").pieces, variant.Dameo)
      val man = Situation(board, P1).actors.find(_.pos == Pos.D4).get
      man.captures.length must_== 10
    }

    "capture correctly near the edges" in {
      val board = Board(FEN("W:Wa2.k,f8,g7,h8:Ba1,a3,b2,g8.k:H0:F1").pieces, variant.Dameo)
      val man1 = Situation(board, P1).actors.find(_.pos == Pos.A2).get
      val man2 = Situation(board, P2).actors.find(_.pos == Pos.G8).get
      man1.captures.length must_== 11
      man2.captures.length must_== 11
    }

    "generate a move capturing the right pieces" in {
      val board = Board(FEN("W:Wb2.k:Bc2,d6:H0:F1").pieces, variant.Dameo)
      val man = Situation(board, P1).actors.find(_.pos == Pos.B2).get

      man.captures.map(_.capture).toSet must_== Set(Some(Pos.C2))
    }

    "capture is blocked by pieces behind captured piece" in {
      val board = Board(FEN("W:Wb2.k,b4:Bb3,c2,d2:H0:F1").pieces, variant.Dameo)
      val man = Situation(board, P1).actors.find(_.pos == Pos.B2).get
      man.captures.length must_== 0
    }

    "have only the maximal capture sequence" in {
      val board = Board(FEN("W:Wc4.k:Bc5,d4,e3:H0:F1").pieces, variant.Dameo)
      val man = Situation(board, P1).actors.find(_.pos == Pos.C4).get
      man.captures.length must_== 2
      man.captures.map(_.capture).toSet must_== Set(Some(Pos.D4))
    }

    "find all capture chains" in {
      val board = Board(FEN("W:Wc4.k:Bc5,d4,e3:H0:F1").pieces, variant.Dameo)
      val man = Situation(board, P1).actors.find(_.pos == Pos.C4).get
      man.allCaptureChains(true).toSet must_== Set(
        List((Pos.C5, Pos.C6)),
        List((Pos.C5, Pos.C7)),
        List((Pos.C5, Pos.C8)),
        List((Pos.D4, Pos.F4)),
        List((Pos.D4, Pos.G4)),
        List((Pos.D4, Pos.H4)),
        List((Pos.D4, Pos.E4), (Pos.E3, Pos.E2)),
        List((Pos.D4, Pos.E4), (Pos.E3, Pos.E1)),
      )
    }

    "have 2 equally long capture sequences" in {
      val board = Board(FEN("W:Wc4.k:Bc5,d4,d6,e5:H0:F1").pieces, variant.Dameo)
      val man = Situation(board, P1).actors.find(_.pos == Pos.C4).get
      man.captures.toSet.map((move: Move) => (move.dest, move.capture)) must_== Set(
        (Pos.C6, Some(Pos.C5)),
        (Pos.E4, Some(Pos.D4))
      )
    }

    "switch players when no more captures are possible" in {
      val board = Board(FEN("W:Wc4.k:Bc5:H0:F1").pieces, variant.Dameo)
      val man = Situation(board, P1).actors.find(_.pos == Pos.C4).get
      man.captures.map(_.autoEndTurn).toSet must_== Set(true)
    }

    "don't switch players when more captures are possible" in {
      val board = Board(FEN("W:Wc4.k:Bc5,d6:H0:F1").pieces, variant.Dameo)
      val man = Situation(board, P1).actors.find(_.pos == Pos.C4).get
      val move = man.captures(0)
      move.autoEndTurn must_== false
    }

    "don't allow capture over ghosts" in {
      val board = Board(FEN("W:Wc4.k:Bc5.g,d4,d6,e5.g,f4.g:H0:F1").pieces, variant.Dameo)
      val man = Situation(board, P1).actors.find(_.pos == Pos.C4).get
      man.captures.length must_== 1
    }

    /* Below tests are specific for kings, roughly same as above but changed to use
    the long jump. */

    "long capture in all orthogonal directions" in {
      val board = Board(FEN("W:Wd4.k:Bb4,d2.k,d6,g4:H0:F1").pieces, variant.Dameo)
      val man = Situation(board, P1).actors.find(_.pos == Pos.D4).get
      man.captures.length must_== 5
    }

    "long capture correctly near the edges" in {
      val board = Board(FEN("W:Wa2.k,f8,g3,h8:Ba1,a4,f2,g8.k:H0:F1").pieces, variant.Dameo)
      val man1 = Situation(board, P1).actors.find(_.pos == Pos.A2).get
      val man2 = Situation(board, P2).actors.find(_.pos == Pos.G8).get
      man1.captures.length must_== 6
      man2.captures.length must_== 7
    }

    "generate a move long capturing the right pieces" in {
      val board = Board(FEN("W:Wb2.k:Bd2,e6:H0:F1").pieces, variant.Dameo)
      val man = Situation(board, P1).actors.find(_.pos == Pos.B2).get

      man.captures.map(_.capture).toSet must_== Set(Some(Pos.D2))
    }

    "long capture is blocked by pieces behind captured piece" in {
      val board = Board(FEN("W:Wb2.k,b5:Bb4,e2,f2:H0:F1").pieces, variant.Dameo)
      val man = Situation(board, P1).actors.find(_.pos == Pos.B2).get
      man.captures.length must_== 0
    }

    "have only the maximal (long) capture sequence" in {
      val board = Board(FEN("W:Wc4.k:Bc6,e4,f3:H0:F1").pieces, variant.Dameo)
      val man = Situation(board, P1).actors.find(_.pos == Pos.C4).get
      man.captures.length must_== 2
      man.captures.map(_.capture).toSet must_== Set(Some(Pos.E4))
    }

    "coup turc" in {
      val board = Board(FEN("W:Wc6.k:Bc4,d2,d3,e4,h3:H0:F1").pieces, variant.Dameo)
      val man = Situation(board, P1).actors.find(_.pos == Pos.C6).get
      man.captures.length must_== 1
      man.captures(0).dest must_== Pos.C2
      man.captures(0).capture must_== Some(Pos.C4)
    }

    "have 2 equally long (long) capture sequences" in {
      val board = Board(FEN("W:Wc4.k:Bc6,e4,e7,g5:H0:F1").pieces, variant.Dameo)
      val man = Situation(board, P1).actors.find(_.pos == Pos.C4).get
      man.captures.toSet.map((move: Move) => (move.dest, move.capture)) must_== Set(
        (Pos.C7, Some(Pos.C6)),
        (Pos.G4, Some(Pos.E4))
      )
    }

    "don't allow long capture over ghosts" in {
      val board = Board(FEN("W:Wc3.k:Bc5.g,d3,d6,e5.g,g3.g:H0:F1").pieces, variant.Dameo)
      val man = Situation(board, P1).actors.find(_.pos == Pos.C3).get
      man.captures.length must_== 2
    }
  }

}
