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

    "have the correct boardAfter P1" in {
      val board = Board(FEN("W:Wd2:B:H0:F1").pieces, variant.Dameo)
      val moves = Situation(board, P1).actors.find(_.pos == Pos.D2).get.noncaptures
      moves.length must_== 3
      moves.map(_.after.pieces).toSet must_== Set(
        FEN("W:Wc3:B:H0:F1").pieces,
        FEN("W:Wd3:B:H0:F1").pieces,
        FEN("W:We3:B:H0:F1").pieces
      )
    }

    "have the correct boardAfter P2" in {
      val board = Board(FEN("W:W:Bd4:H0:F1").pieces, variant.Dameo)
      val moves = Situation(board, P2).actors.find(_.pos == Pos.D4).get.noncaptures
      moves.length must_== 3
      moves.map(_.after.pieces).toSet must_== Set(
        FEN("W:W:Bc3:H0:F1").pieces,
        FEN("W:W:Bd3:H0:F1").pieces,
        FEN("W:W:Be3:H0:F1").pieces
      )
    }

    "have the correct situationAfter P1 promotion" in {
      val board = Board(FEN("W:Wd7:B:H0:F1").pieces, variant.Dameo)
      val moves = Situation(board, P1).actors.find(_.pos == Pos.D7).get.noncaptures
      moves.length must_== 3
      moves.map(_.situationAfter.board.pieces).toSet must_== Set(
        FEN("W:Wc8.k:B:H0:F1").pieces,
        FEN("W:Wd8.k:B:H0:F1").pieces,
        FEN("W:We8.k:B:H0:F1").pieces
      )
    }

    "have the correct situationAfter P2 promotion" in {
      val board = Board(FEN("W:W:Bd2:H0:F1").pieces, variant.Dameo)
      val moves = Situation(board, P2).actors.find(_.pos == Pos.D2).get.noncaptures
      moves.length must_== 3
      moves.map(_.situationAfter.board.pieces).toSet must_== Set(
        FEN("W:W:Bc1.k:H0:F1").pieces,
        FEN("W:W:Bd1.k:H0:F1").pieces,
        FEN("W:W:Be1.k:H0:F1").pieces
      )
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
      move.situationAfter.player == P2
    }

    "don't switch players when more captures are possible" in {
      val board = Board(FEN("W:Wc4:Bc5,d6:H0:F1").pieces, variant.Dameo)
      val man = Situation(board, P1).actors.find(_.pos == Pos.C4).get
      val move = man.captures(0)
      move.autoEndTurn must_== false
      move.situationAfter.player == P1
    }

    "don't allow capture over ghosts" in {
      val board = Board(FEN("W:Wc4:Bc5.g,d4,d6,e5.g:H0:F1").pieces, variant.Dameo)
      val man = Situation(board, P1).actors.find(_.pos == Pos.C4).get
      man.captures.length must_== 1
    }

    "remove captured piece" in {
      val board = Board(FEN("W:Wc5:Bc6:H0:F1").pieces, variant.Dameo)
      val man = Situation(board, P1).actors.find(_.pos == Pos.C5).get
      man.captures.length must_== 1
      val move = man.captures(0)
      move.autoEndTurn must_== true
      move.situationAfter.player == P2
      move.situationAfter.board.pieces must_== FEN("W:Wc7:B:H0:F1").pieces
    }

    "leave ghosts and make capturing piece active after partial capture" in {
      val board = Board(FEN("W:Wc5:Bc6,d7:H0:F1").pieces, variant.Dameo)
      val man = Situation(board, P1).actors.find(_.pos == Pos.C5).get
      man.captures.length must_== 1
      val move = man.captures(0)
      move.autoEndTurn must_== false
      move.situationAfter.player == P1
      move.situationAfter.board.pieces must_== FEN("W:Wc7.a:Bc6.g,d7:H0:F1").pieces
    }    

    "remove all ghosts and return capturing piece to normal after full capture" in {
      val board = Board(FEN("W:Wc5:Bc2.g,c4.p,c6:H0:F1").pieces, variant.Dameo)
      val man = Situation(board, P1).actors.find(_.pos == Pos.C5).get
      man.captures.length must_== 1
      val move = man.captures(0)
      move.autoEndTurn must_== true
      move.situationAfter.player == P2
      move.situationAfter.board.pieces must_== FEN("W:Wc7:B:H0:F1").pieces
    }

    "promote P1 piece that ends on back row" in {
      val board = Board(FEN("W:Wd6:Bd7:H0:F1").pieces, variant.Dameo)
      val moves = Situation(board, P1).actors.find(_.pos == Pos.D6).get.captures
      moves.length must_== 1
      moves(0).situationAfter.board.pieces must_== FEN("W:Wd8.k:B:H0:F1").pieces
    }

    "promote P2 piece that ends on back row" in {
      val board = Board(FEN("W:Wd2:Bd3:H0:F1").pieces, variant.Dameo)
      val moves = Situation(board, P2).actors.find(_.pos == Pos.D3).get.captures
      moves.length must_== 1
      moves(0).situationAfter.board.pieces must_== FEN("W:W:Bd1.k:H0:F1").pieces
    }

    "don't promote P1 piece that captures through back row" in {
      val board = Board(FEN("W:Wd6:Bb7,c8,d7:H0:F1").pieces, variant.Dameo)
      val moves = Situation(board, P1).actors.find(_.pos == Pos.D6).get.captures
      moves.length must_== 1
      moves(0).situationAfter.board.pieces must_== FEN("W:Wd8.a:Bb7,c8,d7.g:H0:F1").pieces

      val moves2 = moves(0).situationAfter.actors.find(_.pos == Pos.D8).get.captures
      moves2.length must_== 1
      moves2(0).situationAfter.board.pieces must_== FEN("W:Wb8.a:Bb7,c8.g,d7.g:H0:F1").pieces

      val moves3 = moves2(0).situationAfter.actors.find(_.pos == Pos.B8).get.captures
      moves3.length must_== 1
      moves3(0).situationAfter.board.pieces must_== FEN("W:Wb6:B:H0:F1").pieces
    }

    "don't promote P2 piece that captures through back row" in {
      val board = Board(FEN("W:Wb2,c1,d2:Bd3:H0:F1").pieces, variant.Dameo)
      val moves = Situation(board, P2).actors.find(_.pos == Pos.D3).get.captures
      moves.length must_== 1
      moves(0).situationAfter.board.pieces must_== FEN("W:Wb2,c1,d2.g:Bd1.a:H0:F1").pieces

      val moves2 = moves(0).situationAfter.actors.find(_.pos == Pos.D1).get.captures
      moves2.length must_== 1
      moves2(0).situationAfter.board.pieces must_== FEN("W:Wb2,c1.g,d2.g:Bb1.a:H0:F1").pieces

      val moves3 = moves2(0).situationAfter.actors.find(_.pos == Pos.B1).get.captures
      moves3.length must_== 1
      moves3(0).situationAfter.board.pieces must_== FEN("W:W:Bb3:H0:F1").pieces
    }

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
      man.captures.length must_== 1
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
      man.captures.length must_== 1
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

    "leave ghosts and make capturing king active after partial capture" in {
      val board = Board(FEN("W:Wc5.k:Bc7,d8:H0:F1").pieces, variant.Dameo)
      val king = Situation(board, P1).actors.find(_.pos == Pos.C5).get
      king.captures.length must_== 1
      val move = king.captures(0)
      move.autoEndTurn must_== false
      move.situationAfter.player == P1
      move.situationAfter.board.pieces must_== FEN("W:Wc8.b:Bc7.g,d8:H0:F1").pieces
    }    

    "remove all ghosts and return capturing king to normal after full capture" in {
      val board = Board(FEN("W:Wc8.b:Bc7.g,d8,f8,g8:H0:F1").pieces, variant.Dameo)
      val man = Situation(board, P1).actors.find(_.pos == Pos.C8).get
      man.captures.length must_== 1
      val move = man.captures(0)
      move.autoEndTurn must_== true
      move.situationAfter.player == P2
      move.situationAfter.board.pieces must_== FEN("W:We8.k:Bf8,g8:H0:F1").pieces
    }
  }

  "active pieces" should {
    "active man has captures" in {
      val board = Board(FEN("W:Wd4.a:Bc4,d3.k,d5,e4:H0:F1").pieces, variant.Dameo)
      val man = Situation(board, P1).actors.find(_.pos == Pos.D4).get
      man.captures.length must_== 4
    }

    "active king has captures" in {
      val board = Board(FEN("W:Wd4.b:Bb4,d2.k,d6,g4:H0:F1").pieces, variant.Dameo)
      val man = Situation(board, P1).actors.find(_.pos == Pos.D4).get
      man.captures.length must_== 5
    }
  }
}
