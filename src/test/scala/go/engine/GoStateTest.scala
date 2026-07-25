package strategygames.go.engine

import org.specs2.mutable.Specification

class GoStateTest extends Specification {

  import GoEngineTestSupport._

  "initial state" should {
    "offer the pass alongside the drops and give black the move" in {
      forall(List(9, 13, 19)) { size =>
        val state = GoState.initial(size)
        (state.legalMoves.toList must contain(state.passMove)) and
          (state.playerTurn === 1) and
          (state.turn === "b")
      }
    }
    "reject out of range moves" in {
      val state = GoState.initial(9)
      (state.isLegal(-1) === false) and
        (state.isLegal(state.passMove + 1) === false) and
        (state.isLegal(state.passMove) === true)
    }
    "hash a placement to the value every other process hashes it to" in {
      GoState.initial(9)(engineMove(9, "e5")).positionHash === 7651700626678406181L
    }
  }

  "occupied points" should {
    "be illegal for the opponent" in {
      val state = playAll(9, List("e5"))
      (state.isLegal(engineMove(9, "e5")) === false) and
        (state.legalMoves.toList must not(contain(engineMove(9, "e5"))))
    }
  }

  "corner capture" should {
    "remove the cornered stone on every board size" in {
      forall(List((9, "a2", "a1", "b1"), (13, "l1", "m1", "m2"), (19, "r19", "s19", "s18"))) {
        case (size, first, cornered, capturing) =>
          val state = playAll(size, List(first, cornered, capturing))
          (state.stoneOwnerAt(engineMove(size, cornered)) === 0) and
            (state.capturesByBlack === 1) and
            (state.capturesByWhite === 0) and
            (state.simpleKoMove === None) and
            (state.playerTurn === -1) and
            (state.turn === "w")
      }
    }
  }

  "multi chain capture" should {
    "remove two separate enemy chains with one stone" in {
      val state = playAll(9, List("a3", "a2", "b2", "b1", "c1", "e5", "a1"))
      (state.capturesByBlack === 2) and
        (state.stoneOwnerAt(engineMove(9, "a2")) === 0) and
        (state.stoneOwnerAt(engineMove(9, "b1")) === 0) and
        (state.stoneOwnerAt(engineMove(9, "a1")) === 1) and
        (state.simpleKoMove === None)
    }
  }

  "suicide" should {
    "forbid a single stone suicide" in {
      val state = playAll(9, List("a2", "e5", "b1"))
      (state.isLegal(engineMove(9, "a1")) === false) and
        (state.legalMoves.toList must not(contain(engineMove(9, "a1")))) and
        (state(engineMove(9, "a1")) must throwAn[IllegalArgumentException])
    }
    "forbid a multi stone chain suicide" in {
      val state = playAll(9, List("a2", "a3", "i9", "b2", "i8", "b1"))
      (state.isLegal(engineMove(9, "a1")) === false) and
        (state(engineMove(9, "a1")) must throwAn[IllegalArgumentException])
    }
  }

  "simple ko" should {
    val koSequence = List("g5", "f5", "f4", "e4", "f6", "d5", "h5", "e6", "e5")
    "record the ko point and forbid immediate recapture" in {
      val state = playAll(9, koSequence)
      (state.simpleKoMove === Some(engineMove(9, "f5"))) and
        (state.capturesByBlack === 1) and
        (state.stoneOwnerAt(engineMove(9, "f5")) === 0) and
        (state.stoneOwnerAt(engineMove(9, "e5")) === 1) and
        (state.isLegal(engineMove(9, "f5")) === false) and
        (state.legalMoves.toList must not(contain(engineMove(9, "f5"))))
    }
    "allow the recapture after an exchange elsewhere" in {
      val state      = playAll(9, koSequence ++ List("a1", "c9"))
      val recaptured = state(engineMove(9, "f5"))
      (state.isLegal(engineMove(9, "f5")) === true) and
        (recaptured.simpleKoMove === Some(engineMove(9, "e5"))) and
        (recaptured.capturesByWhite === 1) and
        (recaptured.isLegal(engineMove(9, "e5")) === false)
    }
    "clear the ko point on a pass" in {
      val state  = playAll(9, koSequence)
      val passed = state(state.passMove)
      (passed.simpleKoMove === None) and
        (passed.consecutivePasses === 1) and
        (passed.positionHash === state.positionHash) and
        (passed.playerTurn === 1)
    }
  }

  "snapback" should {
    val setup = List("a3", "a2", "b3", "b2", "c2", "c1", "b1", "a1")
    "not record a ko point when the capturing stone joins a bigger chain" in {
      val state = playAll(9, setup)
      (state.capturesByWhite === 1) and
        (state.simpleKoMove === None) and
        (state.isLegal(engineMove(9, "b1")) === true)
    }
    "allow the immediate recapture of the bigger chain" in {
      val state = playAll(9, setup ++ List("b1"))
      (state.capturesByBlack === 3) and
        (state.stoneOwnerAt(engineMove(9, "a1")) === 0) and
        (state.stoneOwnerAt(engineMove(9, "a2")) === 0) and
        (state.stoneOwnerAt(engineMove(9, "b2")) === 0) and
        (state.stoneOwnerAt(engineMove(9, "b1")) === 1) and
        (state.stoneOwnerAt(engineMove(9, "c1")) === -1)
    }
  }

  "positional superko" should {
    "forbid the returning capture of a send-two-return-one cycle" in {
      val state = playAll(9, List("d1", "a2", "c2", "b2", "a1", "pass", "b1", "c1", "a1", "pass"))
      (state.playerTurn === 1) and
        (state.capturesByWhite === 2) and
        (state.isLegal(engineMove(9, "b1")) === false) and
        (state.legalMoves.toList must not(contain(engineMove(9, "b1")))) and
        (state(engineMove(9, "b1")) must throwAn[IllegalArgumentException])
    }
    "forbid recreating a position through a triple ko" in {
      val state = playAll(
        9,
        List(
          "b8",
          "b7",
          "c9",
          "c6",
          "d8",
          "d7",
          "f8",
          "f7",
          "g9",
          "g6",
          "h8",
          "h7",
          "b2",
          "b3",
          "c1",
          "c4",
          "d2",
          "d3",
          "f2",
          "f3",
          "g1",
          "g4",
          "h2",
          "h3",
          "c7",
          "c2",
          "g3",
          "g8",
          "g7",
          "c8",
          "c3",
          "g2",
          "c7",
          "c2"
        )
      )
      (state.isLegal(engineMove(9, "g3")) === false) and
        (state.legalMoves.toList must not(contain(engineMove(9, "g3"))))
    }
  }

  "position hash" should {
    "be stable under transposition" in {
      val first  = playAll(9, List("e5", "c3", "e6"))
      val second = playAll(9, List("e6", "c3", "e5"))
      (first.positionHash === second.positionHash) and
        (first.positionHash !== GoState.initial(9).positionHash)
    }
    "match a state rebuilt from its stone owners" in {
      val state   = playAll(9, List("a3", "a2", "b3", "b2", "c2", "c1", "b1", "a1", "b1"))
      val rebuilt = GoState.fromStoneOwners(
        9,
        state.stoneOwnerAt,
        state.playerTurn,
        state.capturesByBlack,
        state.capturesByWhite,
        state.simpleKoMove,
        state.consecutivePasses
      )
      (rebuilt.positionHash === state.positionHash) and
        (rebuilt.legalMoves.toList === state.legalMoves.toList)
    }
  }

  "pass" should {
    "flip the turn and keep the position hash" in {
      val state  = GoState.initial(9)
      val passed = state(state.passMove)
      (passed.playerTurn === -1) and
        (passed.consecutivePasses === 1) and
        (passed.positionHash === state.positionHash) and
        (passed.inDeadStoneSelectionPhase === false)
    }
    "enter the selection phase after two consecutive passes" in {
      val state = playAll(9, List("pass", "pass"))
      (state.consecutivePasses === 2) and
        (state.inDeadStoneSelectionPhase === true) and
        (state.playerTurn === 1)
    }
    "reset the pass count on a stone placement" in {
      val state = playAll(9, List("pass", "pass", "e5"))
      (state.consecutivePasses === 0) and
        (state.inDeadStoneSelectionPhase === false)
    }
  }

  "immutability" should {
    "leave the original state unchanged after a placement" in {
      val initial = GoState.initial(9)
      val next    = initial(engineMove(9, "e5"))
      (initial.stoneOwnerAt(engineMove(9, "e5")) === 0) and
        (next.stoneOwnerAt(engineMove(9, "e5")) === 1) and
        (initial.playerTurn === 1) and
        (initial.positionHash === GoState.initial(9).positionHash) and
        (initial.legalMoves.length === 82)
    }
    "leave the original state unchanged after a capture" in {
      val before = playAll(9, List("g5", "f5", "f4", "e4", "f6", "d5", "h5", "e6"))
      val after  = before(engineMove(9, "e5"))
      (before.stoneOwnerAt(engineMove(9, "f5")) === -1) and
        (after.stoneOwnerAt(engineMove(9, "f5")) === 0) and
        (before.capturesByBlack === 0) and
        (after.capturesByBlack === 1)
    }
    "not let callers mutate the cached legal moves" in {
      val state    = GoState.initial(9)
      val exposed  = state.legalMoves
      val original = exposed(0)
      exposed(0) = -999
      state.legalMoves(0) === original
    }
  }
}
