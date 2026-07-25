# 0014 — Positional superko (PSK), not situational superko (SSK)

**Status:** Accepted (2026-07-24). Amended (2026-07-25, 0015): the "future migration of the
old variants" exposure below is now accepted fact — 0015 flips ids 1/2/4 onto the scala
engine, so a stored joansala game containing an opposite-player board repeat will no longer
replay on the canonical variants.

## Context

0004 and 0011 settled *when* superko is enforced (at move generation, not after the fact).
They did not settle *which* superko rule. Two are in live use:

- **Positional superko (PSK)** — a play may not recreate any previous *board position*.
  New Zealand rules 6.1, and Chinese rules art. 6 on a strict reading.
- **Situational superko (SSK)** — a play may not recreate a previous board position *with
  the same player to move*. AGA rules. (Japanese rules forbid only the simple two-stone ko
  and have no superko rule at all.)

They differ only when a position recurs with the opposite player to move, which on a real
board requires an odd number of intervening passes. The scala engine implements PSK:
`GoState.occurredPositionHashes` keys on the stone arrangement alone, and `afterPass`
deliberately records no hash, so a repeat separated by a pass still collides.

### The counter-example

The repo already carries this position as a regression test: `GoApiTest` "(Issue#490) Go
game with a recapture of more than one stone after ko point". 9x9, engine move numbers
`40 49 48 58 57 76 67 68 59 66 50 81 67 49 81` and then the disputed `58` — as UCI,
`s@e5 s@e6 s@d6 s@e7 s@d7 s@e9 s@e8 s@f8 s@f7 s@d8 s@f6 pass s@e8 s@e6 pass` then `s@e7`.

| ply | action | board | to move |
|-----|--------|-------|---------|
| 11 | `s@f6` (P1) | `4s4/3s1s3/3SsS3/3SsS3/4S4/9/9/9/9` | P2 |
| 12 | `pass` (P2) | unchanged | P1 |
| 13 | `s@e8` (P1) | `4s4/3sSs3/3S1S3/3S1S3/4S4/9/9/9/9` | P2 |
| 14 | `s@e6` (P2) | `4s4/3sSs3/3S1S3/3SsS3/4S4/9/9/9/9` | P1 |
| 15 | `pass` (P1) | unchanged | P2 |
| 16 | `s@e7` (P2) | `4s4/3s1s3/3SsS3/3SsS3/4S4/9/9/9/9` | P1 |

The board after ply 16 is byte-identical to the board after ply 11, but ply 11 had P2 to
move and ply 16 has P1 to move. The two passes flip the parity. Under SSK ply 16 is legal;
under PSK it is not.

Both engines at ply 15, with `s@e7` pending:

| | joansala `go9x9` | scala `go9x9Scala` |
|---|---|---|
| `apiPosition.legalDrops.contains(58)` | `true` | `false` |
| `situation.drops` size | `71` | `70` |
| `situation.drops` contains `e7` | `true` | `false` |
| playing `s@e7` | accepted, `isRepetition === false`, game continues | rejected: `Situation(…) cannot perform the drop: Stone on e7` |

Upstream `BasicTests.notRepetitionOnPassTest` (9x9 `80 71 79 70 69 62 77 78 79 53 80 78 79
pass`) is the same shape: ply 13's returning capture recreates the ply-10 board with the
other player to move, joansala plays it and reports `isRepetition() == false`.

Unlike every other divergence in 0008–0013, **this one is visible through the contract
surface**: the wrapper drop lists differ, and a game joansala accepts cannot be replayed on
the scala engine at all.

## Decision

**Keep positional superko.** The scala variants forbid recreating any earlier board
position regardless of whose turn it is. joansala's behaviour is not adopted.

Reasons, in order of weight:

1. **Consistency with the scoring rules already chosen.** 0006 commits to Chinese area
   scoring. PSK is the superko rule of the Chinese/New Zealand family; SSK belongs to the
   AGA ruleset, which pairs it with different scoring and pass-stone conventions. Mixing
   Chinese scoring with AGA ko would be a ruleset nobody plays.
2. **PSK is strictly the safer superset.** Every cycle SSK forbids, PSK also forbids. PSK
   makes board repetition unreachable outright, which is what lets `isRepetition` be
   permanently false (0011) and removes repetition draws from the scala variants entirely.
3. **Already implemented and pinned.** PLAN.md pre-committed to it ("Rules-correct
   positional superko (forbid the move) is a known divergence → ADR"), `GoStateTest`
   "positional superko" pins the send-two-return-one cycle
   (`d1 a2 c2 b2 a1 pass b1 c1 a1 pass`, then `b1` must be illegal) — precisely the cycle
   PSK forbids and SSK permits — and `GoDifferentialTest` allowlists the raw legal-set
   difference explicitly rather than tolerating it.

## Alternatives

- **Switch to situational superko.** The implementation is small and known: fold
  `playerTurn` into the hash recorded in `GoState.occurredPositionHashes` and into the
  predicted hash computed in `isLegalPlacement`. This would erase this divergence entirely
  and keep the triple-ko test green, but it would flip the `GoStateTest`
  send-two-return-one assertion, which would have to be rewritten to the SSK expectation,
  and it would pair AGA ko with Chinese scoring. Recorded, not chosen. If this is ever
  revisited, supersede this ADR rather than editing it.
- **Simple ko only (Japanese).** Permits long cycles and non-terminating games. Rejected
  already in 0004.

## Consequences

- A small class of pass-mediated repeats that are legal on `go9x9`/`go13x13`/`go19x19`
  today are illegal on `go9x9Scala`/`go13x13Scala`/`go19x19Scala`. The scala variants are
  new keys (ids 5/6/7) with no existing games, so nothing breaks now; the exposure is a
  future migration of the old variants, where a stored game containing such a move could
  not be replayed. This is the concrete form of the "not drop-in behavioral replacements"
  consequence recorded in 0007.
- The rules *content* of upstream Issue#490 — a simple-ko point must not block a recapture
  of more than one stone — is preserved and passes unchanged; only the pass-mediated
  continuation past ply 15 diverges.
- The divergence is pinned from both sides: the `issue490-multi-stone-recapture` replay case
  in `GoDifferentialTest` stops at ply 15 and carries
  `superkoActionsByPly = Map(15 -> List(58))`, and a dedicated example asserts that joansala
  offers `e7`, that the scala engine does not, and that the board after joansala plays it
  equals the ply-11 board with the opposite player to move.
- Any future engine work that changes hashing must keep passes out of the hash history;
  recording a hash on a pass would silently convert PSK into something weaker than either
  rule.
