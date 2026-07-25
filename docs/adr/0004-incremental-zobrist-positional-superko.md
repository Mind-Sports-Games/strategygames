# 0004 — Incremental Zobrist hashing; positional superko via hash history

**Status:** Accepted (2026-07-24)

## Context

Go forbids recreating an earlier whole-board position (superko). The old engine does not
forbid such moves; it reports repetition after the fact (`isRepetition` ⇒ variant end /
draw), and `validDrops` filters drops that lead to it. Detecting repetition by comparing
full board states per candidate move is O(board) per check and would dominate legal-move
generation.

## Decision

- **Hashing:** precomputed `Long` Zobrist tables indexed by (point × color), XORed
  incrementally on every stone placement and removal. The current position hash is always
  available in O(1).
- **History:** each state keeps the set of prior position hashes (`Set[Long]`).
  **Positional superko** = candidate move's resulting hash already in the set.
- **Cheap candidate hashes:** only capturing moves can recreate an earlier position, so the
  superko check runs only for them, and the resulting hash is computed by XORing the placed
  stone and the removed stones — no board mutation or copy needed for the test.

## Alternatives

- **Full position snapshots + equality compare** — O(board) per candidate and O(history)
  memory in board copies. Rejected.
- **Hash only, computed from scratch per position** — O(board) per move; discards the
  incrementality the mutation path gives for free. Rejected.
- **Simple ko only (last-position check)** — misses long cycles; not rules-correct
  (see 0007). Rejected as the only mechanism; the simple-ko square is still tracked
  because the FEN format's third field requires it (see 0006).

## Consequences

- Superko checking is O(captured stones) per capturing candidate and O(1) for the rest.
- 64-bit hash collisions are theoretically possible (~2⁻⁶⁴ per pair); accepted — this is
  the standard trade in Go engines.
- Forbidding the move up front is a behavioral divergence from the old engine's
  repetition-ends-game model; recorded separately per 0007, one ADR per divergence.
- Hash history becomes part of engine state and must survive the copy-on-apply discipline
  of 0005 (a persistent `Set` makes this free).
