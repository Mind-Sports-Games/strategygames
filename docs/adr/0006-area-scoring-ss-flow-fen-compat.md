# 0006 — Chinese area scoring, dead stones via the existing `ss:` flow, byte-identical FEN

**Status:** Accepted (2026-07-24)

## Context

The go game logic already commits to area (Chinese) scoring and a specific end-of-game
protocol: pass, pass ⇒ `canSelectSquares`; a `ss:<squares>` action removes agreed dead
stones and ends the game (passCount=3 semantics in FEN); empty `ss:` is legal; a third
consecutive pass auto-triggers `ss:`. Komi is 5.5 (9x9) / 7.5 (13x13, 19x19); scores and
komi are stored as x10 integers. The FEN format is:

```
board[SSSSSSSSSSssssssssss] turn ko p1Score p2Score p1Captures p2Captures komi passCount fullMove
```

with `S`/`s`/run-length board encoding, the simple-ko square (or `-`) as field 3, and an
old-style 9-field form (no passCount) that must still parse. Stored games, the PlayStrategy
app, and the differential tests all speak this format.

## Decision

The new engine adopts all of it unchanged:

- **Scoring:** area (Chinese). Empty regions are flood-filled; a region counts for a color
  iff bordered only by that color; score = stones + territory; white adds komi, black adds
  any handicap compensation. Draw when scores are equal.
- **Dead stones:** the existing pass/pass ⇒ `ss: SelectSquares` flow, verbatim — `ss:`
  removes the listed stones, rescores, and ends the game.
- **FEN:** emit and parse byte-identical to the current format, including the simple-ko
  field and x10 integer scores, and including parsing the old 9-field form.

## Alternatives

- **Territory (Japanese) scoring** — a different game contract than the one live variants,
  stored scores, and komi values already encode. Rejected.
- **Automatic dead-stone determination (no `ss:`)** — changes the interaction protocol the
  app and Situation flow (`canSelectSquares`) are built on, and life-and-death judgment is
  out of scope for a rules engine. Rejected.
- **A new FEN dialect for the new variants** — would fork every FEN consumer (Forsyth,
  Replay, app, tests) on variant. Rejected.

## Consequences

- Differential tests can compare old and new engines FEN-for-FEN; fixtures and stored
  games work on both.
- The engine tracks state it does not need for its own rules (simple-ko square, x10
  captures/scores mid-game) purely to satisfy the format.
- FEN cannot express superko history (0004); positions reconstructed from FEN alone start
  with an empty hash history. Accepted: replays feed full action sequences.
- FEN still cannot identify which engine family a position belongs to — variant must
  travel out of band (see 0002 consequences).
