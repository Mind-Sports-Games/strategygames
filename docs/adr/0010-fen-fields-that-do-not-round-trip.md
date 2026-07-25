# 0010 — Two FEN fields deliberately do not round-trip: the ko point and handicap scores

**Status:** Accepted (2026-07-24). Amended (2026-07-25): a parsed ko point is now enforced
by `GoState.isLegal`. Amended (2026-07-25, 0015): the joansala validator whose literal-`-`
regex forced the ko field to `-` dies with the engine, so `GoFen.render` now emits the real
ko coordinate when a simple ko is active (`-` otherwise) and the `GoFen`-based `validateFEN`
accepts it, using `go.Pos`'s a–s alphabet including `i`; the "always writes `-`" decision
below is superseded for the post-flip engine. Stored FENs always carried `-`, so parsing old
data is unaffected.

## Context

Two fields of the FEN format of 0006 carry no information out of either engine, and the
new engine reproduces that on purpose.

**Ko point (field 3).** joansala always emits a literal `-`, even when a simple ko exists.
Verified on the 19x19 ko position `2 59 20 39 22 41 40 21`, where both engines forbid the
recapture at move 40 and both still emit `-`. The joansala emitter cannot be "fixed",
because `Api.validateFEN` matches against a regex whose ko field is the literal `- `:

```
([0-9Ss]?){1,19}(/([0-9Ss]?){1,19}){8,18}\[[Ss]+\] [w|b] - [0-9]+ ...
```

Emitting a real coordinate there would make every such FEN fail validation. A second
hazard sits behind that one: joansala's coordinate alphabet skips `i` (a–h, j–t) while
`strategygames.go.Pos` uses a–s including `i`, so an emitted ko coordinate would not even
mean the same square on both sides.

**Handicap scores (fields 4 and 5).** `Go9x9.fenFromSetupConfig(4, 55)` writes `40 55` into
the score fields; both engines re-score the position on load and emit `810 55`. Neither
engine preserves the setup values.

## Decision

- `GoFen.render` always writes `-` for the ko point (`KoPointOmittedByValidator`), while
  `GoFen.parse` *accepts* a coordinate there (`parseKoPoint`, `GoFenError.MalformedKoPoint`)
  so a hand-written or future FEN carrying one is not rejected. Asymmetric on purpose:
  liberal in what it accepts, bug-compatible in what it emits.
- Scores are always recomputed from the board on load; the incoming score fields are parsed
  for shape only (`parseNumber("p1Score", …)`, result discarded) and never trusted.

## Alternatives

- **Emit the real ko point** — breaks `Api.validateFEN` for every position with a ko, and
  reintroduces the `i`-file alphabet mismatch. Rejected; this is the one place where
  bug-for-bug parity beats correctness, and 0007's "correctness wins" yields to a hard
  format constraint.
- **Trust incoming score fields** — lets a FEN assert a score its board contradicts.
  Rejected; area scoring is a pure function of the position (0006).

## Consequences

- The simple-ko square survives only inside engine state (`GoState.simpleKoMove`), never
  across a FEN boundary. A position reconstructed from a FEN this engine emitted loses it,
  exactly as on joansala — one more instance of the FEN-cannot-express-history consequence
  recorded in 0006 for superko history.
- A FEN that *does* carry a coordinate there is honoured: `GoState.isLegal` refuses the
  point it names. Superko makes that redundant in a played-out game, but a state rebuilt
  from a FEN starts with an empty position history, so the ko field is the only recapture
  protection such a position has — enforcing it is 0007's correctness-first rule applied to
  the one field the emitter cannot write.
- Handicap setup FENs must not be used as round-trip fixtures; the differential corpus
  compares post-load FENs, which agree.
- Not observable through the contract: both engines emit the same `-` and the same
  recomputed scores.
