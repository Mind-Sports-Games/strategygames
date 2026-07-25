# 0001 — Replace the joansala Go engine with a pure-Scala engine

**Status:** Accepted (2026-07-25)

## Context

Go delegated move generation, scoring and repetition detection to the external joansala
engine (`com.joansala % go-engine`), which was slow on the hot paths (legal-move
generation, full-game replay), carried observable rules bugs, and was that dependency's
only consumer. All Go consumers already sat behind the single `Api.Position` seam, and the
codebase's contract is functional and immutable.

## Decision

The canonical variants `go9x9`/`go13x13`/`go19x19` (ids 1/2/4) run a pure-Scala engine
(`src/main/scala/go/engine/`); the `com.joansala` go-engine dependency is removed
(`aalina` stays — samurai uses it).

**Engine.** One flat byte array with a sentinel border serves all three sizes; chains are
union-find with pseudo-liberty counts; a Zobrist hash is maintained incrementally and
positional superko is enforced at move generation, so `isRepetition` is permanently false —
no repeating position is ever reachable. The public API is immutable over contained
interior mutability: a move copies the backing arrays and mutates only the copy before it
escapes. Scoring is Chinese area scoring; dead stones are settled through the existing
`ss:` flow.

**Rules-correct over parity.** Where joansala and the rules of Go disagreed, the rules
won: the engine is not a bug-for-bug replacement, so a stored joansala-era game can
validate or score differently on replay. The divergences are tabulated in
`docs/go-engine.md` ("Where it differed from joansala").

**FEN.** Parse-compatible with every stored FEN — both the 10-field format and the legacy
9-field form. Raw engine fields are honest: captures and passCount carry real values
(joansala hardcoded zeroes that the wrapper overwrote anyway). The ko field carries the
live simple-ko coordinate when one exists (`-` otherwise); it round-trips, and a parsed
coordinate is enforced by the legality check. Emitting that coordinate is the one dialect
change consumers must tolerate — stored FENs only ever carried `-`, so old data parses
unchanged.

## Alternatives

- **Keep joansala** — a slow engine with known rules bugs pinned in `build.sbt` forever.
  Rejected.
- **Bug-for-bug parity** — freezes the old engine's accidents into a new codebase.
  Rejected.
- **Situational superko (SSK)** — AGA's rule; pairs badly with the Chinese area scoring
  already in the contract, and positional superko forbids a strict superset of cycles.
  Rejected.

## Consequences

- Stored games keep their ids, urls, ratings and history; no app-side migration. The cost
  is a retroactive rules change on replay — the divergence table in `docs/go-engine.md` is
  the checklist for any downstream stored-game audit.
- Go variant ids 3, 5, 6, 7 and perfIds 503/504/505 are retired and must never be reused.
- `docs/go-engine.md` is the maintainer document: layout, divergence table, benchmarks.

## History

Developed as parallel `go*Scala` variants validated by differential testing against
joansala before the canonical ids were flipped and joansala deleted; details in git
history.
