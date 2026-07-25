# 0015 — Variant identity when the joansala engine retires

**Status:** Accepted (2026-07-25). Proposed 2026-07-25 recording the open trade-off; decided
by Lakin the same day.

## Context

The parallel go variants of 0001 exist so the pure-Scala engine can prove itself against
joansala before replacing it. When joansala is deleted (see "Retiring joansala" in
`docs/go-engine.md`), the variant ids stop lining up with the engines: `go9x9`/`go13x13`/
`go19x19` (ids 1, 2, 4) name joansala-backed rulesets, `go9x9Scala`/`go13x13Scala`/
`go19x19Scala` (ids 5, 6, 7) name the pure-Scala ones, and downstream applications hold
stored games, ratings and performance history keyed by the old ids.

The two rulesets are deliberately not identical: every divergence recorded under 0007
(empty-region scoring 0008, raw FEN counts 0009, superko refusal 0011, multi-digit row
parsing 0012, post-end behaviour 0013, positional superko 0014) means a stored joansala
game replayed under the scala engine can validate differently or score differently.

## Options

**A — Flip the old ids onto the scala engine.** Point ids 1, 2 and 4 at the pure-Scala
implementation and delete ids 5–7. Stored games keep their identity, urls and ratings;
nothing migrates. The cost is a retroactive rules change: every 0007 divergence now
applies to games that were played under joansala rules, so a replay of a stored game can
reject a move joansala accepted (superko, malformed rows) or report a different score
(empty regions, FEN counts).

**B — Delete the old ids and migrate app-side.** Ids 1, 2 and 4 die with the engine;
downstream applications migrate stored games, ratings and history onto ids 5–7 (or freeze
the old games as unreplayable archives). Replay honesty is preserved — no game is ever
re-interpreted under rules it was not played by — at the cost of a coordinated migration
in every consumer of this library.

## Decision

Option A. The pure-Scala engine takes over everything: the canonical keys and ids —
`go9x9`/`go13x13`/`go19x19`, ids 1, 2 and 4 (perfIds 500/501/502) — route to the scala
engine from the flip onward, and the codebase must work completely without joansala.

The flip proceeds in two steps:

1. **Transitional state.** joansala is parked as `go9x9Joansala`/`go13x13Joansala`/
   `go19x19Joansala` (ids 5, 6, 7, perfIds 503/504/505 — the never-shipped slots of 0001
   renamed, since the `*Scala` suffix would be a lie once the scala engine holds the
   canonical ids). These exist purely as the differential oracle for validating the flip;
   they never ship downstream. `validateFEN` is rewritten over `GoFen` as part of the flip
   itself, so the last scala-variant code path through joansala code dies before the
   removal commit.
2. **Removal.** A single commit deletes the joansala engine entirely — including the
   `com.joansala` `go-engine` dependency in `build.sbt` — following the "Retiring
   joansala" playbook in `docs/go-engine.md`. (`aalina` stays; samurai uses it.)

The ko-field round-trip loss of 0010 closes as part of the flip. With the joansala
validator and its literal-`-` regex gone, `GoFen.render` emits the real ko coordinate
when a simple ko is active (`-` otherwise), and the `GoFen`-based `validateFEN` accepts
it. The coordinate alphabet is `go.File`'s a–s including `i`; the joansala alphabet that
skipped `i` dies with joansala.

## Alternatives

- **Option B (migrate app-side)** — replay honesty at the cost of a coordinated migration
  in every consumer, new urls, and severed rating history. Rejected: the stored-game
  corpus is worth more under its own identity than under its own bugs.
- **Keep joansala indefinitely as a bench/oracle dependency** — a dead engine pinned in
  `build.sbt` forever, two rule systems to reason about forever. Rejected: the bench value
  is already captured by recorded baselines, and the differential suite's job ends when
  the flip is validated.

## Consequences

- **Retroactive rules change** for stored-game replays under ids 1/2/4: every 0008–0014
  divergence now applies to the canonical variants. Headline items: positional superko is
  enforced up-front and `isRepetition` is permanently false (0011, 0014), so a
  joansala-legal stored game containing an opposite-player board repeat will no longer
  replay; raw FEN capture/pass fields report real counts instead of hardcoded zeroes
  (0009); no-colour empty regions score for nobody (0008); overrun board rows are rejected
  (0012); finished games list no actions and `makeMovesNoLegalCheck` still rejects illegal
  moves (0013). The divergence table in `docs/go-engine.md` is the checklist for any
  stored-game audit downstream.
- Stored games, urls, ratings and performance history keep their identity; no app-side
  migration.
- **Ko field compatibility:** stored FENs never carried a ko coordinate (both engines
  always emitted `-`), so parsing old data is unaffected. Newly emitted FENs for
  live-ko positions carry a coordinate, which downstream consumers must tolerate.
  0010 is amended accordingly.
- The `*Joansala` variants are transitional scaffolding only: any downstream sighting of
  ids 5/6/7 or perfIds 503/504/505 is a bug.
- After the removal commit, `Variant.usesScalaEngine` and its dispatch sites (0002)
  collapse, and FEN size inference (`go/format/FEN.scala` `variant`) may name the
  canonical variants without ambiguity.
- Foreclosed: bug-for-bug replay of joansala-era games under joansala rules. Once the
  removal commit lands, the only record of the old behaviour is the 0007–0014 ADR series
  and the recorded differential baselines.
