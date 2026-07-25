# 0002 — Keep `Api.Position` as the single engine-agnostic seam

**Status:** Accepted (2026-07-24)

## Context

`strategygames.go.Api.Position` (abstract class, `src/main/scala/go/Api.scala`) is already
the boundary between the go game logic and its engine: Board, Situation, Game, Forsyth,
Replay, and `go.variant.Variant` all consume it. Nothing outside `go/Api.scala` touches
joansala types except one leak: `Position.setBoard(goBoard: GoBoard)` takes a joansala
`GoBoard` on the abstract surface.

The pure-Scala engine (0001) needs a place to plug in without changing every consumer.

## Decision

`Api.Position` stays the single seam. The joansala leak is removed from its surface
(`setBoard` refactored off the abstract class) before any new-engine code lands. The new
engine implements the cleaned seam as `ScalaPosition`; dispatch happens by variant key in
`Api.positionFromVariant`, `Api.positionFromVariantNameAndFEN`, and `Api.initialFen`.
`ScalaPosition` carries its variant explicitly rather than inferring it from FEN board size,
because size-based inference maps to the old variants.

## Alternatives

- **A new parallel seam (trait) for the new engine** — every consumer would need to branch
  on which seam it holds; the existing abstract class already is the trait. Rejected.
- **Plug in below the seam by reimplementing joansala's own interfaces** — ties the new
  engine to a foreign API shape we intend to delete. Rejected.

## Consequences

- Consumers (Board/Situation/Game/Forsyth/Replay/variant) are untouched by the engine swap;
  the diff is confined to `go/Api.scala`, `go/variant/`, and the new engine package.
- The seam-cleaning refactor is a hard prerequisite: no engine work lands while
  `setBoard(GoBoard)` remains on the surface.
- Callers of `Forsyth.<<` / `fen.variant` must be audited: any variant-from-FEN inference
  silently routes to the old engine.
