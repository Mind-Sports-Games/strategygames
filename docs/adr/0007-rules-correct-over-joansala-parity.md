# 0007 — Rules-correct Go over bug-for-bug joansala parity

**Status:** Accepted (2026-07-24). Amended (2026-07-25, 0015): "migrating games between the
families is out of scope" is settled — there is no migration. 0015 flips ids 1/2/4 onto the
scala engine, so every divergence recorded under this ADR now applies to the canonical
variants and their stored games.

## Context

The joansala engine has observable quirks that differential tests will surface, e.g.:

- Superko is not forbidden; recreating a position ends the game as a repetition draw
  (`isRepetition` ⇒ VariantEnd), and `validDrops` filters drops leading to it.
- A consecutive pass triggers `unmakeMove` internally (`Api.makeMovesImpl`) to allow
  post-double-pass disagreement drops.

The new engine (0001) could either reproduce these exactly (making differential tests
trivially green) or implement the rules of Go correctly (making some differences
permanent and intentional).

## Decision

The new variants implement rules-correct Go. Where the two engines disagree, correctness
wins over parity. Every intentional divergence gets its own ADR recording the old
behavior, the new behavior, and why — starting with positional superko as a forbidden
move (0004) rather than an after-the-fact repetition draw. Differential tests model known
old-engine quirks explicitly; a divergence is never left as a silently failing or silently
skipped comparison.

## Alternatives

- **Bug-for-bug parity** — freezes the old engine's accidents into a new codebase and
  makes the eventual joansala retirement a second behavior change. Rejected.
- **Correctness without records** — future readers of a red differential test cannot tell
  a bug from a decision. Rejected.

## Consequences

- The differential suite is an inequality oracle: matches must match, and every mismatch
  must trace to a divergence ADR or be a bug.
- The new variants are not drop-in behavioral replacements for ids 1/2/4; migrating games
  between the families is out of scope.
- The ADR series grows by one small record per divergence found during engine work.
