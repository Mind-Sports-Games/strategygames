# Go speed dive: results

The pure-Scala go engine replaced joansala. Numbers below are JMH-measured on the landed code,
2026-07-25; protocol in [docs/go-engine-speed-dive.md](go-engine-speed-dive.md).

## Full-game replay (µs per game, lower is better)

| board | joansala (removed) | per-ply path | batch default | batch vs joansala |
| ----- | ------------------- | ------------ | -------------- | ------------------ |
| 9x9   | 7,395                | 332          | 72.7           | 102x                |
| 13x13 | 24,313               | 648          | 122.0          | 199x                |
| 19x19 | 96,385               | 2,127        | 277.6          | 347x                |

## Legal-move list for a client (validDrops, µs per call)

| board | before (eager) | after (lazy) | speedup |
| ----- | -------------- | ------------ | ------- |
| 9x9   | 39.1           | 1.9          | 20x     |
| 13x13 | 202.2          | 5.9          | 34x     |
| 19x19 | 794.0          | 13.9         | 57x     |

Allocation per 19x19 replay dropped 8.57MB → 566KB (15.2x).

See [docs/go-engine-speed-dive.md](go-engine-speed-dive.md) for methodology, error bars, and the
full investigation.
