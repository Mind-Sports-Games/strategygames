# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build Commands

```bash
sbt compile              # Compile the project
sbt test                 # Run all tests
sbt "testOnly **.TestNameTest"  # Run a single test
sbt scalafmtAll          # Format all code
sbt clean coverage test coverageReport  # Tests with coverage
```

## Architecture

This is **strategygames**, a Scala 2.13 game rules engine library for [playstrategy.org](https://playstrategy.org). It's a fork of [scalachess](https://github.com/lichess-org/scalachess) extended to support multiple game types.

### Core Design Pattern

The codebase uses a **wrapper pattern** to provide a unified API across different game logics. Top-level types in `src/main/scala/` (Game, Board, Situation, Move, Pos, Piece, Variant, etc.) are abstract wrappers that delegate to game-specific implementations.

```
strategygames.Game        → wraps chess.Game, draughts.DraughtsGame, go.Game, etc.
strategygames.Variant     → wraps chess.variant.Variant, go.variant.Variant, etc.
strategygames.Pos         → wraps chess.Pos, go.Pos, etc.
```

### Supported Game Logics

Each game has its own package under `src/main/scala/`:
- `chess/` - Standard chess and variants (Crazyhouse, Atomic, etc.)
- `draughts/` - International draughts variants
- `fairysf/` - Shogi, Xiangqi, Mini variants (uses fairystockfish engine)
- `samurai/` - Oware (mancala)
- `togyzkumalak/` - Togyz Kumalak (mancala variant)
- `go/` - Go (9x9, 13x13, 19x19)
- `backgammon/` - Backgammon (has dice, undo, endTurn actions)
- `abalone/` - Abalone
- `dameo/` - Dameo (draughts variant)

### Package Structure (per game)

Each game package follows a consistent structure:
- `Game.scala` - Game state and action application
- `Board.scala` - Board representation
- `Situation.scala` - Current game situation (board + player to move + derived state)
- `variant/` - Game variants
- `format/` - FEN, PGN/PDN parsing and rendering
- `opening/` - Opening book data (if applicable)

### Key Types

- `GameLogic` - Enum identifying which game engine to use
- `Action` - Union type for Move, Drop, Pass, DiceRoll, EndTurn, etc.
- `VActionStrs = Vector[Vector[String]]` - Multi-action turn history
- `Player` - P1 (first player) or P2 (second player), not "white/black"

### Adding New Games

To add a new game, create a new package with the standard structure and add wrapper cases to all top-level types (Game, Board, Situation, Pos, Piece, Move, Variant, etc.).

## Clock Notation Formats

Clock configs are defined in `Clock.scala`. Display format uses minutes for limit, seconds for grace.

| Type | Format | Example | Description |
|------|--------|---------|-------------|
| Fischer | `{limit}+{inc}` | `10+2` | Increment added after each move |
| Bronstein | `{limit} d+{delay}` | `5 d+3` | Get back time used, up to delay |
| Simple/US Delay | `{limit} d/{delay}` | `5 d/3` | Delay always added, even if you moved faster |
| Byoyomi | `{limit}+{inc}\|{byo}({periods}x)` | `10+0\|30(3x)` | Main time, then periods of byoyomi |

Limit display: minutes if divisible by 60, else `¼` (15s), `½` (30s), `¾` (45s), `1.5` (90s), or decimal.

PGN TimeControl tag uses seconds: `[TimeControl "600+2"]` → `10+2` display.

## Code Style

- Uses scalafmt 3.4.3 with `align.preset = most`
- Functional, immutable design - no side effects
- Max line length: 110 characters
