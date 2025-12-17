# Pac-Man Minimax

A Haskell Pac-Man solver/playback using minimax with alpha–beta pruning, with an optional parallel evaluation of successors. Includes BFS-based heuristics to reduce oscillation and improve decisions around walls.

## Features
- Sequential minimax with alpha–beta pruning.
- Parallel sibling evaluation via `Control.Parallel.Strategies`.
- CLI to run on custom text grids and visualize states.

## Grid format
Text files with:
- `1` = Wall
- `0` = Empty
- `*` = Pellet
- `P` = Pac-Man (exactly one)
- `G` = Ghost (one or more)

Recommend a solid wall border to keep entities in bounds.

Examples in `grids/`:
- `small.txt` (8×10)
- `medium.txt` (16x28)
- `larger.txt` (28x28)

## Project layout (key modules)
- `src/Types.hs` – core data types (`Cell`, `Action`, `GameState`).
- `src/Minimax.hs` – sequential minimax and `evaluateGameState`.
- `src/MinimaxParallel.hs` – parallelized sibling evaluation (`minimaxPar`).
- `src/Plan.hs` – path planning (`planPacmanPath`, `bestPacmanAction`), greedy BFS ghosts.
- `src/Game.hs` – grid loading/parsing and simple visualization helpers.
- `app/Main.hs` – CLI entry point.

## Build and run

With Stack:
- Setup: `stack setup`
- Build: `stack build`
- Run: `stack exec pacman-minimax-exe`

CLI options:
- Use parallel mode: `--par` (or `--seq` for sequential)
- Visualize states: `--visualize`
- Choose grid: `--grid path/to/grid_file.txt`

Example:
```
stack exec pacman-minimax-exe -- --visualize --grid grids/medium.txt
```

Multiple cores: try `+RTS -N`, e.g.:
```
stack exec pacman-minimax-exe -- --par --grid grids/medium.txt +RTS -N
```

## Benchmarks

| # cores | time elapsed | speedup factor |
| ------- | ------------ | -------------- |
| 1       | 14.012       | 1              |
| 2       | 12.158       | 1.152492186    |
| 3       | 9.544        | 1.468147527    |
| 4       | 7.227        | 1.938840459    |
| 5       | 5.48         | 2.556934307    |
| 6       | 4.987        | 2.809705234    |
| 7       | 4.761        | 2.943079185    |
| 8       | 4.733        | 2.960490175    |

Run data regarding time and utilization were collected as follows:

For one core:

`stack exec pacman-minimax-exe -- --grid grids/large.txt --visualize`

For 2-8 cores:

`stack exec pacman-minimax-exe -- --grid grids/large.txt --par --visualize +RTS -N[num cores] -l`

Timing information is printed to console before and after the visualization is finished.

Seeing utilization in ThreadScope is just a matter of running:

`threadscope pacman-minimax-exe.eventlog`

After each run.

## Notes

After presenting, we realized that in reality, we are using 3 levels of parallelism due to an off-by-one error (sorry about that). This would explain the even distribution among cores, as there were questions about the branching factor of 2-3 for each step not being enough to satisfy every core. 