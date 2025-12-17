module MinimaxParallel
  ( minimaxPar
  , bestPacmanActionPar
  ) where

import Types
import Minimax (minimax, evaluateGameState)
import qualified Data.Vector as V
import Control.Parallel.Strategies (parMap, rdeepseq)
import Data.List (maximumBy)
import Data.Ord (comparing)

-- Parallelized minimax (siblings evaluated concurrently).
-- Arguments:
--   gs: current game state
--   depth: remaining search depth
--   alpha, beta: bounds (used by deeper recursive calls)
--   maximizingPlayer: True for Pacman, False for Ghosts
minimaxPar :: GameState -> Int -> Int -> Int -> Int -> Bool -> Int
minimaxPar gs depth parLevels alpha beta maximizingPlayer
    | isTerminal gs || depth <= 0 = evaluateGameState gs
    | parLevels > 0 && maximizingPlayer =
      let actions = legalActionsPacman gs
      in case actions of
           [] -> evaluateGameState gs
           _  ->
                let children = [ applyPacmanAction gs a | a <- actions ]
                    vals     = parMap rdeepseq (\c -> minimaxPar c (depth - 1) (parLevels - 1) alpha beta False) children
                in maximum vals
    | parLevels > 0 && not maximizingPlayer =
      let jointGhostMoves = legalJointGhostActions gs
      in case jointGhostMoves of
           [] -> evaluateGameState gs
           _  ->
                let children = [ applyGhostJointActions gs jm | jm <- jointGhostMoves ]
                    vals     = parMap rdeepseq (\c -> minimaxPar c (depth - 1) (parLevels - 1) alpha beta True) children
                in minimum vals
    | otherwise = minimax gs depth alpha beta maximizingPlayer 

bestPacmanActionPar :: GameState -> Int -> Int -> Action
bestPacmanActionPar gs depth parLevels =
  let actions = legalActionsPacman gs
      scored  = parMap rdeepseq (\a -> (a, minimaxPar (applyPacmanAction gs a) (depth - 1) parLevels minBound maxBound False)) actions
  in fst $ maximumBy (comparing snd) scored

-- === Helpers ===

legalActionsPacman :: GameState -> [Action]
legalActionsPacman gs =
  filter (isLegalMove (grid gs) (pacmanPos gs)) allActions

legalActionsPerGhost :: GameState -> [[Action]]
legalActionsPerGhost gs =
  map (\pos -> filter (isLegalMove (grid gs) pos) allActions) (ghostPositions gs)

legalJointGhostActions :: GameState -> [[Action]]
legalJointGhostActions gs = cartesian (legalActionsPerGhost gs)

applyPacmanAction :: GameState -> Action -> GameState
applyPacmanAction gs act =
  let g         = grid gs
      fromPos   = pacmanPos gs
      toPos     = movePosition fromPos act
      targetPos = if isLegalMove g fromPos act then toPos else fromPos
      cell      = cellAt g targetPos
      atePellet = cell == Pellet
      newGrid   = if atePellet then setCell g targetPos Empty else g
      newScore  = score gs + (if atePellet then 10 else 0)
      newPelRem = pelletsRemaining gs - (if atePellet then 1 else 0)
      collided  = targetPos `elem` ghostPositions gs
      terminal  = collided || newPelRem <= 0
      deathP    = if collided then Just targetPos else deathPos gs
  in gs { grid             = newGrid
        , pacmanPos        = targetPos
        , score            = newScore
        , pelletsRemaining = newPelRem
        , isTerminal       = terminal
        , deathPos         = deathP
        }

applyGhostJointActions :: GameState -> [Action] -> GameState
applyGhostJointActions gs acts =
  let g           = grid gs
      ghosts      = ghostPositions gs
      movedGhosts = zipWith (\pos act ->
                              let np = movePosition pos act
                              in if isLegalMove g pos act then np else pos
                            ) ghosts acts
      collided    = pacmanPos gs `elem` movedGhosts
      terminal    = collided || pelletsRemaining gs <= 0
      deathP      = if collided then Just (pacmanPos gs) else deathPos gs
  in gs { ghostPositions = movedGhosts
        , isTerminal     = terminal
        , deathPos       = deathP
        }

movePosition :: Position -> Action -> Position
movePosition (x, y) act = case act of
  MoveUp    -> (x, y - 1)
  MoveDown  -> (x, y + 1)
  MoveLeft  -> (x - 1, y)
  MoveRight -> (x + 1, y)

inBounds :: V.Vector (V.Vector Cell) -> Position -> Bool
inBounds g (x, y) =
  y >= 0 && y < V.length g &&
  x >= 0 && not (V.null g) && x < V.length (g V.! y)

cellAt :: V.Vector (V.Vector Cell) -> Position -> Cell
cellAt g (x, y)
  | inBounds g (x, y) = (g V.! y) V.! x
  | otherwise         = Wall

isLegalMove :: V.Vector (V.Vector Cell) -> Position -> Action -> Bool
isLegalMove g pos act =
  let np = movePosition pos act
  in inBounds g np && cellAt g np /= Wall

setCell :: V.Vector (V.Vector Cell) -> Position -> Cell -> V.Vector (V.Vector Cell)
setCell g (x, y) c =
  g V.// [(y, (g V.! y) V.// [(x, c)])]

-- Cartesian product
cartesian :: [[a]] -> [[a]]
cartesian []       = [[]]
cartesian (xs:xss) = [ x:ys | x <- xs, ys <- cartesian xss ]