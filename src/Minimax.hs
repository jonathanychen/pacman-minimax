module Minimax
    ( minimax
    , evaluateGameState
    ) where

import Types
import qualified Data.Vector as V
import Data.Vector (Vector)
import Data.List (minimumBy, maximumBy)
import Data.Ord (comparing)

-- Minimax with alpha-beta pruning.
-- Arguments:
--   gs: current game state
--   depth: remaining search depth
--   alpha, beta: current alpha-beta bounds
--   maximizingPlayer: True for Pacman, False for Ghosts
minimax :: GameState -> Int -> Int -> Int -> Bool -> Int
minimax gs depth alpha beta maximizingPlayer
    -- Terminal or depth limit: evaluate
    | isTerminal gs || depth <= 0 = evaluateGameState gs
    | maximizingPlayer =
        let actions = legalActionsPacman gs
        in if null actions
              then evaluateGameState gs
              else goMax alpha actions
    | otherwise =
        let jointGhostMoves = legalJointGhostActions gs
        in if null jointGhostMoves
              then evaluateGameState gs
              else goMin beta jointGhostMoves
  where
    goMax a [] = a  -- Shouldn't happen with non-empty actions
    goMax a (act:rest) =
        let child = applyPacmanAction gs act
            val = minimax child (depth - 1) a beta False
            a' = max a val
        in if beta <= a'
              then a'
              else goMax a' rest

    goMin b [] = b  -- Shouldn't happen with non-empty actions
    goMin b (joint:rest) =
        let child = applyGhostJointActions gs joint
            val = minimax child (depth - 1) alpha b True
            b' = min b val
        in if b' <= alpha
              then b'
              else goMin b' rest

-- Heuristic evaluation of the game state.
-- Larger values are better for Pacman.
evaluateGameState :: GameState -> Int
evaluateGameState gs
    | isTerminal gs = terminalPenalty gs
    | otherwise =
        let baseScore     = score gs * 100
            pelletFactor  = if pelletsRemaining gs == 0 then 5000 else 0
            distToPelletW = case nearestPelletDistance gs of
                              Nothing -> 0
                              Just d  -> negate (10 * d)
            ghostProxPen  = negate (sum (map dangerFromGhost (ghostDistances gs)))
        in baseScore + pelletFactor + distToPelletW + ghostProxPen
  where
    terminalPenalty s =
        -- Strong penalty for death; mildly reward winning if pellets are zero.
        if pelletsRemaining s == 0 then 1000000 + score s * 100 else (-10000)

    dangerFromGhost d
        | d <= 0    = 5000   -- on same tile: immediate death threat
        | d == 1    = 500    -- adjacent: high danger
        | d == 2    = 150
        | d == 3    = 50
        | otherwise = 10

-- =========================
-- Successor generation
-- =========================

-- Legal Pacman actions (cannot move into a wall; must remain in bounds).
legalActionsPacman :: GameState -> [Action]
legalActionsPacman gs =
    filter (isLegalMove (grid gs) (pacmanPos gs)) allActions

-- Legal actions per ghost, returned as list aligned with ghostPositions.
legalActionsPerGhost :: GameState -> [[Action]]
legalActionsPerGhost gs =
    map (\pos -> filter (isLegalMove (grid gs) pos) allActions) (ghostPositions gs)

-- Cartesian product of ghost action lists to form joint actions.
legalJointGhostActions :: GameState -> [[Action]]
legalJointGhostActions gs =
    let perGhost = legalActionsPerGhost gs
    in cartesian perGhost

-- Apply a Pacman action to produce a successor GameState.
applyPacmanAction :: GameState -> Action -> GameState
applyPacmanAction gs act =
    let g        = grid gs
        fromPos  = pacmanPos gs
        toPos    = movePosition fromPos act
        -- If illegal, keep position (no-op)
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

-- Apply a joint list of ghost actions (one per ghost).
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

-- =========================
-- Utilities
-- =========================

movePosition :: Position -> Action -> Position
movePosition (x, y) act = case act of
    MoveUp    -> (x, y - 1)
    MoveDown  -> (x, y + 1)
    MoveLeft  -> (x - 1, y)
    MoveRight -> (x + 1, y)

inBounds :: Vector (Vector Cell) -> Position -> Bool
inBounds g (x, y) =
    y >= 0 && y < V.length g &&
    x >= 0 && not (V.null g) && x < V.length (g V.! y)

cellAt :: Vector (Vector Cell) -> Position -> Cell
cellAt g (x, y)
    | inBounds g (x, y) = (g V.! y) V.! x
    | otherwise         = Wall -- Treat out-of-bounds as walls

isLegalMove :: Vector (Vector Cell) -> Position -> Action -> Bool
isLegalMove g pos act =
    let np = movePosition pos act
    in inBounds g np && cellAt g np /= Wall

setCell :: Vector (Vector Cell) -> Position -> Cell -> Vector (Vector Cell)
setCell g (x, y) c =
    g V.// [(y, (g V.! y) V.// [(x, c)])]

-- Distances from Pacman to each ghost
ghostDistances :: GameState -> [Int]
ghostDistances gs =
    let g   = grid gs
        p   = pacmanPos gs
        big = maxBound `div` 4
    in [ maybe big id (bfsShortestDistance g p gp) | gp <- ghostPositions gs ]

-- Distance from Pacman to the nearest pellet, if any
nearestPelletDistance :: GameState -> Maybe Int
nearestPelletDistance gs =
    bfsNearestPelletDistance (grid gs) (pacmanPos gs)

-- BFS to find the shortest number of steps from start to any Pellet cell.
bfsNearestPelletDistance :: Vector (Vector Cell) -> Position -> Maybe Int
bfsNearestPelletDistance g start
  | not (inBounds g start) = Nothing
  | otherwise =
      let -- visited as a grid of Bool
          h = V.length g
          w = if h == 0 then 0 else V.length (g V.! 0)
          visited0 = V.replicate h (V.replicate w False)

          markVisited v (x, y) =
            v V.// [(y, (v V.! y) V.// [(x, True)])]

          isVisited v (x, y)
            | y < 0 || y >= V.length v = True
            | x < 0 || x >= V.length (v V.! y) = True
            | otherwise = (v V.! y) V.! x

          neighbors (x, y) = [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]

          -- simple list queue; fine for small/medium grids
          go [] _ = Nothing
          go ((p, d):qs) vis
            | cellAt g p == Pellet = Just d
            | otherwise =
                let nexts =
                      [ n
                      | n <- neighbors p
                      , inBounds g n
                      , cellAt g n /= Wall
                      , not (isVisited vis n)
                      ]
                    vis' = foldl markVisited vis nexts
                    qs'  = qs ++ [ (n, d + 1) | n <- nexts ]
                in go qs' vis'

          visited1 = markVisited visited0 start
      in go [(start, 0)] visited1

-- BFS shortest path distance between two positions (avoids walls).
bfsShortestDistance :: Vector (Vector Cell) -> Position -> Position -> Maybe Int
bfsShortestDistance g start target
  | not (inBounds g start)  = Nothing
  | not (inBounds g target) = Nothing
  | start == target         = Just 0
  | otherwise =
      let h = V.length g
          w = if h == 0 then 0 else V.length (g V.! 0)
          visited0 = V.replicate h (V.replicate w False)

          markVisited v (x, y) =
            v V.// [(y, (v V.! y) V.// [(x, True)])]

          isVisited v (x, y)
            | y < 0 || y >= V.length v = True
            | x < 0 || x >= V.length (v V.! y) = True
            | otherwise = (v V.! y) V.! x

          neighbors (x, y) = [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]

          go [] _ = Nothing
          go ((p, d):qs) vis
            | p == target = Just d
            | otherwise =
                let nexts =
                      [ n
                      | n <- neighbors p
                      , inBounds g n
                      , cellAt g n /= Wall
                      , not (isVisited vis n)
                      ]
                    vis' = foldl markVisited vis nexts
                    qs'  = qs ++ [ (n, d + 1) | n <- nexts ]
                in go qs' vis'

          visited1 = markVisited visited0 start
      in go [(start, 0)] visited1

-- Cartesian product for a list of lists
cartesian :: [[a]] -> [[a]]
cartesian [] = [[]]
cartesian (xs:xss) = [ x:ys | x <- xs, ys <- cartesian xss ]