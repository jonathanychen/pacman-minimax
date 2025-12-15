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
        if pelletsRemaining s == 0 then 10000 + score s * 100 else (-10000)

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

manhattan :: Position -> Position -> Int
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

-- Distances from Pacman to each ghost
ghostDistances :: GameState -> [Int]
ghostDistances gs = map (manhattan (pacmanPos gs)) (ghostPositions gs)

-- Distance from Pacman to the nearest pellet, if any
nearestPelletDistance :: GameState -> Maybe Int
nearestPelletDistance gs =
    let g = grid gs
        p = pacmanPos gs
        ys = [0 .. V.length g - 1]
        xsFor y = [0 .. V.length (g V.! y) - 1]
        pelletPositions =
            [ (x, y)
            | y <- ys
            , x <- xsFor y
            , cellAt g (x, y) == Pellet
            ]
    in case pelletPositions of
         [] -> Nothing
         ps -> Just $ minimum (map (manhattan p) ps)

-- Cartesian product for a list of lists
cartesian :: [[a]] -> [[a]]
cartesian [] = [[]]
cartesian (xs:xss) = [ x:ys | x <- xs, ys <- cartesian xss ]