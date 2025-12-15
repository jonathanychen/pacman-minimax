module Plan
  ( planPacmanPath
  , bestPacmanAction
  ) where

import Types
import Minimax
import MinimaxParallel
import qualified Data.Vector as V
import Data.List (maximumBy)
import Data.Ord (comparing)

-- Choose the best Pacman action at a state using minimax.
bestPacmanAction :: GameState -> Int -> Action
bestPacmanAction gs depth =
  let g        = grid gs
      ppos     = pacmanPos gs
      actions  = legalActionsPacman gs
      scored   = [ (a, minimax (applyPacmanAction gs a) (depth - 1) minBound maxBound False) | a <- actions ]
      bestVal  = maximum (map snd scored)
      candidates = [ a | (a, v) <- scored, v == bestVal ]
      eatsPellet a =
        let np = movePosition ppos a
        in cellAt g np == Pellet
  in case candidates of
       [] -> head actions
       _  -> maximumBy (comparing (\a -> if eatsPellet a then 1 :: Int else 0)) candidates

-- Generate the sequence of GameStates for Pacman’s path until terminal.
-- depth: minimax search depth per Pacman decision
-- maxSteps: safety cap to avoid infinite loops on boards with no win/loss
planPacmanPath :: GameState -> Int -> Int -> Bool -> [GameState]
planPacmanPath start depth maxSteps useParallel = go start 0
  where
    go gs n
      | isTerminal gs || n >= maxSteps = [gs]
      | useParallel =
          let a    = bestPacmanActionPar gs depth 2 -- use 2 levels of parallelism
              pac' = applyPacmanAction gs a
              gh'  = stepGhostsGreedy pac'
          in gs : go gh' (n + 1)
      | otherwise =
          let a    = bestPacmanAction gs depth
              pac' = applyPacmanAction gs a
              gh'  = stepGhostsGreedy pac'
          in gs : go gh' (n + 1)

-- =========== Helpers (reuse same conventions as Minimax.hs) ===========

maximumByCmp :: Ord b => (a -> b) -> [a] -> a
maximumByCmp f (x:xs) = foldl (\best y -> if f y > f best then y else best) x xs
maximumByCmp _ []     = error "maximumByCmp: empty list"

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

-- Legal Pacman actions
legalActionsPacman :: GameState -> [Action]
legalActionsPacman gs = filter (isLegalMove (grid gs) (pacmanPos gs)) allActions

-- Apply Pacman action (same logic as in Minimax.hs)
applyPacmanAction :: GameState -> Action -> GameState
applyPacmanAction gs act =
  let g        = grid gs
      fromPos  = pacmanPos gs
      toPos    = movePosition fromPos act
      targetPos = if isLegalMove g fromPos act then toPos else fromPos
      cell      = cellAt g targetPos
      atePellet = cell == Pellet
      newGrid   = if atePellet then g V.// [(snd targetPos, (g V.! snd targetPos) V.// [(fst targetPos, Empty)])] else g
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

stepGhostsGreedy :: GameState -> GameState
stepGhostsGreedy gs
  | isTerminal gs = gs
  | otherwise =
      let g        = grid gs
          ppos     = pacmanPos gs
          distGrid = bfsDistances g ppos
          choose pos =
            let acts  = filter (isLegalMove g pos) allActions
                nexts = [ (movePosition pos a, a) | a <- acts ]
                -- Look up BFS distance for each candidate next position; prefer smaller distances.
                -- If unreachable (Nothing), treat as large number to avoid.
                score np = maybe big id (distanceAt distGrid np)
                big      = maxBound `div` 4
                best     = minimumByCmp (\(np, _) -> score np) nexts
            in fst best
          movedGhosts = map choose (ghostPositions gs)
          collided    = ppos `elem` movedGhosts
          terminal    = collided || pelletsRemaining gs <= 0
          deathP      = if collided then Just ppos else deathPos gs
      in gs { ghostPositions = movedGhosts
            , isTerminal     = terminal
            , deathPos       = deathP
            }

-- BFS distances from a start position to all reachable cells (avoids walls).
bfsDistances :: V.Vector (V.Vector Cell) -> Position -> V.Vector (V.Vector (Maybe Int))
bfsDistances g start =
  let h = V.length g
      w = if h == 0 then 0 else V.length (g V.! 0)
      initGrid = V.replicate h (V.replicate w Nothing)
      enqueue q x = q ++ [x]  -- simple list queue; for bigger boards use Seq
      neighbors (x, y) =
        [ (x+1,y), (x-1,y), (x,y+1), (x,y-1) ]
        |> filter (inBounds g)
        |> filter (\p -> cellAt g p /= Wall)
      go q dist dgrid =
        case q of
          [] -> dgrid
          (p:qs) ->
            let curD = maybe 0 id (distanceAt dgrid p)
                ns   = neighbors p
                (qs', dgrid') =
                  foldl
                    (\(accQ, accG) n ->
                       case distanceAt accG n of
                         Just _  -> (accQ, accG)      -- already visited
                         Nothing ->
                           let accG' = writeDistance accG n (curD + 1)
                           in (enqueue accQ n, accG'))
                    (qs, dgrid)
                    ns
            in go qs' dist dgrid'
      dgrid0 = writeDistance initGrid start 0
  in go [start] 0 dgrid0

-- Read distance at a position from the distance grid.
distanceAt :: V.Vector (V.Vector (Maybe Int)) -> Position -> Maybe Int
distanceAt dg (x, y)
  | y < 0 || y >= V.length dg = Nothing
  | x < 0 || x >= V.length (dg V.! y) = Nothing
  | otherwise = (dg V.! y) V.! x

-- Write a distance at a position.
writeDistance :: V.Vector (V.Vector (Maybe Int)) -> Position -> Int -> V.Vector (V.Vector (Maybe Int))
writeDistance dg (x, y) d =
  dg V.// [(y, (dg V.! y) V.// [(x, Just d)])]

-- Simple pipe operator for readability.
(|>) :: a -> (a -> b) -> b
(|>) x f = f x

minimumByCmp :: Ord b => (a -> b) -> [a] -> a
minimumByCmp f (x:xs) = foldl (\best y -> if f y < f best then y else best) x xs
minimumByCmp _ []     = error "minimumByCmp: empty list"

manhattan :: Position -> Position -> Int
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)