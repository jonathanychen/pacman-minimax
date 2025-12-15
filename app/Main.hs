module Main where

import Types
import Game
import Plan
import System.Environment (getArgs)
import System.CPUTime
import Data.Time.Clock (getCurrentTime, UTCTime, diffUTCTime)
import Text.Printf
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified Control.Concurrent
import Control.DeepSeq (deepseq)
    
data RunMode = Minimax
    deriving (Eq, Show)

data Config = Config
    { parallel :: Bool
    , visualize :: Bool
    , gridFile :: FilePath
    , useGloss :: Bool
    } deriving (Show)

defaultConfig :: Config
defaultConfig = Config
    { parallel = False
    , visualize = False
    , gridFile = "grids/large.txt"
    , useGloss = False
    }

main :: IO ()
main = do
    args <- getArgs
    config <- parseArgs args defaultConfig

    printf "Pac-Man Minimax\n"
    printf "==================\n"
    printf "Configuration:\n"
    printf "  Parallel: %s\n" (show $ parallel config)
    printf "  Visualization: %s\n" (show $ visualize config)
    printf "  Use Gloss: %s\n" (show $ useGloss config)
    printf "  Grid file: %s\n" (gridFile config)

    -- Load game state
    initialState <- loadGrid (gridFile config)

    startTime <- getCurrentTime
    let path = planPacmanPath initialState 9 200 (parallel config)
    endTime <- path `deepseq` getCurrentTime

    let timeDiff = diffUTCTime endTime startTime
    printf "Planning completed in %.3f seconds.\n" (realToFrac timeDiff :: Double)

    visualizeGameStates path 120

    printf "Planning completed in %.3f seconds.\n" (realToFrac timeDiff :: Double)

-- Parse command line arguments
parseArgs :: [String] -> Config -> IO Config
parseArgs [] cfg = return cfg
parseArgs ("--par":rest) cfg = parseArgs rest cfg { parallel = True }
parseArgs ("--seq":rest) cfg = parseArgs rest cfg { parallel = False }
parseArgs ("--visualize":rest) cfg = parseArgs rest cfg { visualize = True }
parseArgs ("-v":rest) cfg = parseArgs rest cfg { visualize = True }
parseArgs ("--gloss":rest) cfg = parseArgs rest cfg { useGloss = True, visualize = True }
parseArgs ("-g":rest) cfg = parseArgs rest cfg { useGloss = True, visualize = True }
parseArgs ("--grid":file:rest) cfg = parseArgs rest cfg { gridFile = file }
parseArgs (unknown:rest) cfg = do
    printf "Warning: Unknown argument '%s'\n" unknown
    parseArgs rest cfg
