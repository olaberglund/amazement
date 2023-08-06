module Main where

import Control.Monad (replicateM_)
import qualified Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time
import Generators (aldousStep)
import Showing
import Solvers (backtracker, start)
import System.Console.ANSI (clearScreen)
import System.Console.Haskeline
  ( InputT,
    defaultSettings,
    getInputChar,
    getInputLine,
    outputStrLn,
    runInputT,
  )
import System.IO
import Types
import Utils
import Prelude hiding (Left, Right)

main :: IO ()
main = runInputT defaultSettings $ do
  liftIO $ hSetBuffering stdin LineBuffering >> hSetEcho stdin True
  n <- getInputLine "New seed: "
  liftIO clearScreen
  liftIO $ hSetBuffering stdin NoBuffering >> hSetEcho stdin False
  startTime <- liftIO getCurrentTime
  case n of
    Nothing -> return ()
    Just n -> gameLoop (generateMaze aldousStep (read n))
  endTime <- liftIO getCurrentTime
  outputStrLn $ "Time: " ++ show (diffUTCTime endTime startTime)
  outputStrLn $ "Seed: " ++ fromMaybe "None" n
  liftIO main
  where
    gameLoop :: MazeState -> InputT IO ()
    gameLoop ms = do
      let m = maze ms
          ploc = player ms
          mazeHeight = length $ T.lines $ showMaze ms
      -- replicateM_ (58 - mazeHeight) (outputStrLn "")
      liftIO clearScreen
      outputStrLn $ T.unpack $ showMaze ms
      if ploc == (width - 1, height - 1)
        then do
          liftIO clearScreen
          outputStrLn $ T.unpack $ showMaze ms {showPath = True}
          return ()
        else do
          key <- getInputChar ""
          case key of
            Just 'w' -> gameLoop (ms {player = moveLegally ploc Up m})
            Just 's' -> gameLoop (ms {player = moveLegally ploc Down m})
            Just 'd' -> gameLoop (ms {player = moveLegally ploc Right m})
            Just 'a' -> gameLoop (ms {player = moveLegally ploc Left m})
            -- cheating
            Just 'k' -> gameLoop (ms {player = move Up ploc})
            Just 'j' -> gameLoop (ms {player = move Down ploc})
            Just 'l' -> gameLoop (ms {player = move Right ploc})
            Just 'h' -> gameLoop (ms {player = move Left ploc})
            Just 'r' -> gameLoop (ms {player = (0, 0)})
            Just 'g' -> gameLoop (ms {player = (width - 1, height - 1)})
            Just 'p' -> gameLoop (ms {showPath = not (showPath ms)})
            -- meta
            Just 'n' -> liftIO clearScreen >> liftIO main
            Just 'q' -> return ()
            _ -> gameLoop ms

generateMaze :: (MazeState -> MazeState) -> Int -> MazeState
generateMaze stepper n =
  let m = iterateUntil haveVisitedAll stepper (initialMaze n)
   in m {player = (0, 0), shortestPath = S.fromList $ backtracker (maze m) start}
