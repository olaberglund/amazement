module Main where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Set as S
import Data.Text (unpack)
import Generators (aldousStep)
import Showing
import Solvers (backtracker, start)
import System.Console.ANSI (clearScreen)
import System.Console.Haskeline
import Types
import Utils
import Prelude hiding (Left, Right)

main :: IO ()
main = runInputT defaultSettings $ do
  liftIO clearScreen
  n <- getInputLine "Seed: "
  case n of
    Nothing -> return ()
    Just n -> gameLoop (generateMaze aldousStep (read n))
  where
    gameLoop :: MazeState -> InputT IO ()
    gameLoop ms = do
      liftIO clearScreen
      let m = maze ms
          ploc = player ms
      outputStrLn $ unpack $ showMaze ms
      key <- getInputChar ""
      case key of
        Just 'w' -> gameLoop (ms {player = moveLegally ploc Up m})
        Just 's' -> gameLoop (ms {player = moveLegally ploc Down m})
        Just 'd' -> gameLoop (ms {player = moveLegally ploc Right m})
        Just 'a' -> gameLoop (ms {player = moveLegally ploc Left m})
        -- cheatings
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
