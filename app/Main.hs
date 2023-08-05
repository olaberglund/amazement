module Main where

import Control.Arrow (Arrow (first, second))
import Control.Monad.IO.Class (liftIO)
import Data.List (iterate')
import Generators (aldousStep)
import Showing
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
    Just n -> gameLoop (0, 0) (generateMaze aldousStep (read n))
  where
    gameLoop ploc m = do
      liftIO clearScreen
      outputStrLn $ showMaze ploc m
      key <- getInputChar ""
      case key of
        Just 'w' -> gameLoop (moveLegally ploc Up m) m
        Just 's' -> gameLoop (moveLegally ploc Down m) m
        Just 'd' -> gameLoop (moveLegally ploc Right m) m
        Just 'a' -> gameLoop (moveLegally ploc Left m) m
        -- cheating
        Just 'k' -> gameLoop (move Up ploc) m
        Just 'j' -> gameLoop (move Down ploc) m
        Just 'l' -> gameLoop (move Right ploc) m
        Just 'h' -> gameLoop (move Left ploc) m
        Just 'r' -> gameLoop (0, 0) m
        Just 'g' -> gameLoop (width - 1, height - 1) m
        -- meta
        Just 'n' -> liftIO clearScreen >> liftIO main
        Just 'q' -> return ()
        _ -> gameLoop ploc m
