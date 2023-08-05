module Types where

import Algebra.Graph.Undirected (Graph)
import Data.Set (Set)
import System.Random (StdGen)

type Location = (Int, Int)

data MazeState = MazeState
  { maze :: Maze,
    location :: Location,
    visited :: Set Location,
    gen :: StdGen
  }

data Direction = Up | Right | Down | Left deriving (Enum, Eq, Show)

type Maze = Graph Location

type Stack = (Location, [Location])
