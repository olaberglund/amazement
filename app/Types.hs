module Types where

import Algebra.Graph.Undirected (Graph)
import Data.Set (Set)
import System.Random (StdGen)

type Location = (Int, Int)

data MazeState = MazeState
  { maze :: Maze,
    visited :: Set Location,
    gen :: StdGen,
    showPath :: Bool,
    player :: Location,
    shortestPath :: Set Location
  }

data Direction = Up | Right | Down | Left deriving (Enum, Eq, Show)

type Maze = Graph Location

type Stack = (Location, [Location])
