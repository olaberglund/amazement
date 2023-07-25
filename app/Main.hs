module Main where

import Algebra.Graph.Undirected (Graph, edge, overlay, vertex, vertices)
import qualified Algebra.Graph.Undirected as G
import Control.Arrow (Arrow (first, second))
import Data.Sequence (iterateN)
import Data.Set (Set, insert, notMember)
import qualified Data.Set as S
import Debug.Trace (trace)
import System.Random (StdGen)
import qualified System.Random as R
import Prelude hiding (Left, Right)

main :: IO ()
main = putStrLn "Hello, Haskell!"

type Location = (Int, Int)

type Maze = Graph Location

-- Need to keep track of:
-- 1. Current location
-- 2. Visited locations
-- 3. Maze

-- --  Choose a vertex. Any vertex.
-- --  Choose a connected neighbor of the vertex and travel to it.
-- --  If the neighbor has not yet been visited,
-- --  add the traveled edge to the spanning tree.
-- --  Repeat step 2 until all vertexes have been visited.

aldousStep :: (StdGen, Maze, Location, Set Location) -> (StdGen, Maze, Location, Set Location)
aldousStep (gen, maze, location, visited) =
  if notMember location' visited
    then (gen', overlay (edge location location') maze, location', insert location' visited)
    else (gen', maze, location', visited)
  where
    (dir, gen') = randomDirection gen
    location' = move dir location

randomDirection :: StdGen -> (Direction, StdGen)
randomDirection = first toEnum . R.randomR (0 :: Int, fromEnum (maxBound :: Direction))

data Direction = Up | Right | Down | Left deriving (Enum, Bounded, Show)

move :: Direction -> Location -> Location
move Up = second succ
move Right = first succ
move Down = second pred
move Left = first pred

-- generateRandomMaze :: StdGen -> (Int, Int) -> M.Map Location CellBoundaries
-- generateRandomMaze gen (numRows, numColumns) = undefined
