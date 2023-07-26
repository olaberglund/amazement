module Main where

import Algebra.Graph.Undirected (Graph, edge, overlay, vertex, vertices)
import qualified Algebra.Graph.Undirected as G
import Control.Arrow (Arrow (first, second))
import Data.List (iterate')
import Data.Sequence (iterateN)
import Data.Set (Set, insert, notMember)
import qualified Data.Set as S
import Debug.Trace (trace)
import System.Random (StdGen)
import qualified System.Random as R
import Prelude hiding (Left, Right)

type Location = (Int, Int)

data MazeState = MazeState
  { maze :: Maze,
    location :: Location,
    visited :: Set Location,
    gen :: StdGen
  }

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
width = 10

height = 10

initialMaze :: MazeState
initialMaze =
  let startLocation = (0, 0)
   in MazeState
        { maze = G.empty,
          location = startLocation,
          visited = S.singleton startLocation,
          gen = R.mkStdGen 10
        }

main :: IO ()
main = do
  let m = iterateUntil haveVisitedAll aldousStep initialMaze
  print (maze m)
  where
    haveVisitedAll :: MazeState -> Bool
    haveVisitedAll m = S.size (visited m) == width * height

-- https://www.w3.org/TR/xml-entity-names/025.html

iterateUntil :: (a -> Bool) -> (a -> a) -> a -> a
iterateUntil p f = head . filter p . iterate' f

aldousStep :: MazeState -> MazeState
aldousStep m =
  if notMember loc' (visited m)
    then
      m
        { gen = gen',
          maze = overlay (edge (location m) loc') (maze m),
          location = loc',
          visited = insert loc' (visited m)
        }
    else m {gen = gen', location = loc'}
  where
    (loc', gen') = findLegalMove (gen m) (location m)

findLegalMove :: StdGen -> Location -> (Location, StdGen)
findLegalMove gen loc =
  let (dir, gen') = randomDirection gen
      loc' = move dir loc
   in if withinBounds loc' then (loc', gen') else findLegalMove gen' loc
  where
    withinBounds (x, y) = x >= 0 && x < width && y >= 0 && y < height

randomDirection :: StdGen -> (Direction, StdGen)
randomDirection = first toEnum . R.randomR (0 :: Int, fromEnum (maxBound :: Direction))

data Direction = Up | Right | Down | Left deriving (Enum, Bounded, Show)

move :: Direction -> Location -> Location
move Up = second succ
move Right = first succ
move Down = second pred
move Left = first pred
