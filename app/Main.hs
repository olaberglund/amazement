module Main where

import Algebra.Graph.Undirected (Graph, edge, overlay, vertex, vertices)
import qualified Algebra.Graph.Undirected as G
import Control.Arrow (Arrow (first, second))
import Data.Function (on)
import Data.List (groupBy, iterate', sort, sortOn)
import Data.List.Split (chunksOf)
import Data.Ord (comparing)
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

data MazeEdge = MazeEdge
  { slope :: D,
    level :: Int
  }
  deriving (Show, Eq)

instance Ord MazeEdge where
  (MazeEdge DX l1) `compare` MazeEdge DX l2 = l1 `compare` l2
  (MazeEdge DY l1) `compare` MazeEdge DY l2 = l1 `compare` l2
  (MazeEdge DX l1) `compare` MazeEdge DY l2 = l2 `compare` l1
  (MazeEdge DY l1) `compare` MazeEdge DX l2 = l1 `compare` l2

-- sort first by axis value, then direction

-- Need to keep track of:
-- 1. Current location
-- 2. Visited locations
-- 3. Maze

-- --  Choose a vertex. Any vertex.
-- --  Choose a connected neighbor of the vertex and travel to it.
-- --  If the neighbor has not yet been visited,
-- --  add the traveled edge to the spanning tree.
-- --  Repeat step 2 until all vertexes have been visited.
width = 4

height = width

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
main = print m

m = maze $ iterateUntil haveVisitedAll aldousStep initialMaze

data D = DX | DY deriving (Eq, Ord, Show)

d :: (Location, Location) -> D
d ((x1, _), (x2, _)) = if x1 == x2 then DY else DX

axisValue :: (Location, Location) -> Int
axisValue dl = case d dl of
  DX -> snd (fst dl)
  DY -> fst (fst dl)

haveVisitedAll :: MazeState -> Bool
haveVisitedAll m = S.size (visited m) == width * height

ls :: [MazeEdge]
ls = mkMazeEdge <$> G.edgeList m

mkMazeEdge :: (Location, Location) -> MazeEdge
mkMazeEdge = MazeEdge <$> d <*> axisValue

groupByAxisValue :: [MazeEdge] -> [[MazeEdge]]
groupByAxisValue = undefined

-- goal: group edges by axis value

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
