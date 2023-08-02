module Main where

import Algebra.Graph.Undirected (Graph, edge, overlay, vertex, vertices)
import qualified Algebra.Graph.Undirected as G
import Control.Arrow (Arrow (first, second))
import Data.Function (on)
import Data.List
  ( group,
    groupBy,
    iterate',
    sort,
    sortBy,
    sortOn,
    (\\),
  )
import Data.List.Split (chunksOf)
import qualified Data.Ord as Ord (Down (Down))
import Data.Sequence (iterateN, mapWithIndex)
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

-- --  Choose a vertex. Any vertex.

-- --  Choose a vertex. Any vertex.
-- --  Choose a connected neighbor of the vertex and travel to it.
-- --  If the neighbor has not yet been visited,
-- --  add the traveled edge to the spanning tree.
--  Repeat step 2 until all vertexes have been visited.
width = 3 * height

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
main = print m

m = maze $ iterateUntil haveVisitedAll aldousStep initialMaze

haveVisitedAll :: MazeState -> Bool
haveVisitedAll m = S.size (visited m) == width * height

-- https://www.w3.org/TR/xml-entity-names/025.html

vertical = "║"

horizontal = "═"

tlcorner = "╔"

nl = "\n"

data Direction = N | E | S | W deriving (Show, Eq, Ord)

dir :: Location -> Location -> Direction
dir (x, y) (x', y') =
  case (x' - x, y' - y) of
    (0, -1) -> N
    (1, 0) -> E
    (0, 1) -> S
    (-1, 0) -> W

showWalls1 :: [Direction] -> String
showWalls1 [] = "  "
showWalls1 [N] = horizontal <> horizontal
showWalls1 [W] = vertical <> " "
showWalls1 [N, W] = tlcorner <> horizontal

showWalls2 :: [Direction] -> String
showWalls2 [] = "  "
showWalls2 [N] = "  "
showWalls2 [W] = vertical <> " "
showWalls2 [N, W] = vertical <> " "

-- showCell :: (Location, [Location]) -> String
showCell1 :: (Location, [Location]) -> String
showCell1 (loc, neighbors) = showWalls1 walls
  where
    walls = [N, W] \\ map (dir loc) neighbors

showCell2 :: (Location, [Location]) -> String
showCell2 (loc, neighbors) = showWalls2 walls
  where
    walls = [N, W] \\ map (dir loc) neighbors

showMaze :: Maze -> String
showMaze = unlines . (++ [concat $ replicate (2 * width) horizontal]) . map showRow . chunksOf width . sortOn (snd . fst) . G.adjacencyList
  where
    showRow r = concatMap showCell1 r <> vertical <> nl <> concatMap showCell2 r <> vertical

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
  let (dir, gen') = randomMovement gen
      loc' = move dir loc
   in if withinBounds loc' then (loc', gen') else findLegalMove gen' loc
  where
    withinBounds (x, y) = x >= 0 && x < width && y >= 0 && y < height

randomMovement :: StdGen -> (Movement, StdGen)
randomMovement = first toEnum . R.randomR (0 :: Int, fromEnum (maxBound :: Movement))

data Movement = Up | Right | Down | Left deriving (Enum, Bounded, Show)

move :: Movement -> Location -> Location
move Up = second succ
move Right = first succ
move Down = second pred
move Left = first pred
