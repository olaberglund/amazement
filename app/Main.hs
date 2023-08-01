module Main where

import Algebra.Graph.Undirected (Graph, edge, overlay, vertex, vertices)
import qualified Algebra.Graph.Undirected as G
import Control.Arrow (Arrow (first, second))
import Data.Function (on)
import Data.List (group, groupBy, iterate', sort, sortOn)
import Data.List.Split (chunksOf)
import Data.Ord (comparing)
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

data Line = Line
  { p :: Location,
    q :: Location
  }
  deriving (Eq, Show)

instance Ord Line where
  compare = comparing minY <> comparing slope <> comparing minX

-- --  Choose a vertex. Any vertex.
-- --  Choose a connected neighbor of the vertex and travel to it.
-- --  If the neighbor has not yet been visited,
-- --  add the traveled edge to the spanning tree.
--  Repeat step 2 until all vertexes have been visited.
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

slope :: Line -> D
slope (Line (x1, _) (x2, _)) = if x1 == x2 then DY else DX

minY :: Line -> Int
minY (Line (_, y1) (_, y2)) = min y1 y2

minX :: Line -> Int
minX (Line (x1, _) (x2, _)) = min x1 x2

haveVisitedAll :: MazeState -> Bool
haveVisitedAll m = S.size (visited m) == width * height

ls :: [Line]
ls = map (uncurry Line) $ G.edgeList m

slopeRows :: [Line] -> [[Line]]
slopeRows = groupBy ((==) `on` slope) . sort

showMaze :: Maze -> String
showMaze m = unlines $ map showRow $ slopeRows ls
  where
    showRow :: [Line] -> String
    showRow = map (\p -> if p then 'X' else ' ') . prefixMatches [0 .. width - 1] . map minX

    showLine :: Line -> String
    showLine l = case slope l of
      DX -> "═"
      DY -> "║"

-- https://www.w3.org/TR/xml-entity-names/025.html
-- Write this function using foldr

prefixMatches :: [Int] -> [Int] -> [Bool]
prefixMatches [] _ = []
prefixMatches l [] = replicate (length l) False
prefixMatches (x : xs) l@(y : ys) = if x == y then True : prefixMatches xs ys else False : prefixMatches xs l

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
