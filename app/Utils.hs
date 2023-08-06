{-# LANGUAGE TupleSections #-}

module Utils where

import qualified Algebra.Graph.Undirected as G
import Control.Arrow (first, second)
import Data.List (iterate', (\\))
import qualified Data.Set as S
import qualified System.Random as R
import Types
import Prelude hiding (Left, Right)

width = height

height = 30

withinBounds :: Location -> Bool
withinBounds (x, y) = x >= 0 && x < width && y >= 0 && y < height

move :: Direction -> Location -> Location
move Down = second succ
move Right = first succ
move Up = second pred
move Left = first pred

moveLegally :: Location -> Direction -> Maze -> Location
moveLegally ploc dir m =
  let ns = S.toList (G.neighbours ploc m)
      ws = allWalls (ploc, ns)
      ploc' = move dir ploc
   in if dir `elem` ws || not (withinBounds ploc')
        then ploc
        else ploc'

walls :: (Location, [Location]) -> [Direction]
walls (loc, neighbors) = [Up, Left] \\ map (dir loc) neighbors

allWalls :: (Location, [Location]) -> [Direction]
allWalls (loc, neighbors) = [Up, Left, Down, Right] \\ map (dir loc) neighbors

initialMaze :: Int -> MazeState
initialMaze n =
  let startLocation = (0, 0)
   in MazeState
        { maze = G.empty,
          visited = S.singleton startLocation,
          gen = R.mkStdGen n,
          showPath = False,
          player = startLocation,
          shortestPath = S.empty
        }

dir :: Location -> Location -> Direction
dir (x, y) (x', y') =
  case (x' - x, y' - y) of
    (0, -1) -> Up
    (1, 0) -> Right
    (0, 1) -> Down
    (-1, 0) -> Left

haveVisitedAll :: MazeState -> Bool
haveVisitedAll = (width * height ==) . S.size . visited

iterateUntil :: (a -> Bool) -> (a -> a) -> a -> a
iterateUntil p f = head . filter p . iterate' f

push :: Stack -> Location -> Stack
push (loc, locs) loc' = (loc', loc : locs)

path :: Stack -> [Location]
path = uncurry (:)

stack :: Location -> Stack
stack = (,[])
