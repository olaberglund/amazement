module Main where

import Algebra.Graph.Undirected (Graph, connects, empty, vertex, vertices)
import Control.Arrow (Arrow (first))
import qualified Data.Map as M
import Data.Sequence (iterateN)
import System.Random (StdGen)
import qualified System.Random as R
import Prelude hiding (Left, Right)

main :: IO ()
main = putStrLn "Hello, Haskell!"

type Maze = Graph BoundaryType

data BoundaryType = Wall | WorldBoundary | Path deriving (Eq, Show)

-- data CellBoundaries = CellBoundaries
--   { upBoundary :: BoundaryType,
--     rightBoundary :: BoundaryType,
--     downBoundary :: BoundaryType,
--     leftBoundary :: BoundaryType
--   }

-- instance Show CellBoundaries where
--   show (CellBoundaries up right down left) =
--     show up
--       ++ show right
--       ++ show down
--       ++ show left

uncarvedMaze :: Int -> Int -> Maze
uncarvedMaze width height = undefined
  where
    rows :: [Graph BoundaryType]
    rows = replicate height (connects $ replicate width (vertex Wall))

    maze = foldr1 (zipWith connects) rows

-- --  Choose a vertex. Any vertex.
-- --  Choose a connected neighbor of the vertex and travel to it.
-- --  If the neighbor has not yet been visited,
-- --  add the traveled edge to the spanning tree.
-- --  Repeat step 2 until all vertexes have been visited.

-- aldousBroderStep :: StdGen -> (Location, Maze) -> (Location, Maze)
-- aldousBroderStep stdgen (loc, m) = case M.lookup loc' m of
--   Just cbs -> case btype dir cbs of
--     Wall -> (loc', M.insert loc' (CellBoundaries Wall Wall Wall Wall) m)
--     WorldBoundary -> (loc', M.insert loc' (CellBoundaries Wall Wall Wall Wall) m)
--     AdjacentCell _ -> (loc', M.insert loc' (CellBoundaries Wall Wall Wall Wall) m)
--   Nothing -> (loc, m)
--   where
--     (dir, stdgen') = randomDirection stdgen
--     loc' = move dir loc

-- randomDirection :: StdGen -> (Direction, StdGen)
-- randomDirection = first toEnum . R.randomR (0 :: Int, fromEnum (maxBound :: Direction))

-- data Direction = Up | Right | Down | Left deriving (Enum, Bounded)

-- opposite :: Direction -> Direction
-- opposite Up = Down
-- opposite Right = Left
-- opposite Down = Up
-- opposite Left = Right

-- move :: Direction -> Location -> Location
-- move Up (x, y) = (x, y - 1)
-- move Right (x, y) = (x + 1, y)
-- move Down (x, y) = (x, y + 1)
-- move Left (x, y) = (x - 1, y)

-- btype :: Direction -> CellBoundaries -> BoundaryType
-- btype Up = upBoundary
-- btype Right = rightBoundary
-- btype Down = downBoundary
-- btype Left = leftBoundary

-- topWall,
--   rightWall,
--   bottomWall,
--   leftWall ::
--     CellBoundaries
-- topWall = walls {upBoundary = WorldBoundary}
-- rightWall = walls {rightBoundary = WorldBoundary}
-- bottomWall = walls {downBoundary = WorldBoundary}
-- leftWall = walls {leftBoundary = WorldBoundary}

-- walls :: CellBoundaries
-- walls = CellBoundaries Wall Wall Wall Wall

-- generateRandomMaze :: StdGen -> (Int, Int) -> M.Map Location CellBoundaries
-- generateRandomMaze gen (numRows, numColumns) = undefined
