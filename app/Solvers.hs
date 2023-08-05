module Solvers where

import qualified Algebra.Graph.Undirected as G
import Data.List ((\\))
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Types
import Utils (dir, height, move, path, push, stack, width)

start = stack (0, 0)

goal = (width - 1, height - 1)

-- the recursive backtracker performs a drunkard's walk from some starting point,
-- and pushes each node it visits onto a stack.
-- And when it reaches a dead-end, instead of doing an O(n) search,
-- it simply pops nodes off its stack until it finds one that it can continue walking from.
backtracker :: Maze -> Stack -> [Location]
backtracker m s@(l, ls)
  | l == goal = path s
  | otherwise = locations l m \\ take 1 ls >>= backtracker m . push s

locations :: Location -> Maze -> [Location]
locations = (S.toList .) . G.neighbours
