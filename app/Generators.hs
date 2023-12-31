module Generators where

import Algebra.Graph.Undirected (edge, overlay)
import qualified Algebra.Graph.Undirected as G
import Control.Arrow (first)
import Data.Set (insert, notMember)
import System.Random (StdGen)
import qualified System.Random as R
import Types
import Utils (move, withinBounds)

-- --  Choose a vertex. Any vertex.
-- --  Choose a connected neighbor of the vertex and travel to it.
-- --  If the neighbor has not yet been visited,
-- --  add the traveled edge to the spanning tree.
--  Repeat step 2 until all vertexes have been visited.

aldousStep :: MazeState -> MazeState
aldousStep m =
  if notMember loc' (visited m)
    then
      m
        { gen = gen',
          maze = overlay (edge (player m) loc') (maze m),
          player = loc',
          visited = insert loc' (visited m)
        }
    else m {gen = gen', player = loc'}
  where
    (loc', gen') = findLegalMove (gen m) (player m)

findLegalMove :: StdGen -> Location -> (Location, StdGen)
findLegalMove gen loc =
  let (dir, gen') = randomMovement gen
      loc' = move dir loc
   in if withinBounds loc' then (loc', gen') else findLegalMove gen' loc

randomMovement :: StdGen -> (Direction, StdGen)
randomMovement = first toEnum . R.randomR (0, 3)
