module Generators where

import Algebra.Graph.Undirected (Graph, edge, overlay, vertex, vertices)
import qualified Algebra.Graph.Undirected as G
import Control.Arrow (first)
import Data.Set (Set, insert, notMember)
import qualified Data.Set as S
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

randomMovement :: StdGen -> (Direction, StdGen)
randomMovement = first toEnum . R.randomR (0, 3)

initialMaze :: Int -> MazeState
initialMaze n =
  let startLocation = (0, 0)
   in MazeState
        { maze = G.empty,
          location = startLocation,
          visited = S.singleton startLocation,
          gen = R.mkStdGen n
        }
