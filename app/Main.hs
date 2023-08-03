module Main where

import Algebra.Graph.Undirected (Graph, edge, overlay, vertex, vertices)
import qualified Algebra.Graph.Undirected as G
import Control.Arrow (Arrow (first, second))
import Control.Monad.IO.Class
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
import System.Console.ANSI (clearScreen)
import System.Console.Haskeline
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

width = 2 * height

height = 20

main :: IO ()
main = runInputT defaultSettings $ do
  n <- getInputLine "Seed: "
  liftIO clearScreen
  case n of
    Nothing -> return ()
    Just n -> gameLoop (0, 0) (generateMaze (read n))
  where
    gameLoop ploc m = do
      liftIO clearScreen
      outputStrLn $ showMaze ploc m
      key <- getInputChar ""
      case key of
        Nothing -> return ()
        Just 'a' -> gameLoop (moveLegally ploc Left m) m
        Just 's' -> gameLoop (moveLegally ploc Down m) m
        Just 'd' -> gameLoop (moveLegally ploc Right m) m
        Just 'w' -> gameLoop (moveLegally ploc Up m) m
        Just 'q' -> return ()
        Just _ -> gameLoop ploc m

    moveLegally :: Location -> Direction -> Maze -> Location
    moveLegally ploc dir m =
      let ns = S.toList (G.neighbours ploc m)
          ws = allWalls (ploc, ns)
          ploc' = move' dir ploc
       in if dir `elem` ws || not (withinBounds ploc')
            then ploc
            else ploc'

initialMaze :: Int -> MazeState
initialMaze n =
  let startLocation = (0, 0)
   in MazeState
        { maze = G.empty,
          location = startLocation,
          visited = S.singleton startLocation,
          gen = R.mkStdGen n
        }

generateMaze :: Int -> Maze
generateMaze = maze . iterateUntil haveVisitedAll aldousStep . initialMaze

haveVisitedAll :: MazeState -> Bool
haveVisitedAll = (width * height ==) . S.size . visited

wall = "██"

space = "  "

player = "◁▷"

blockSize = length space

dir :: Location -> Location -> Direction
dir (x, y) (x', y') =
  case (x' - x, y' - y) of
    (0, -1) -> Up
    (1, 0) -> Right
    (0, 1) -> Down
    (-1, 0) -> Left

showBlockTop :: Location -> (Location, [Location]) -> String
showBlockTop ploc b@(loc, _) = showWalls (walls b)
  where
    showWalls [] = wall <> space
    showWalls [Up] = wall <> wall
    showWalls [Left] = wall <> space
    showWalls [Up, Left] = wall <> wall

showBlockBottom :: Location -> (Location, [Location]) -> String
showBlockBottom ploc b@(loc, _) = showWalls (walls b)
  where
    showWalls [] = space <> spaceOrPlayer ploc loc
    showWalls [Up] = space <> spaceOrPlayer ploc loc
    showWalls [Left] = wall <> spaceOrPlayer ploc loc
    showWalls [Up, Left] = wall <> spaceOrPlayer ploc loc

spaceOrPlayer :: Location -> Location -> String
spaceOrPlayer loc ploc = if loc == ploc then player else space

walls :: (Location, [Location]) -> [Direction]
walls (loc, neighbors) = [Up, Left] \\ map (dir loc) neighbors

allWalls :: (Location, [Location]) -> [Direction]
allWalls (loc, neighbors) = [Up, Left, Down, Right] \\ map (dir loc) neighbors

showMaze :: Location -> Maze -> String
showMaze ploc = unlines . appendFloor . map (showBlockRow ploc) . mkBlocks . sortOn y . G.adjacencyList
  where
    showRow showF = (<> wall) . concatMap showF
    bottom = showRow . showBlockBottom
    top = showRow . showBlockTop
    showBlockRow ploc r = top ploc r <> "\n" <> bottom ploc r
    mkBlocks = chunksOf width
    y = snd . fst

appendFloor :: [String] -> [String]
appendFloor = (++ [concat $ wall : replicate (blockSize * width) wall])

iterateUntil :: (a -> Bool) -> (a -> a) -> a -> a
iterateUntil p f = head . filter p . iterate' f

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

withinBounds :: Location -> Bool
withinBounds (x, y) = x >= 0 && x < width && y >= 0 && y < height

randomMovement :: StdGen -> (Direction, StdGen)
randomMovement = first toEnum . R.randomR (0, 3)

data Direction = Up | Right | Down | Left deriving (Enum, Eq, Show)

move :: Direction -> Location -> Location
move Up = second succ
move Right = first succ
move Down = second pred
move Left = first pred

move' :: Direction -> Location -> Location
move' Down = second succ
move' Right = first succ
move' Up = second pred
move' Left = first pred
