module Showing where

import qualified Algebra.Graph.Undirected as G
import Data.List (sortOn)
import Data.List.Split (chunksOf)
import System.Console.ANSI
import Types
import Utils (walls, width)
import Prelude hiding (Left)

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

wall = "██"

space = "  "

player = setSGRCode [SetColor Foreground Vivid Red] <> wall <> setSGRCode [Reset]

blockSize = length space

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
