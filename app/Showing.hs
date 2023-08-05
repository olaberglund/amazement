module Showing where

import qualified Algebra.Graph.Undirected as G
import Data.List (sortOn)
import Data.List.Split (chunksOf)
import System.Console.ANSI
import Types
import Utils (walls, width)
import Prelude hiding (Left)

showMaze :: MazeState -> String
showMaze ms = unlines $ appendFloor $ map showBlockRow $ mkBlocks $ sortOn y $ G.adjacencyList $ maze ms
  where
    showRow :: (a -> String) -> [a] -> String
    showRow showF = (<> wall) . concatMap showF

    bottom = showRow (showBlockBottom ms)

    top = showRow (showBlockTop ms)

    showBlockRow :: [(Location, [Location])] -> String
    showBlockRow r = top r <> "\n" <> bottom r

    mkBlocks = chunksOf width
    y = snd . fst

appendFloor :: [String] -> [String]
appendFloor = (++ [concat $ wall : replicate (blockSize * width) wall])

wall = "██"

space = "  "

playerSymbol = setSGRCode [SetColor Foreground Vivid Red] <> wall <> setSGRCode [Reset]

pathSymbol = setSGRCode [SetColor Foreground Vivid Green] <> wall <> setSGRCode [Reset]

blockSize = length space

showBlockTop :: MazeState -> (Location, [Location]) -> String
showBlockTop ms b@(loc, _) = showWalls (walls b)
  where
    showWalls [] = wall <> space
    showWalls [Up] = wall <> wall
    showWalls [Left] = wall <> space
    showWalls [Up, Left] = wall <> wall

showBlockBottom :: MazeState -> (Location, [Location]) -> String
showBlockBottom ms b@(loc, _) = showWalls (walls b)
  where
    showWalls [] = space <> walkCell ms loc
    showWalls [Up] = space <> walkCell ms loc
    showWalls [Left] = wall <> walkCell ms loc
    showWalls [Up, Left] = wall <> walkCell ms loc

walkCell :: MazeState -> Location -> String
walkCell ms loc
  | loc == player ms = playerSymbol
  | showPath ms && loc `elem` shortestPath ms = pathSymbol
  | otherwise = space
