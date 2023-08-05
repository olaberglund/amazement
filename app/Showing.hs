module Showing where

import qualified Algebra.Graph.Undirected as G
import Data.List (sortOn)
import Data.List.Split (chunksOf)
import Data.Set (member)
import Data.Text (Text)
import Data.Text as T hiding (chunksOf, map)
import System.Console.ANSI
import Types
import Utils (walls, width)
import Prelude hiding (Left, elem, length, unlines)

showMaze :: MazeState -> Text
showMaze ms = unlines $ appendFloor $ map showBlockRow $ mkBlocks $ sortOn location $ G.adjacencyList $ maze ms
  where
    showRow :: (a -> Text) -> [a] -> Text
    showRow showF = (<> pwall) . T.concat . map showF

    bottom, top :: [(Location, [Location])] -> Text
    bottom = showRow (showBlockBottom ms)
    top = showRow (showBlockTop ms)

    showBlockRow :: [(Location, [Location])] -> Text
    showBlockRow r = top r <> pack "\n" <> bottom r

    mkBlocks = chunksOf width
    location = snd . fst

appendFloor :: [Text] -> [Text]
appendFloor = (++ [pwall <> T.replicate (blockSize * width) pwall])

wall = "██"

pwall = pack wall

space = pack "  "

playerSymbol = pack $ setSGRCode [SetColor Foreground Vivid Red] <> wall <> setSGRCode [Reset]

pathSymbol = pack $ setSGRCode [SetColor Foreground Vivid Green] <> wall <> setSGRCode [Reset]

blockSize = length space

showBlockTop :: MazeState -> (Location, [Location]) -> Text
showBlockTop ms b@(loc, _) = showWalls (walls b)
  where
    showWalls [] = pwall <> space
    showWalls [Up] = pwall <> pwall
    showWalls [Left] = pwall <> space
    showWalls [Up, Left] = pwall <> pwall

showBlockBottom :: MazeState -> (Location, [Location]) -> Text
showBlockBottom ms b@(loc, _) = showWalls (walls b)
  where
    showWalls [] = space <> walkCell ms loc
    showWalls [Up] = space <> walkCell ms loc
    showWalls [Left] = pwall <> walkCell ms loc
    showWalls [Up, Left] = pwall <> walkCell ms loc

walkCell :: MazeState -> Location -> Text
walkCell ms loc
  | loc == player ms = playerSymbol
  | showPath ms && loc `member` shortestPath ms = pathSymbol
  | otherwise = space
