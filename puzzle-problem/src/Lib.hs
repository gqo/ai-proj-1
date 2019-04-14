module Lib where

import Data.List (lines)
import Data.List.Split (splitOn, chunksOf)

import Board
import Heuristics
import AStar

-- readBoards takes the data received from reading a test file and cleans it 
-- into manageable data (a tuple of two Int arrays)
readBoards :: String -> (Board, Board)
readBoards content =
    -- construct a tuple from the generated array
    (flattenedBoards !! 0, flattenedBoards !! 1) :: (Board, Board)
    where
        -- split data into cleaned (no \r\n) lines
        fLines = map (filter (\x -> ((/=) '\r' x) && ((/=) '\n' x))) $ lines content
        -- remove new line and split integer characters
        splitFLines = map (splitOn " ") $ filter (/= "") fLines
        -- convert strings to ints
        cleanedContent = map (map (\x -> read x :: Int)) splitFLines
        -- separate the big array of data into two boards
        twoBoards = chunksOf 3 cleanedContent
        -- flatten from two 2d array to two 1d arrays
        flattenedBoards = map concat twoBoards

-- to2d takes a puzzle board and chunks it into a 2d list with elements of 
-- length boardWidth for easier printing
to2d :: Board -> [[Int]]
to2d = chunksOf boardWidth

-- stringBoard "stringifies" a 2d-ified puzzle board
stringBoard :: [[Int]] -> String
stringBoard [] = ""
stringBoard (x:xs) = (show x) ++ "\n" ++ stringBoard xs

stringList :: Show a => [a] -> String
stringList [] = ""
stringList (x:xs) =
    (show x) ++ " " ++ stringList xs


string2dBoard :: [[Int]] -> String
string2dBoard [] = ""
string2dBoard (x:xs) =
    stringList x ++ "\n" ++ string2dBoard xs
        
-- ppBoard prints the a puzzle board in a pretty fashion
ppBoard :: Board -> String
ppBoard = string2dBoard . to2d

printSolution :: SearchState -> String
printSolution (SearchState startBoard goalBoard _ solutionDepth nodesGenerated _ _ solutionMoves solutionVals)
    = ppBoard startBoard ++ "\n" ++
      ppBoard goalBoard ++ "\n" ++
      show solutionDepth ++ "\n" ++
      show (fromIntegral (length nodesGenerated)) ++ "\n" ++
      stringList solutionMoves ++ "\n" ++
      stringList solutionVals ++ "\n"


getSolution :: String -> Board -> Board -> String
getSolution hnChoice startBoard goalBoard =
    case hnChoice of
        "0" ->
            printSolution solution
            where
                solution = runAStar $ SearchState startBoard goalBoard manhattanHeuristic (-1) [] [] 0 [] []
        "1" ->
            printSolution solution
            where
                solution = runAStar $ SearchState startBoard goalBoard manhattanLinConflictHeuristic (-1) [] [] 0 [] []