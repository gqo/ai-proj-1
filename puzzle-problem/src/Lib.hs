module Lib where

import Data.List (lines)
import Data.List.Split (splitOn, chunksOf)

import Board

-- readBoards takes the data received from reading a test file and cleans it 
-- into manageable data (a tuple of two Int arrays)
readBoards :: String -> (Board, Board)
readBoards content =
    -- construct a tuple from the generated array
    (flattenedBoards !! 0, flattenedBoards !! 1) :: (Board, Board)
    where
        -- split data into lines
        fLines = lines content
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

-- ppBoard prints the a puzzle board in a pretty fashion
ppBoard :: Board -> IO ()
ppBoard = putStr . stringBoard . to2d