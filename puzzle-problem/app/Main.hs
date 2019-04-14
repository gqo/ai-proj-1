module Main where

import System.Environment (getArgs)
import qualified Data.Map as Map

import Lib
import Heuristics
import AStar

main :: IO ()
main = do
    -- Get filepath from command line args
    [inputFilePath] <- getArgs
    -- Read the file into a string
    content <- readFile (inputFilePath)
    -- Process that string into two workable arrays
    let (startBoard, goalBoard) = readBoards content
    -- Print the initial positions of those arrays
    putStrLn "Starting board position: "
    ppBoard startBoard
    putStrLn ""
    putStrLn "Goal board position: "
    ppBoard goalBoard
    putStrLn ""
    let startState = SearchState startBoard goalBoard manhattanHeuristic (-1) [] [] 0 [] []
        solution = runAStar startState
    putStrLn $ show solution