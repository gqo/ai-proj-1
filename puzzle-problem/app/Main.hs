module Main where

import System.Environment (getArgs)

import Lib

main :: IO ()
main = do
    -- Get filepath and heuristic choice from command line args
    args <- getArgs
    case args of
        [] -> putStrLn "Missing filepath and heuristic choice"
        [inputFilePath] -> putStrLn "Missing heuristic choice - input 0 for manhattan distance heuristic or 1 for manhattan & 2x linear conflict heuristic"
        [inputFilePath, hnChoice] -> do
            -- Read the file into a string
            content <- readFile (inputFilePath)
            -- Process that string into two workable arrays
            let (startBoard, goalBoard) = readBoards content
                solution = getSolution hnChoice startBoard goalBoard

            -- Write the solution to an outfile
            writeFile "./output/output.txt" solution