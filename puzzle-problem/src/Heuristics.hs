module Heuristics where

import Board
import Index
import Manhattan
import LinearConflict
    
manhattanHeuristic :: Board -> Board -> Maybe Int
manhattanHeuristic current goal =
    manhattanSum current goal

manhattanLinConflictHeuristic :: Board -> Board -> Maybe Int
manhattanLinConflictHeuristic current goal =
    sumMaybeInt $ manhattanResult:linConflictResult:[]
    where
        manhattanResult = manhattanSum current goal
        linConflictResult = linConflictSum current goal