module Heuristics where

import Board
import Index
import Manhattan
import LinearConflict
    
-- heuristic 1: only the manhattan distance
manhattanHeuristic :: Board -> Board -> Maybe Int
manhattanHeuristic current goal =
    manhattanSum current goal

-- heuristic 2: the sum of manhattan distance + 2x * linear conflicts
manhattanLinConflictHeuristic :: Board -> Board -> Maybe Int
manhattanLinConflictHeuristic current goal =
    sumMaybeInt $ manhattanResult:linConflictResult:[]
    where
        manhattanResult = manhattanSum current goal
        linConflictResult = linConflictSum current goal