module Manhattan where

import Data.List (elemIndex)
import Data.Functor (fmap)
import Control.Monad (sequence)

import Index
import Board

-- Get the distance between two x,y coords
coordDistance :: (Int, Int) -> (Int, Int) -> Int
coordDistance (x1,y1) (x2,y2) =
    xDistance + yDistance
    where
        xDistance = abs $ x1 - x2
        yDistance = abs $ y1 - y2

-- Get the manhattan distance given an element on the board
manhattanDistance :: Board -> Board -> Int -> Maybe Int
manhattanDistance current goal element =
    case findStartGoalIndexes current goal element of
        Just (currentIndex, goalIndex) ->
            (Just distance)
            where
                distance = coordDistance (fromIndex currentIndex) (fromIndex goalIndex)
        Nothing -> Nothing

-- Get the sum of all calculated manhattan distances on the board
manhattanSum :: Board -> Board -> Maybe Int
manhattanSum current goal =
    sumMaybeInt distanceList
    where
        distanceList = map (manhattanDistance current goal) [1..8]

testManhattanSum :: Board -> Board -> Int
testManhattanSum current goal =
    case manhattanSum current goal of
        Just sum -> sum
        Nothing -> -1